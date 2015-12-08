((title . "Odd Duck Hunt")
 (date . "2015-12-08 10:36:49 +0100"))

Thanks to Emacs being the best IDE for hacking Emacs, there's a number
of useful tools available, including a traditional debugger
(``debug.el``), a source-level debugger (``edebug.el``), tracing
(``trace.el``), coverage testing (``testcover.el``), a testing
framework (``ert.el``), an instrumenting profiler (``elp.el``) and a
statistical profiler (``profiler.el``).  There's a few more like
``benchmark.el``, ``checkdoc.el``, ``disass.el``, ``elint.el`` and
``warnings.el``, but these seem to be lesser known.  In a perfect
world you'd make extensive use of these tools and never run into bugs,
but alas, that's not how it works.  I'll focus on ``profiler.el`` here
because that's one of the few I'm using on a regular basis, the other
one being ``edebug.el`` for stepping through complicated functions and
macros.

The profiler workflow is simple enough, you start the profiler,
perform execution of code that exposes less than ideal behaviour, stop
the profiler and take a look at the profiler report in hope of
catching a bottleneck worth optimizing.  This can be reproduced with
``M-x profiler-start``, ``M-x profiler-stop`` and ``M-x
profiler-report``.  Except that if you do this, nothing happens.  No
newly displayed buffer or error or anything.  If you try variating the
steps, you'll eventually find out that the correct sequence is ``M-x
profiler-start`` and ``M-x profiler-report``.  Which is weird, but
seems to work if you're disciplined enough to report right away and
don't report a second time because the second report will be started
from anew.  I suspect the few people using the profiler did just learn
to deal with this and never questioned the behaviour.

This has been bugging me enough to attempt writing a patch.  The first
step to this was studying `the code`_ in all of its entirety.  Most of it
concerns itself with rendering the report and allowing for interaction
with it.  My first hunch was inspecting ``profiler-stop``:

.. code:: elisp

    (defun profiler-stop ()
      "Stop started profilers.  Profiler logs will be kept."
      (interactive)
      (let ((cpu (if (fboundp 'profiler-cpu-stop) (profiler-cpu-stop)))
            (mem (profiler-memory-stop)))
        (message "%s profiler stopped"
                 (cond ((and mem cpu) "CPU and memory")
                       (mem "Memory")
                       (cpu "CPU")
                       (t "No")))))

This is surely weird.  Why does the docstring state profiler logs
persist after stopping when the report won't make use of this?
``profiler-reset`` is even weirder:

.. code:: elisp

    (defun profiler-reset ()
      "Reset profiler logs."
      (interactive)
      (when (fboundp 'profiler-cpu-log)
        (ignore (profiler-cpu-log)))
      (ignore (profiler-memory-log))
      t)

Apparently it's sufficient to reset the profiler logs by... attempting
to access them?  What the hell.  What about the report functions?

.. code:: elisp

    (defun profiler-report-cpu ()
      (let ((profile (profiler-cpu-profile)))
        (when profile
          (profiler-report-profile-other-window profile))))

    (defun profiler-report-memory ()
      (let ((profile (profiler-memory-profile)))
        (when profile
          (profiler-report-profile-other-window profile))))

OK, this sure looks as if both ``profiler-cpu-profile`` and
``profiler-memory-profile`` could return ``nil`` if they wish to.
Surely there must be a condition when this happens...

.. code:: elisp

    (defun profiler-cpu-profile ()
      "Return CPU profile."
      (when (profiler-running-p 'cpu)
        (profiler-make-profile
         :type 'cpu
         :timestamp (current-time)
         :log (profiler-cpu-log))))

    (defun profiler-memory-profile ()
      "Return memory profile."
      (when (profiler-memory-running-p)
        (profiler-make-profile
         :type 'memory
         :timestamp (current-time)
         :log (profiler-memory-log))))

Finally some sort of explanation.  ``profiler.el`` only produces a
profile if the profiler is still running.  Patching out the check
makes it return a profile the first time, but errors out on the log
functions on subsequent attempts.  `To the C`_!

.. code:: c

    DEFUN ("profiler-cpu-log", Fprofiler_cpu_log, Sprofiler_cpu_log,
           0, 0, 0,
           doc: /* Return the current cpu profiler log.
    The log is a hash-table mapping backtraces to counters which represent
    the amount of time spent at those points.  Every backtrace is a vector
    of functions, where the last few elements may be nil.
    Before returning, a new log is allocated for future samples.  */)
      (void)
    {
      Lisp_Object result = cpu_log;
      /* Here we're making the log visible to Elisp, so it's not safe any
         more for our use afterwards since we can't rely on its special
         pre-allocated keys anymore.  So we have to allocate a new one.  */
      cpu_log = (profiler_cpu_running
                 ? make_log (profiler_log_size, profiler_max_stack_depth)
                 : Qnil);
      Fputhash (Fmake_vector (make_number (1), Qautomatic_gc),
                make_number (cpu_gc_count),
                result);
      cpu_gc_count = 0;
      return result;
    }

    DEFUN ("profiler-memory-log",
           Fprofiler_memory_log, Sprofiler_memory_log,
           0, 0, 0,
           doc: /* Return the current memory profiler log.
    The log is a hash-table mapping backtraces to counters which represent
    the amount of memory allocated at those points.  Every backtrace is a vector
    of functions, where the last few elements may be nil.
    Before returning, a new log is allocated for future samples.  */)
      (void)
    {
      Lisp_Object result = memory_log;
      /* Here we're making the log visible to Elisp , so it's not safe any
         more for our use afterwards since we can't rely on its special
         pre-allocated keys anymore.  So we have to allocate a new one.  */
      memory_log = (profiler_memory_running
                    ? make_log (profiler_log_size, profiler_max_stack_depth)
                    : Qnil);
      return result;
    }

This explains the other part of the puzzle.  Once the profiler log has
been made accessible, it is reset and subsequent attempts at exposing
it with the profiler not running will try putting new elements into
an empty log.  Which will error out.  However if the profiler is still
running, the show will go on and a new log will be returned every time
it is requested.  I'll assume that's why the author went this route,
despite it not being right.  Speaking of which, I'm pretty sure I've
seen his name elsewhere_...

As for the fix, I've solved this by introducing two variables caching
the last CPU and memory log.  A reset clears them, both stopping and
reporting update them with new logs.  The report function simply checks
whether anything is present in them and displays the appropriate
reports.  No more check whether the profiler is still running either.
The pending bug report is at debbugs_, in case it is rejected you can
still apply the patch from `my patch repository`_.

.. _the code: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/profiler.el?id=6148555ee5a3d0139ae517803718b3e0357933c7
.. _To the C: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/profiler.c?id=6148555ee5a3d0139ae517803718b3e0357933c7
.. _elsewhere: https://github.com/auto-complete/auto-complete/blob/master/auto-complete.el
.. _debbugs: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=22114
.. _my patch repository: https://github.com/wasamasa/emacs-patches/blob/master/profiler-stop.patch
