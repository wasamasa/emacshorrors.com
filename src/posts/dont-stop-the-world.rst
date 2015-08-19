((title . "Don't Stop the World")
 (date . "2015-03-29 00:31:31"))

Emacs being essentially an oversized, graphical REPL_ [1]_, is one of
the reasons why it comes with `Garbage Collection`_ (which I will
abbreviate from now on) as feature.  This essentially means that
you're able to inspect its state, customize its tunables to change
Emacs performance and can even force an explicit GC to happen.  The
specific type of GC Emacs employs is a simple `Mark and Sweep`_
method; due to its weakness of introducing periodical freezes, it is
also known as "Stop the World" GC.  The main tunable responsible for
the frequency and length of pauses would be the ``gc-cons-threshold``
variable which is set by default to a measly 800 thousand bytes.

A lesser known feature of Emacs would be dynamic scoping.  To put it
short, it allows a variable to be accessible from its subroutines and
therefore allows one to override the value of an existing variable for
the lifetime of a subroutine.  It is possible to enable lexical
scoping for saner scoping rules on a per-file basis, by doing so the
language behaves much more like `Common Lisp`_ where top-level
definitions are dynamically bound and everything else is lexically
bound.

So, what could GC and dynamic scoping have to do with the horrors of
Emacs?  Well, there is Emacs Lisp code that binds the
``gc-cons-threshold`` variable to a much higher value than usual.

.. code:: elisp

    (defvar calc-aborted-prefix nil)
    (defvar calc-start-time nil)
    (defvar calc-command-flags nil)
    (defvar calc-final-point-line)
    (defvar calc-final-point-column)
    ;;; Note that modifications to this function may break calc-pass-errors.
    (defun calc-do (do-body &optional do-slow)
      (calc-check-defines)
      (let* ((calc-command-flags nil)
             (calc-start-time (and calc-timing (not calc-start-time)
                                   (require 'calc-ext)
                                   (current-time-string)))
             (gc-cons-threshold (max gc-cons-threshold
                                     (if calc-timing 2000000 100000)))
             calc-final-point-line calc-final-point-column)
        (setq calc-aborted-prefix "")
        (unwind-protect
            (condition-case err
                (save-excursion
                  (if calc-embedded-info
                      (calc-embedded-select-buffer)
                    (calc-select-buffer))
                  (and (eq calc-algebraic-mode 'total)
                       (require 'calc-ext)
                       (use-local-map calc-alg-map))
                  (when (and do-slow calc-display-working-message)
                    (message "Working...")
                    (calc-set-command-flag 'clear-message))
                  (funcall do-body)
                  (setq calc-aborted-prefix nil)
                  (when (memq 'renum-stack calc-command-flags)
                    (calc-renumber-stack))
                  (when (memq 'clear-message calc-command-flags)
                    (message "")))
              (error
               (if (and (eq (car err) 'error)
                        (stringp (nth 1 err))
                        (string-match "max-specpdl-size\\|max-lisp-eval-depth"
                                      (nth 1 err)))
                   (error "Computation got stuck or ran too long.  Type `M' to increase the limit")
                 (setq calc-aborted-prefix nil)
                 (signal (car err) (cdr err)))))
          (when calc-aborted-prefix
            (calc-record "<Aborted>" calc-aborted-prefix))
          (and calc-start-time
               (let* ((calc-internal-prec 12)
                      (calc-date-format nil)
                      (end-time (current-time-string))
                      (time (if (equal calc-start-time end-time)
                                0
                              (math-sub
                               (calcFunc-unixtime (math-parse-date end-time) 0)
                               (calcFunc-unixtime (math-parse-date calc-start-time)
                                                  0)))))
                 (if (math-lessp 1 time)
                     (calc-record time "(t)"))))
          (or (memq 'no-align calc-command-flags)
              (derived-mode-p 'calc-trail-mode)
              (calc-align-stack-window))
          (and (memq 'position-point calc-command-flags)
               (if (derived-mode-p 'calc-mode)
                   (progn
                     (goto-char (point-min))
                     (forward-line (1- calc-final-point-line))
                     (move-to-column calc-final-point-column))
                 (save-current-buffer
                   (calc-select-buffer)
                   (goto-char (point-min))
                   (forward-line (1- calc-final-point-line))
                   (move-to-column calc-final-point-column))))
          (unless (memq 'keep-flags calc-command-flags)
            (save-excursion
              (calc-select-buffer)
              (setq calc-inverse-flag nil
                    calc-hyperbolic-flag nil
                    calc-option-flag nil
                    calc-keep-args-flag nil)))
          (when (memq 'do-edit calc-command-flags)
            (switch-to-buffer (get-buffer-create "*Calc Edit*")))
          (calc-set-mode-line)
          (when calc-embedded-info
            (calc-embedded-finish-command))))
      (identity nil))  ; allow a GC after timing is done

`This one`_ is from calc_, a package allowing one to do advanced
calculation in Emacs.  Apparently the function is an entry point and
not only raises the GC limit, but also tries dealing with other
critical errors by doing a regular expression match on error messages
and measuring run times, then suggesting an increase of these other
limits.

If you're asking yourself why one would possibly want to increase the
GC limit temporarily, `another place of Emacs`_ using that trick should
make it clearer.

.. code:: elisp

    ;; Read Lisp objects.  Temporarily increase `gc-cons-threshold' to
    ;; prevent a GC that would not free any memory.

So that's why.  Pauses are only perceived as OK if they free memory.
Reminds me a bit of `the story`_ about `Erik Naggum`_ deactivating the
garbage collection messages Emacs displayed back then to see whether
it would end the complaints of it being slow coming from a group of
Emacs users.  Surprisingly enough, it did.

I do wonder whether this incident made the Emacs core developers
change that default.  If you wish to feel like you're in the nineties,
just set ``garbage-collection-messages`` to ``t``.

.. [1] Emacs also comes with its own textual REPL_, IELM_. It is
       tremendously useful for writing Emacs Lisp code in a more
       traditional style.

.. _REPL: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
.. _IELM: https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#index-ielm
.. _Garbage Collection: https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)
.. _Mark and Sweep: https://en.wikipedia.org/wiki/Tracing_garbage_collection#Na.C3.AFve_mark-and-sweep
.. _Common Lisp: https://common-lisp.net/
.. _This one: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/calc/calc.el?id=e6127d94746e230f95bdf2ad002e4379474e5a8b#n1585
.. _calc: https://www.gnu.org/software/emacs/manual/html_mono/calc.html
.. _another place of Emacs: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/ebrowse.el#n865
.. _the story: http://www.ravenbrook.com/project/mps/master/manual/html/mmref/lang.html
.. _Erik Naggum: https://en.wikipedia.org/wiki/Erik_Naggum
