((title . "longlines-mode")
 (date . "2015-04-19 19:51:54 +0200"))

Sometimes there are files that are too wide to be displayed
comfortably.  One option to deal with them is hard-wrapping or in
other words, inserting physical newlines in the file with ``M-q``.
The other option is displaying the lines by wrapping them visually and
inserting visual newlines in the window, this is what Emacs is doing
by default, in a graphical instance it even displays visual indicators
in the fringe at the wrapping points.

There is one problem with this mechanism though.  First of all,
soft-wrapping uses the window border as its ruler for word-breaking.
This is kind of annoying with today's wide screens as they easily go
beyond the ancient standard of `80 columns`_.  The other problem is
that words are broken up which doesn't look as good as breaking after
whitespace.

Emacs used to offer a solution to both issues, longlines-mode_.  As of
24.4 it got `marked as obsolete`_ though, with the remark that it is
replaced by visual-line-mode_.  Quick experimentation shows that while
it avoids breaking words, it does not obey ``fill-column`` and will
therefore be no good on wide windows.

`A reddit discussion`_ made me investigate into what exactly the
difference between both minor modes was, simply because I wanted to
know why they behaved differently, what the reason for the deprecation
of the former was and how the regression of the latter could be
fixed.  I'll let the code speak for itself this time, you can look at
both longlines.el_ and simple.el_.

First of all, ``longlines.el`` has 400 SLOC, the code in ``simple.el``
making up ``visual-line-mode`` amounts to 80 SLOC.  The reduction in
code size by a factor of five can be explained easily by looking its
method of operation: ``visual-line-mode`` modifies a few tunables of
the display engine and lets it do the heavy work.  Apparently there is
no tunable for the equivalent of ``fill-column`` yet which explains
the apparent limitation.

``longlines-mode`` however does something entirely different.  It
actually goes ahead and inserts specially marked up newlines at the
breakpoints as determined by scanning forward and backward with Emacs
Lisp code, then employs several hacks to convince Emacs that the text
didn't change and that it should neither include these extra newlines
into copied text [1]_ nor should save them to disk.  Additionally to
that, a number of mode-specific hacks (including changing the default
search function) is employed and the text rewrapped manually after
each command and window size change.  To me it's pretty clear why this
hackfest got obsoleted.

Fortunately there's good news as well.  It looks like someone already
wrote a package to address the "deficiency" of using
``visual-line-mode`` as replacement for ``longlines-mode``,
visual-fill-column_.  So, as long as you weren't relying on the
implementation details of ``longlines-mode`` to work around another
ugly bug [2]_, using the following in your init file after installing
the package should do the trick:

.. code:: elisp

   (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)

.. [1] Emacs comes with support for filtering buffer contents before
       passing them on commands, see the `Buffer Contents`_ section of
       the Emacs Lisp manual.
.. [2] There is a long-standing display engine problem that makes the
       redisplay excruciatingly slow for files with long lines.  A
       commonly cited workaround is hard-wrapping these lines if
       possible, considering that ``longlines-mode`` is doing exactly
       that (and its extra hacks on top of it to make it look
       otherwise), it doesn't surprise me that using it over
       ``visual-line-mode`` is `a possible remedy`_ for that bug.

.. _80 columns: http://programmers.stackexchange.com/a/148678
.. _longlines-mode: http://www.emacswiki.org/emacs/LongLines
.. _marked as obsolete: http://git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS.24?id=65ac8bc6a9e256b60c8ddfa3c99a1b28145a0763#n944
.. _visual-line-mode: https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Visual-Line-Mode
.. _A reddit discussion: http://www.reddit.com/r/emacs/comments/32z242/visuallinemode_and_the_former_longlinesmode_not/
.. _longlines.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/obsolete/longlines.el?id=65ac8bc6a9e256b60c8ddfa3c99a1b28145a0763
.. _simple.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/simple.el?id=65ac8bc6a9e256b60c8ddfa3c99a1b28145a0763#n6169
.. _visual-fill-column: https://github.com/joostkremers/visual-fill-column
.. _Buffer Contents: http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Buffer-Contents
.. _a possible remedy: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=18530#13

