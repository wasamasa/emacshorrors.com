((title . "Honesty is the Best Policy")
 (date . "2016-10-28 10:12:10 +0200"))

.. code:: elisp

    ;;; Please do not try to understand this code unless you have a VERY
    ;;; good reason to do so.  I gave up trying to figure it out well
    ;;; enough to explain it, long ago.

This_ precedes ``paredit-forward-sexps-to-kill`` which appears to be
code that deals with the problem that commands operating on
S-expressions may fail mysteriously if there's no trailing newline
afterwards.

Credits go to contrapunctus_.

.. _This: http://mumble.net/~campbell/emacs/paredit.el
.. _contrapunctus: https://github.com/contrapunctus-1
