((title . "IELM used SUBSTITUTE!")
 (date . "2015-08-07 10:25:41 +0200"))

IELM_ is a REPL_ for Emacs Lisp.  It is built upon comint_, a major
mode for interacting with subprocesses that accept input and return
output.  But wait, how can this possibly work if it's Emacs ``comint``
is speaking to?  After all, it doesn't behave like, say, ``bash`` and
convincing an Emacs process to behave this way in a proper manner is
sort of difficult [1]_.

Such petty things do of course not deter Real Emacs Lisp Hackers™.
Instead of fixing ``comint`` to be more flexible about its mode of
operation, well, `just see for yourself`_:

.. code:: elisp

    ;; A dummy process to keep comint happy. It will never get any input
    (unless (comint-check-proc (current-buffer))
      ;; Was cat, but on non-Unix platforms that might not exist, so
      ;; use hexl instead, which is part of the Emacs distribution.
      (condition-case nil
          (start-process "ielm" (current-buffer) "hexl")
        (file-error (start-process "ielm" (current-buffer) "cat")))
      ...)

.. [1] Worst REPL ever: ``emacs --batch --eval '(while (princ (format
       "%s\n" (eval (read-minibuffer "EMACS> ")))))'``.  Courtesy to
       ``#emacs`` for the initial version!

.. _IELM: https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#index-ielm
.. _REPL: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
.. _comint: https://www.masteringemacs.org/article/comint-writing-command-interpreter
.. _just see for yourself: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/ielm.el?id=0aec2aaccd8b745fa7214f3edd453c04a04bfba4#n570
