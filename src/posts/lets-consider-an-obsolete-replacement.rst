((title . "Let's consider an obsolete replacement")
 (date . "2016-06-08 10:52:35 +0200"))

I'm currently writing `my second mode`_, this time for textual
markup.  As I still don't have much experience with it, I did look at
other modes of that kind, ultimately ending up with ``rst.el``.

It's not unusual for older code to redefine things that could possibly
not supported by all Emacs versions out there.  What I did not expect
however, was `an implementation of symbolic regular expressions`_:

.. code:: elisp

    (defvar rst-re-alist) ; Forward declare to use it in `rst-re'.

    ;; FIXME: Use `sregex' or `rx' instead of re-inventing the wheel.
    (rst-testcover-add-compose 'rst-re)
    ;; testcover: ok.
    (defun rst-re (&rest args)
      "Interpret ARGS as regular expressions and return a regex string.
    Each element of ARGS may be one of the following:

    A string which is inserted unchanged.

    A character which is resolved to a quoted regex.

    A symbol which is resolved to a string using `rst-re-alist-def'.

    A list with a keyword in the car.  Each element of the cdr of such
    a list is recursively interpreted as ARGS.  The results of this
    interpretation are concatenated according to the keyword.

    For the keyword `:seq' the results are simply concatenated.

    For the keyword `:shy' the results are concatenated and
    surrounded by a shy-group (\"\\(?:...\\)\").

    For the keyword `:alt' the results form an alternative (\"\\|\")
    which is shy-grouped (\"\\(?:...\\)\").

    For the keyword `:grp' the results are concatenated and form a
    referenceable group (\"\\(...\\)\").

    After interpretation of ARGS the results are concatenated as for
    `:seq'."
      (apply 'concat
             (mapcar
              (lambda (re)
                (cond
                 ((stringp re)
                  re)
                 ((symbolp re)
                  (cadr (assoc re rst-re-alist)))
                 ((characterp re)
                  (regexp-quote (char-to-string re)))
                 ((listp re)
                  (let ((nested
                         (mapcar (lambda (elt)
                                   (rst-re elt))
                                 (cdr re))))
                    (cond
                     ((eq (car re) :seq)
                      (mapconcat 'identity nested ""))
                     ((eq (car re) :shy)
                      (concat "\\(?:" (mapconcat 'identity nested "") "\\)"))
                     ((eq (car re) :grp)
                      (concat "\\(" (mapconcat 'identity nested "") "\\)"))
                     ((eq (car re) :alt)
                      (concat "\\(?:" (mapconcat 'identity nested "\\|") "\\)"))
                     (t
                      (error "Unknown list car: %s" (car re))))))
                 (t
                  (error "Unknown object type for building regex: %s" re))))
              args)))

    ;; FIXME: Remove circular dependency between `rst-re' and `rst-re-alist'.
    (with-no-warnings ; Silence byte-compiler about this construction.
      (defconst rst-re-alist
        ;; Shadow global value we are just defining so we can construct it step by
        ;; step.
        (let (rst-re-alist)
          (dolist (re rst-re-alist-def rst-re-alist)
            (setq rst-re-alist
                  (nconc rst-re-alist
                         (list (list (car re) (apply 'rst-re (cdr re))))))))
        "Alist mapping symbols from `rst-re-alist-def' to regex strings."))

I find it hilarious that they appear to be aware of a now obsolete
alternative and a more powerful, officially supported one, yet decided
to do their own thang.  At least there's not much code around that
could be yucky, if you ignore that one circular dependency mentioned
at the bottom between the function and its look-up alist.

.. _my second mode: https://github.com/wasamasa/svnwiki-mode
.. _an implementation of symbolic regular expressions: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/textmodes/rst.el?id=bc93643957a83262dd9cc7c4256f356d5f7b66f4#n530
