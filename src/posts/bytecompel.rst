((title . "bytecomp.el")
 (date . "2017-10-31 08:44:03 +0100"))

It's halloween, so here's a real treat for you, the commentary in
bytecomp.el_!  The author of that piece of code is Jamie Zawinski who
did invaluable work for both GNU Emacs and XEmacs, these days he runs
a night club and blogs_.  Here are my favorite parts of the file:

- .. code:: elisp

     ";; We successfully didn't compile this file."

- .. code:: elisp

     (insert "\n") ; aaah, unix.

- .. code:: elisp

                (when old-style-backquotes
                  (byte-compile-warn "!! The file uses old-style backquotes !!
    This functionality has been obsolete for more than 10 years already
    and will be removed soon.  See (elisp)Backquote in the manual."))

- .. code:: elisp

    ;; Insert semicolons as ballast, so that byte-compile-fix-header
    ;; can delete them so as to keep the buffer positions
    ;; constant for the actual compiled code.
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n"

- .. code:: elisp

    ;; To avoid consing up monstrously large forms at load time, we split
    ;; the output regularly.

- .. code:: elisp

    ;; If things not being bound at all is ok, so must them being
    ;; obsolete.  Note that we add to the existing lists since Tramp
    ;; (ab)uses this feature.

- .. code:: elisp

    ;; If foo.el declares `toto' as obsolete, it is likely that foo.el will
    ;; actually use `toto' in order for this obsolete variable to still work
    ;; correctly, so paradoxically, while byte-compiling foo.el, the presence
    ;; of a make-obsolete-variable call for `toto' is an indication that `toto'
    ;; should not trigger obsolete-warnings in foo.el.

- .. code:: elisp

     ;; FIXME: we also use this hunk-handler to implement the function's dynamic
     ;; docstring feature.  We could actually implement it more elegantly in
     ;; byte-compile-lambda so it applies to all lambdas, but the problem is that
     ;; the resulting .elc format will not be recognized by make-docfile, so
     ;; either we stop using DOC for the docstrings of preloaded elc files (at the
     ;; cost of around 24KB on 32bit hosts, double on 64bit hosts) or we need to
     ;; build DOC in a more clever way (e.g. handle anonymous elements).

- .. code:: elisp

     ;; Don't reload the source version of the files below
     ;; because that causes subsequent byte-compilation to
     ;; be a lot slower and need a higher max-lisp-eval-depth,
     ;; so it can cause recompilation to fail.

- .. code:: elisp

    ;; To avoid "lisp nesting exceeds max-lisp-eval-depth" when bytecomp compiles
    ;; itself, compile some of its most used recursive functions (at load time).

Don't get me wrong, I'm aware that these are all necessary and don't
indicate deeper faults in the source code.  I merely find it
interesting what hacks one has to come up with for byte-code
compilation and found studying the file enlightening to say the least.

.. _bytecomp.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/emacs-lisp/bytecomp.el
.. _blogs: https://www.jwz.org/blog/
