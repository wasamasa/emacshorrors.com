((title . "Function Signature From Hell")
 (date . "2016-02-22 15:53:58 +0100"))

I've finally given in to the sweet temptation of record-style types,
despite `being wary of them`_ at first.  To have an idea how the
constructor syntax looks in practice, I grepped the Emacs sources for
examples and located ``package-buffer-info``.  If you have eldoc_
enabled, put point inside the ``package-desc-from-define`` form and
wait for it to kick in, you'll see the following:

.. code::

    package-desc-from-define: (NAME-STRING VERSION-STRING &optional
    SUMMARY REQUIREMENTS &rest REST-PLIST &AUX (NAME (INTERN
    NAME-STRING)) (VERSION (VERSION-TO-LIST VERSION-STRING)) (REQS
    (MAPCAR #'(LAMBDA (ELT) (LIST (CAR ELT) (VERSION-TO-LIST (CADR
    ELT)))) (IF (EQ 'QUOTE (CAR REQUIREMENTS)) (NTH 1 REQUIREMENTS)
    REQUIREMENTS))) (KIND (PLIST-GET REST-PLIST :KIND)) (ARCHIVE
    (PLIST-GET REST-PLIST :ARCHIVE)) (EXTRAS (LET (ALIST) (WHILE
    REST-PLIST (UNLESS (MEMQ (CAR REST-PLIST) '(:KIND :ARCHIVE)) (LET
    ((VALUE (CADR REST-PLIST))) (WHEN VALUE (PUSH (CONS (CAR
    REST-PLIST) (IF (EQ (CAR-SAFE VALUE) 'QUOTE) (CADR VALUE) VALUE))
    ALIST)))) (SETQ REST-PLIST (CDDR REST-PLIST))) ALIST)))

This prompted me to customize eldoc to never make it happen again:

.. code:: elisp

    (setq eldoc-echo-area-use-multiline-p nil)

.. _being wary of them: http://emacshorrors.com/posts/dont-bother.html
.. _eldoc: https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Doc.html
