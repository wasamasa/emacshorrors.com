((title . "The Vagabond")
 (date . "2015-06-29 00:55:28 +0200"))

Much has been said about TRAMP_, I've personally have tried it out a
few times, then `disabled it for good`_ as I hated it regularly locking
up Emacs.  While going through bytecomp.el_ again_, I've found the
following gem:

.. code:: elisp

    ;; If things not being bound at all is ok, so must them being
    ;; obsolete.  Note that we add to the existing lists since Tramp
    ;; (ab)uses this feature.
    (let ((byte-compile-not-obsolete-vars
           (append byte-compile-not-obsolete-vars bound-list))
          (byte-compile-not-obsolete-funcs
           (append byte-compile-not-obsolete-funcs fbound-list)))
      ,@body)

The referenced part in tramp-compat.el_:

.. code:: elisp

    ;; `directory-sep-char' is an obsolete variable in Emacs.  But it is
    ;; used in XEmacs, so we set it here and there.  The following is
    ;; needed to pacify Emacs byte-compiler.
    ;; Note that it was removed altogether in Emacs 24.1.
    (when (boundp 'directory-sep-char)
      (defvar byte-compile-not-obsolete-var nil)
      (setq byte-compile-not-obsolete-var 'directory-sep-char)
      ;; Emacs 23.2.
      (defvar byte-compile-not-obsolete-vars nil)
      (setq byte-compile-not-obsolete-vars '(directory-sep-char)))

Altering the way the byte compiler works due to incorrect usage of
variable obsoletion for `a mostly dead Emacs fork`_ makes me a sad
panda.

.. _TRAMP: https://www.gnu.org/software/tramp/
.. _disabled it for good: https://github.com/wasamasa/dotemacs/commit/8f0a5c95ef283fe2c27915616bf29305f0c07db
.. _bytecomp.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/emacs-lisp/bytecomp.el?id=f729a7c09eca37bc695db20b35e4dbeaa8813e48#n3843
.. _again: http://emacshorrors.com/post/gross
.. _tramp-compat.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/net/tramp-compat.el?id=f729a7c09eca37bc695db20b35e4dbeaa8813e48#n91
.. _a mostly dead Emacs fork: http://www.xemacs.org/
