((title . "Don't bother")
 (date . "2015-07-22 15:37:42 +0200"))

All I planned to do was finding a source for the somewhat often
mentioned "Emacs Lisp doesn't really have structs, it's just vectors
in disguise!":

.. code:: elisp

    ;; Don't bother adding to cl-custom-print-functions since it's not used
    ;; by anything anyway!
    ;;(if print-func
    ;;    (push `(if (boundp 'cl-custom-print-functions)
    ;;               (push
    ;;                ;; The auto-generated function does not pay attention to
    ;;                ;; the depth argument cl-n.
    ;;                (lambda (cl-x cl-s ,(if print-auto '_cl-n 'cl-n))
    ;;                  (and ,pred-form ,print-func))
    ;;                cl-custom-print-functions))
    ;;          forms))

As I wasn't sure what `this commented out code`_ in the madness that
the ``cl-defstruct`` macro is meant, I looked `a bit further`_:

.. code:: elisp

    ;;;###autoload
    (define-obsolete-variable-alias
      ;; This alias is needed for compatibility with .elc files that use defstruct
      ;; and were compiled with Emacs<24.3.
      'custom-print-functions 'cl-custom-print-functions "24.3")

    ;;;###autoload
    (defvar cl-custom-print-functions nil
      "This is a list of functions that format user objects for printing.
    Each function is called in turn with three arguments: the object, the
    stream, and the print level (currently ignored).  If it is able to
    print the object it returns true; otherwise it returns nil and the
    printer proceeds to the next function on the list.

    This variable is not used at present, but it is defined in hopes that
    a future Emacs interpreter will be able to use it.")

So, knowing that defining custom print syntax for useful things like
reading back in structs again is a hopeless endeavor, does the first
claim hold true?

.. code:: elisp

    (cl-defstruct love) ;=> love
    (make-love) ;=> [cl-struct-love]

(╯°O°）╯︵ ┻━┻

.. _this commented out code: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/emacs-lisp/cl-macs.el?id=fa1463896d5048e0e43a9b55db1ed82c6441e693#n2740
.. _a bit further: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/emacs-lisp/cl-lib.el?id=fa1463896d5048e0e43a9b55db1ed82c6441e693#n92
