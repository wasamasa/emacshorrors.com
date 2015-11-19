((title . "Contemplating Our Navels")
 (date . "2015-11-19 18:42:10 +0100"))

I've blathered before on `the topic of commands and how one feeds them
their arguments when used interactively`_.  Considering that a command
is just a function with an ``interactive`` spec, one cannot help but
wonder whether it's possible for its code to determine whether the
command was invoked interactively or not.  Let's not worry about how
much sense it would make for a moment and instead gaze at `the
sources`_ of ``called-interactively-p``:

.. code:: elisp

    (defun called-interactively-p (&optional kind)
      "[...]"
      (declare (advertised-calling-convention (kind) "23.1"))
      (when (not (and (eq kind 'interactive)
                      (or executing-kbd-macro noninteractive)))
        (let* ((i 1) ;; 0 is the called-interactively-p frame.
               frame nextframe
               (get-next-frame
                (lambda ()
                  (setq frame nextframe)
                  (setq nextframe (backtrace-frame i 'called-interactively-p))
                  ;; (message "Frame %d = %S" i nextframe)
                  (setq i (1+ i)))))
          (funcall get-next-frame) ;; Get the first frame.
          (while
              ;; FIXME: The edebug and advice handling should be made modular and
              ;; provided directly by edebug.el and nadvice.el.
              (progn
                ;; frame    =(backtrace-frame i-2)
                ;; nextframe=(backtrace-frame i-1)
                (funcall get-next-frame)
                ;; `pcase' would be a fairly good fit here, but it sometimes moves
                ;; branches within local functions, which then messes up the
                ;; `backtrace-frame' data we get,
                (or
                 ;; Skip special forms (from non-compiled code).
                 (and frame (null (car frame)))
                 ;; Skip also `interactive-p' (because we don't want to know if
                 ;; interactive-p was called interactively but if it's caller was)
                 ;; and `byte-code' (idem; this appears in subexpressions of things
                 ;; like condition-case, which are wrapped in a separate bytecode
                 ;; chunk).
                 ;; FIXME: For lexical-binding code, this is much worse,
                 ;; because the frames look like "byte-code -> funcall -> #[...]",
                 ;; which is not a reliable signature.
                 (memq (nth 1 frame) '(interactive-p 'byte-code))
                 ;; Skip package-specific stack-frames.
                 (let ((skip (run-hook-with-args-until-success
                              'called-interactively-p-functions
                              i frame nextframe)))
                   (pcase skip
                     (`nil nil)
                     (`0 t)
                     (_ (setq i (+ i skip -1)) (funcall get-next-frame)))))))
          ;; Now `frame' should be "the function from which we were called".
          (pcase (cons frame nextframe)
            ;; No subr calls `interactive-p', so we can rule that out.
            (`((,_ ,(pred (lambda (f) (subrp (indirect-function f)))) . ,_) . ,_) nil)
            ;; In case #<subr funcall-interactively> without going through the
            ;; `funcall-interactively' symbol (bug#3984).
            (`(,_ . (t ,(pred (lambda (f)
                                (eq internal--funcall-interactively
                                    (indirect-function f))))
                       . ,_))
             t)))))

I generally advise against using this.  Not only is examining the call
stack for no real reason gross, it isn't reliable either in the
presence of instrumented code!  Either rework your code to not need
this at all or in the unlikely case of it being necessary, let
``call-interactively`` fill in an (optional) ``INTERACTIVEP`` argument
for your command to examine.

I'm not surprised that `Guile Emacs did replace this function`_ with
an version always returning ``nil``.

.. _the topic of commands and how one feeds them their arguments when used interactively: http://emacshorrors.com/posts/over-engineered-code-is-over-engineered.html
.. _the sources: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/subr.el?id=ebad964b3afbe5ef77085be94cf566836450c74c#n4328
.. _Guile Emacs did replace this function: http://git.hcoop.net/?p=bpt/emacs.git;a=blob;f=lisp/subr2.el;h=52f9306833367845a21dafc7a8b26c23d1f61983;hb=HEAD
