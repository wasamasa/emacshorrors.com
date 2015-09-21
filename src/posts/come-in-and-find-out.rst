((title . "Come In And Find Out!")
 (date . "2015-09-21 20:22:01 +0200"))

Did you ever wonder how Emacs finds out whether you are using a dark
or light background?  If you've themed it, it can just ask for the
current definition of the ``default`` face, but if you didn't do this
yet, only GUI Emacs can answer this question truthfully.  So, what
does Emacs do for the case of an uncustomized setup in a terminal?
Simple, it has `a hardcoded list of terminal emulators`_ that use
light backgrounds by default and does otherwise assume an unknown one
uses a dark background!  Joke's on you if you're using any of these
with a dark background and wonder why it's looking `so different`_...

.. code:: elisp

    (let* ((frame-default-bg-mode (frame-terminal-default-bg-mode frame))
           (bg-color (frame-parameter frame 'background-color))
           (tty-type (tty-type frame))
           (default-bg-mode
             (if (or (window-system frame)
                     (and tty-type
                          (string-match "^\\(xterm\\|\\rxvt\\|dtterm\\|eterm\\)"
                                        tty-type)))
                 'light
               'dark))
           (non-default-bg-mode (if (eq default-bg-mode 'light) 'dark 'light))
           (bg-mode
            (cond (frame-default-bg-mode)
                  ((equal bg-color "unspecified-fg") ; inverted colors
                   non-default-bg-mode)
                  ((not (color-values bg-color frame))
                   default-bg-mode)
                  ((>= (apply '+ (color-values bg-color frame))
                       ;; Just looking at the screen, colors whose
                       ;; values add up to .6 of the white total
                       ;; still look dark to me.
                       (* (apply '+ (color-values "white" frame)) .6))
                   'light)
                  (t 'dark)))
           (display-type
            (cond ((null (window-system frame))
                   (if (tty-display-color-p frame) 'color 'mono))
                  ((display-color-p frame)
                   'color)
                  ((x-display-grayscale-p frame)
                   'grayscale)
                  (t 'mono)))
           (old-bg-mode
            (frame-parameter frame 'background-mode))
           (old-display-type
            (frame-parameter frame 'display-type)))
      ...)

.. _a hardcoded list of terminal emulators: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/frame.el?id=db828f62f6f17414fbbc3206dac123dc73dd6055#n936
.. _so different: http://emacs.stackexchange.com/q/16802/10
