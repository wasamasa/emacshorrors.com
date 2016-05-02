((title . "C-C-COMPILE IT IN")
 (date . "2016-05-02 22:43:47 +0200"))

I am implementing MAL_ (again_), this time in ChucK_.  Why an audio
programming language?  Well, as I wielded it to implement `a
pseudo-theremin`_, it dawned upon me that it exposed more than enough
language features to allow for implementing something lispier in it.
A good chance to learn OOP, I thought to myself [1]_.

It's not too fun to write loads of code in a language without a proper
major mode for it [2]_, so I decided to give `CC Mode`_ a try, this
time for creating a mode with it.  Half a day and loads of cursing
later, `I was done`_.  Most of my gripes with it stemmed from the
documentation not covering *that* use case (and the examples being
outdated), so in case you plan to go down that road, I found the
sources of d-mode_ and csharp-mode_ highly useful.  I'll concentrate
on a single, innocent looking warning I got while developing it to
show a less obvious reason for hating this piece of Emacs with the
force of a thousand suns.

If you byte-compile ``chuck-mode.el`` (or FWIW, ``csharp-mode.el``),
you'll get two warnings:

.. code::

    chuck-mode.el:152:4:Warning: (lambda nil ...) quoted with ' rather than with #'
    chuck-mode.el:152:4:Warning: (lambda nil ...) quoted with ' rather than with #'

Following the line numbers isn't particularly enlightening as they
point to the wrong place.  I was already familiar with these warnings
as I've seen them before for ``csharp-mode``, but this time I had
something much smaller to bisect.  Commenting out
``(c-init-language-vars)`` made them disappear, so I looked up its
definition and as it's a macro expanding to a trivial looking function
call, that one's source as well.  Here_ it is, in all of its glory:

.. code:: elisp

    (defun c-make-init-lang-vars-fun (mode)
      "Create a function that initializes all the language dependent variables
    for the given mode.

    This function should be evaluated at compile time, so that the
    function it returns is byte compiled with all the evaluated results
    from the language constants.  Use the `c-init-language-vars' macro to
    accomplish that conveniently."

      (if (cc-bytecomp-is-compiling)
          ;; No need to byte compile this lambda since the byte compiler is
          ;; smart enough to detect the `funcall' construct in the
          ;; `c-init-language-vars' macro below and compile it all straight
          ;; into the function that contains `c-init-language-vars'.
          `(lambda ()

             ;; This let sets up the context for `c-mode-var' and similar
             ;; that could be in the result from `c--macroexpand-all'.
             (let ((c-buffer-is-cc-mode ',mode)
                   current-var source-eval)
               (c-make-emacs-variables-local)
               (condition-case err

                   (if (eq c-version-sym ',c-version-sym)
                       (setq ,@(let ((c-buffer-is-cc-mode mode)
                                     (c-lang-const-expansion 'immediate))
                                 ;; `c-lang-const' will expand to the evaluated
                                 ;; constant immediately in `c--macroexpand-all'
                                 ;; below.
                                  (c--mapcan
                                   (lambda (init)
                                     `(current-var ',(car init)
                                                   ,(car init) ,(c--macroexpand-all
                                                                 (elt init 1))))
                                   ;; Note: The following `append' copies the
                                   ;; first argument.  That list is small, so
                                   ;; this doesn't matter too much.
                                   (append (cdr c-emacs-variable-inits)
                                           (cdr c-lang-variable-inits)))))

                     ;; This diagnostic message isn't useful for end
                     ;; users, so it's disabled.
                     ;;(unless (get ',mode 'c-has-warned-lang-consts)
                     ;;  (message ,(concat "%s compiled with CC Mode %s "
                     ;;                    "but loaded with %s - evaluating "
                     ;;                    "language constants from source")
                     ;;           ',mode ,c-version c-version)
                     ;;  (put ',mode 'c-has-warned-lang-consts t))

                     (setq source-eval t)
                     (let ((init ',(append (cdr c-emacs-variable-inits)
                                           (cdr c-lang-variable-inits))))
                       (dolist (var-init init)
                         (setq current-var (car var-init))
                         (set (car var-init) (eval (cadr var-init))))))

                 (error
                  (if current-var
                      (message "Eval error in the `c-lang-defvar' or `c-lang-setvar' for `%s'%s: %S"
                               current-var
                               (if source-eval
                                   (format "\
     (fallback source eval - %s compiled with CC Mode %s but loaded with %s)"
                                           ',mode ,c-version c-version)
                                 "")
                               err)
                    (signal (car err) (cdr err)))))))

        ;; Being evaluated from source.  Always use the dynamic method to
        ;; work well when `c-lang-defvar's in this file are reevaluated
        ;; interactively.
        `(lambda ()
           (require 'cc-langs)
           (let ((c-buffer-is-cc-mode ',mode)
                 (init (append (cdr c-emacs-variable-inits)
                               (cdr c-lang-variable-inits)))
                 current-var)
             (c-make-emacs-variables-local)
             (condition-case err

                 (dolist (var-init init)
                   (setq current-var (car var-init))
                   (set (car var-init) (eval (cadr var-init))))

               (error
                (if current-var
                    (message
                     "Eval error in the `c-lang-defvar' or `c-lang-setver' for `%s' (source eval): %S"
                     current-var err)
                  (signal (car err) (cdr err)))))))
        ))

For clarification, CC Mode expects you to define constants for your
language.  The above monstrosity turns these constants into values
applied to each buffer using the derived mode, but does it
differently, depending on whether you are in the process of
byte-compiling its file or just load it (like, for re-evaluation while
doing some development).  As the mechanism for that backquotes a
lambda and replaces parts of it, the byte-compiler will naturally warn
us about it as it prohibits it from byte-compiling the lambda (unlike
what the comment suggests).

This means that any differences between the two implementations will
have fun side effects, such as you no longer being able to test your
mode meaningfully by re-evaluating parts of it (forcing you to
recompile and load, ideally in a new instance).  When it comes to its
worst, you're going to have a broken mode just because it wasn't
compiled with a CC Mode compiled with the same Emacs version.
Seriously_.  `Don't do that`_.  It just angers people for no real
reason.  Do what every other major mode does and just set the damn
variables buffer-locally.  Thank you for your understanding.

.. _MAL: https://github.com/kanaka/mal
.. _again: http://emacsninja.com/posts/implementing-mal.html
.. _ChucK: http://chuck.cs.princeton.edu/
.. _a pseudo-theremin: https://github.com/wasamasa/theremin
.. _CC Mode: http://cc-mode.sourceforge.net/
.. _I was done: https://github.com/wasamasa/chuck-mode
.. _d-mode: https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/
.. _csharp-mode: https://github.com/josteink/csharp-mode
.. _Here: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/cc-langs.el?id=d05806fda1cbba2db112bc783597fbe9d27175b2#n3341
.. _Seriously: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23053
.. _Don't do that: http://emacs.stackexchange.com/a/14731/10

.. [1] I found out quickly that while ChucK's idea of OOP is clearly
       Java-inspired, it doesn't implement nearly as much and is
       significantly more limited, so I ended up working against it
       most of the time.
.. [2] I got by initially with an abandoned mode, but found it
       annoying that it wasn't defined properly (which made Evil start
       in the wrong state for me), had way too magic behaviour for
       ``=`` and terrible indentation.  That's why I picked this yak
       to shave...
