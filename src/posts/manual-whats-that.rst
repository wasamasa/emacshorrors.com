((title . "\"Manual? What's that?\"")
 (date . "2015-11-26 22:46:03 +0100"))

``perl-mode`` is a thing.  So is ``cperl-mode``, a more extensive
major mode for editing Perl.  I've tried out the latter briefly for
viewing a script, then did succumb to my curiosity and checked the
sources after noticing weird behaviour.

.. code:: elisp

    ;; DO NOT FORGET to read micro-docs (available from `Perl' menu)   <<<<<<
    ;; or as help on variables `cperl-tips', `cperl-problems',         <<<<<<
    ;; `cperl-praise', `cperl-speed'.                                  <<<<<<

Uh-oh_.

.. code:: elisp

    (defvar cperl-problems 'please-ignore-this-line
      "Description of problems in CPerl mode.
    Some faces will not be shown on some versions of Emacs unless you
    install choose-color.el, available from
      http://ilyaz.org/software/emacs

    `fill-paragraph' on a comment may leave the point behind the
    paragraph.  It also triggers a bug in some versions of Emacs (CPerl tries
    to detect it and bulk out).

    See documentation of a variable `cperl-problems-old-emaxen' for the
    problems which disappear if you upgrade Emacs to a reasonably new
    version (20.3 for Emacs, and those of 2004 for XEmacs).")

Notice the name of `this variable`_, its docstring (which doesn't
describe it) and the peculiar ``please-ignore-this-line`` value.  Why
would you possibly abuse it for what looks like something for a
manual?

.. code:: elisp

    (condition-case nil
        (progn
          (require 'easymenu)
          (easy-menu-define
           cperl-menu cperl-mode-map "Menu for CPerl mode"
           '("Perl"
             […]
             ("Micro-docs"
              ["Tips" (describe-variable 'cperl-tips) t]
              ["Problems" (describe-variable 'cperl-problems) t]
              ["Speed" (describe-variable 'cperl-speed) t]
              ["Praise" (describe-variable 'cperl-praise) t]
              ["Faces" (describe-variable 'cperl-tips-faces) t]
              ["CPerl mode" (describe-function 'cperl-mode) t]
              ["CPerl version"
               (message "The version of master-file for this CPerl is %s-Emacs"
                        cperl-version) t]))))
      (error nil))

So, that's why.  Clicking on `these menu items`_ gives you a poor
substitute for a properly written manual.

.. _Uh-oh: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/cperl-mode.el?id=75336a29460752700e4e424a9c7aa932cc237689#n37
.. _this variable: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/cperl-mode.el?id=75336a29460752700e4e424a9c7aa932cc237689#n760
.. _these menu items: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/cperl-mode.el?id=75336a29460752700e4e424a9c7aa932cc237689#n1223
