((title . "Debugging? Don't ask me, I just work here!")
 (date . "2015-08-14 00:16:47"))

cc-mode_ is about 20k SLOC of Emacs Lisp responsible for setting up
major modes for C, C++ and other sufficiently C-like languages.  To
put this into perspective, ``ruby-mode`` is 2k SLOC and ``text-mode``
about 200 SLOC.  Surely there must be clever and not so clever things
included into it!

And indeed, while attempting to figure out why `indentation went
wrong`_ for csharp-mode_ by squinting hard at
c-get-syntactic-indentation_, I've stumbled upon a most curious
variable, ``c-syntactic-element``:

.. code:: elisp

    (defun c-get-syntactic-indentation (langelems)
      ;; ...
      (let ((indent 0) anchor)

        (while langelems
          (let* ((c-syntactic-element (car langelems))
	         (res (c-calc-offset c-syntactic-element)))
            ...))))


Naturally I tried inspecting its value with good ol' ``F1 v``:

.. code:: text

    c-syntactic-element is void as a variable.

    ...

    This is always bound dynamically.  It should never be set
    statically (e.g. with `setq').

Well, would you look at that.  This variable was defined, but never
initialized.  In fact, its value is only non-void when dynamic binding
is used to bind it throughout the invocation of whatever ends up
calling ``c-get-syntactic-indentation``.  Usually you'd only have a
docstring if you were to use ``defvar`` which requires an initial
value, but this requirement was cleverly circumvented by directly
adding a docstring property to the symbol of the variable after its
definition.

I guess this is done to avoid passing the value explicitly, but it
still annoys me that I need to instrument a function to figure out the
value of a variable it relies on.  Worse, this hack would most likely
break if lexical binding were to be enabled for ``cc-mode`` and turns
any meaningful (read: not involving subtle and less subtle breakage)
refactoring of the code into a surprisingly difficult task.  The only
place I can spot where ``c-syntactic-element`` *is* dynamically bound
is said function, but it is not clear how calling it affects the
number of other functions using that variable.

I'm sure a set of specific helpers is included to simplify debugging,
but it still annoys me when something from the official toolbox is
rendered unusable.

.. _cc-mode: http://cc-mode.sourceforge.net/
.. _indentation went wrong: https://github.com/josteink/csharp-mode/pull/43#issuecomment-129761129
.. _csharp-mode: https://github.com/josteink/csharp-mode
.. _c-get-syntactic-indentation: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/cc-engine.el?id=acac9f4d727072b31914c9224957ff8dfec97df1#n11186
