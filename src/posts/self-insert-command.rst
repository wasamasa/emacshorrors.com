((title . "self-insert-command")
 (date . "2015-11-16 21:57:51 +0100"))

Let's assume for a moment that you're writing a text editor.  Because
you're in love with the Emacs design, but not so much with the
implementation, you're obviously going for carefully chosen primitives
and implement as much as possible in a higher-level language that can
be used to extend this editor at runtime.  So far, so good.  But at
some point you'll need to actually type in text into your creation.
How are you going to do that for every available key that's supposed
to insert what's printed on it?

The elegant solution is to design a single command that can look up
the key that fired it and insert that character.  Given this
information, one can write such a thing in pure Emacs Lisp easily:

.. code:: elisp

    (defun my-self-insert-command ()
      (interactive)
      (insert (this-command-keys)))

All that's left to do is binding that command to all self-inserting
keys which is done by looping over the list of printable characters.
A possible solution for this in both C and Emacs Lisp is given on `the
Emacs StackExchange`_.

Unfortunately ``self-insert-command`` is `not defined in Emacs Lisp`_.
It's defined in the core and does a number of things more:

- Handle the numerical argument to insert the character n times
- Adjust the undo boundary
- Check the key it's about to insert
- Hide the mouse
- Implement overwrite
- Implement abbrevs
- Implement auto-filling
- Run ``post-self-insert-hook``

I understand most of these, but implementing the meat of
``overwrite-mode``, ``abbrev-mode`` and ``auto-fill-mode``?  No wonder
newline_ is so complicated if it needs to take care of executing these
and its own quirks in the correct order...

.. _the Emacs StackExchange: http://emacs.stackexchange.com/a/3883/10
.. _not defined in Emacs Lisp: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/cmds.c?id=937565268a5dc3377d4c9bff6d48eb3645a77160#n263
.. _newline: http://emacshorrors.com/posts/newline.html
