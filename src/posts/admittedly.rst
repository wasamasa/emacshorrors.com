((title . "Admittedly")
 (date . "2015-10-23 21:37:09 +0200"))

Reverting a buffer can be useful once in awhile, so I did bind
``revert-buffer`` to a key.  That would have done the trick, weren't
it for Emacs and its lousy defaults which include prompting you before
reverting a file.  It's one thing to prompt someone when an external
package is reverting a buffer for you, but if it's done by yourself
interactively...

To the source!  While figuring out the right invocation, I've stumbled
over `this gem`_:

.. code:: elisp

    ;; I admit it's odd to reverse the sense of the prefix argument, but
    ;; there is a lot of code out there which assumes that the first
    ;; argument should be t to avoid consulting the auto-save file, and
    ;; there's no straightforward way to encourage authors to notice a
    ;; reversal of the argument sense.  So I'm just changing the user
    ;; interface, but leaving the programmatic interface the same.

Way to go.

.. _this gem: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/files.el?id=8b47daab5ce7da394a057f40aa2738b6c204c2f5#n5516
