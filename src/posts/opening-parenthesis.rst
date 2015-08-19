((title . "OPENING PARENTHESIS")
 (date . "2015-05-01 10:20:26"))

Ever wondered about docstrings containing backslash-escaped opening
parentheses, but only for the ones at the left-most column?  `Wonder
no more`_!  As far as I can tell, this is the least offensive hack in
syntax.c_ which contains the code for determining the class of glyphs
by the means of a so-called `syntax table`_ in a piece of code (such
as comment, string, symbol, etc.) and are required for both correct
movement (like, movement by defun) and as basis for sufficiently fast
syntax highlighting.  It shouldn't be surprising that these mechanisms
only work out reasonably well for C and Lisp, but that's a topic for a
different post.

.. _Wonder no more: https://www.gnu.org/software/emacs/manual/html_node/emacs/Left-Margin-Paren.html#Left-Margin-Paren
.. _syntax.c: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/syntax.c?id=d4d66f4d5409a5d0ab2c821f66c9c9485d7ea9d0
.. _syntax table: http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html#Syntax-Tables
