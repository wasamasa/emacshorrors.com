((title . "State of Art")
 (date . "2015-06-01 17:59:09"))

The Emacs C sources do come with the probably ugliest `indentation
style`_ (as condemned by the `Linux kernel coding style guide`_):

    First off, I'd suggest printing out a copy of the GNU
    coding standards, and NOT read it.  Burn them, it's a great
    symbolic gesture.

The only reason I can think up justifying it is the vain attempt of
making the code look vaguely Lisp-like.  To understand the heavy macro
abuse inflicted upon the codebase, one must read lisp.h_ thoroughly
(which does unexpectedly have `an explanation`_ for it):

.. code:: c

    /* Some operations are so commonly executed that they are implemented
       as macros, not functions, because otherwise runtime performance would
       suffer too much when compiling with GCC without optimization.
       There's no need to inline everything, just the operations that
       would otherwise cause a serious performance problem.

       For each such operation OP, define a macro lisp_h_OP that contains
       the operation's implementation.  That way, OP can be implemented
       via a macro definition like this:

         #define OP(x) lisp_h_OP (x)

       and/or via a function definition like this:

         LISP_MACRO_DEFUN (OP, Lisp_Object, (Lisp_Object x), (x))

       which macro-expands to this:

         Lisp_Object (OP) (Lisp_Object x) { return lisp_h_OP (x); }

       without worrying about the implementations diverging, since
       lisp_h_OP defines the actual implementation.  The lisp_h_OP macros
       are intended to be private to this include file, and should not be
       used elsewhere.

       FIXME: Remove the lisp_h_OP macros, and define just the inline OP
       functions, once most developers have access to GCC 4.8 or later and
       can use "gcc -Og" to debug.  Maybe in the year 2016.  See
       Bug#11935.

       Commentary for these macros can be found near their corresponding
       functions, below.  */

As confirmed on emacs-devel_, this is the current State of Art™ to
ensure all developers using the unoptimized build do not have to
endure an absolutely terrible user experience.  Enduring absolutely
terrible code is a small price to pay for not requiring a compiler
with the ``-Og`` option or the willingness to have a development build
ready.

.. _indentation style: https://www.gnu.org/prep/standards/html_node/Formatting.html
.. _Linux kernel coding style guide: https://www.kernel.org/doc/Documentation/CodingStyle
.. _lisp.h: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/lisp.h
.. _an explanation: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/lisp.h?id=40b33be830310726048dddaee3fdfba5c8a3480f#n293
.. _emacs-devel: http://lists.gnu.org/archive/html/emacs-devel/2015-04/msg01139.html
