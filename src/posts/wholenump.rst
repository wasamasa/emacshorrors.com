((title . "wholenump")
 (date . "2015-09-19 21:34:54 +0200"))

Like any self-respecting Lisp dialect, Emacs Lisp does come with a
slew of predicates which regularly confuse newcomers to the language.
As I'm hacking on a one-letter library for vector operations, I did
research whether there were any lesser common ones I could reuse for
even more succinct code.  ``nlistp`` qualifies as it's the negated
variant of ``listp`` [1]_, the other one I've found was ``natnump``
which checks whether its argument is a natural number.  Finally, I've
found ``wholenump`` which I did assume to be a predicate for whole
numbers, but apparently it's not.  In English you would call these
"integers", so ``integerp`` is what you'd want.  What exactly would
``wholenump`` do then?

Using ``F1 v`` on both ``natnump`` and ``wholenump`` gave me
docstrings showing exactly the same text save the function names.
Using ``find-function`` on them leads me to the definition of
``natnump``.  What is going on here and why does the help system not
list ``wholenump`` as alias to ``natnump`` despite it behaving like
one?

data.c_ knows the culprit:

.. code:: c

    set_symbol_function (Qwholenump, XSYMBOL (Qnatnump)->function);

.. [1] `The byte-compiler`_ even turns ``nlistp`` into ``(not (listp
       ...))``, so I suspect it's an archaic variant given the low
       number of occurrences in the sources.

.. _data.c: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/data.c?id=2ebcda4da0683f0c9102779bde13843795d6db78#n3678
.. _The byte-compiler: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/emacs-lisp/bytecomp.el?id=2ebcda4da0683f0c9102779bde13843795d6db78#n4092
