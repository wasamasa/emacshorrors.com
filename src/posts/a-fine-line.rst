((title . "A Fine Line")
 (date . "2015-03-05 10:50:31 +0200"))

Makefile.in_ strikes again:

.. code:: makefile

    # We run make-docfile twice because the command line may get too long
    # on some systems.  The sed command operating on lisp.mk also reduces
    # the length of the command line.  Unfortunately, no-one has any idea
    # exactly how long the maximum safe command line length is on all the
    # various systems that Emacs supports.  Obviously, the length depends
    # on what your value of $srcdir is.  If the length restriction goes
    # away, lisp.mk can be merged back into this file.

.. _Makefile.in: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/Makefile.in?id=da02eb556a8751c59d5946dec353804bb826c712#n462
