((title . "Legal Limit")
 (date . "2015-06-22 20:21:59"))

.. code:: c

    /* Push x onto the execution stack.  This used to be #define PUSH(x)
       (*++stackp = (x)) This oddity is necessary because Alliant can't be
       bothered to compile the preincrement operator properly, as of 4/91.
       -JimB */

    #define PUSH(x) (top++, *top = (x))

Source_.

    `Are you sure 21 years is old enough?`_

.. _Source: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/bytecode.c?id=5fac0dee87ea5d4aa90ee93606c19785919da105#n403
.. _Are you sure 21 years is old enough?: https://lists.gnu.org/archive/html/emacs-devel/2012-09/msg00477.html
