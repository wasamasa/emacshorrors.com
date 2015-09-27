((title . "You Lose")
 (date . "2015-09-27 12:00:02 +0200"))

.. code:: c

    #ifdef MSDOS
    #ifndef __DJGPP__
    You lose; /* Emacs for DOS must be compiled with DJGPP */
    #endif
    #define _NAIVE_DOS_REGS
    ...

This_ sounds `a bit familiar`_.  I've seen loads of wonky ``ifdef``
cascades, but haven't had one yet telling me in plain english how
hopeless my endeavor is:

.. code:: shell-session

    [wasa@box ~/code/emacs]$ CFLAGS=-DMSDOS make
    ...

    ../src/conf_post.h:122:1: error: unknown type name ‘You’
    You lose; /* Emacs for DOS must be compiled with DJGPP */
    ^

.. _This: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/conf_post.h?id=f49e3a2603f249bb2ec281f3eaedd80cbaef2243#n120
.. _a bit familiar: http://emacshorrors.com/posts/msdosc.html
