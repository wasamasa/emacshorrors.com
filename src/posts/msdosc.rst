((title . "msdos.c")
 (date . "2015-08-23 13:19:54 +0200"))

Did you know Emacs does not only `support the full range of Windows
operating systems`_, but even `works on MS-DOS`_?  One would have
assumed the support for this `officially unsupported`_ platform would
have been snipped out already, but Eli Zaretskii, a high profile core
Emacs developer best known for their contributions to the display
engine, `is still contributing to it`_.

The build process is slightly weird.  You do need GCC to support the
full range of GNUisms the code is riddled with, but GCC surely doesn't
support DOS... or does it?  Meet DJGPP_!

That being said, msdos.c_ is surprisingly easy to read compared to the
rest I've dipped my toes into.  Must be the low number of C macro
abuse incidents, well commented code and the overall simplicity of the
system target, much unlike modern platforms.  I urge you to take a
look as well, here's my top ten comments:

10.

.. code:: c

    /* Note the startup time, so we know not to clear the screen if we
       exit immediately; see IT_reset_terminal_modes.
       (Yes, I know `clock' returns zero the first time it's called, but
       I do this anyway, in case some wiseguy changes that at some point.)  */
    startup_time = clock ();

9.

.. code:: c

    /* We only look at the keyboard Ctrl/Shift/Alt keys when
       Emacs is ready to read a key.  Therefore, if they press
       `Alt-x' when Emacs is busy, by the time we get to
       `dos_get_modifiers', they might have already released the
       Alt key, and Emacs gets just `x', which is BAD.
       However, for keys with the `Map' property set, the ASCII
       code returns zero only if Alt is pressed.  So, when we DON'T
       have to support international_keyboard, we don't have to
       distinguish between the left and  right Alt keys, and we
       can set the META modifier for any keys with the `Map'
       property if they return zero ASCII code (c = 0).  */

8.

.. code:: c

    /* Emacs calls cursor-movement functions a lot when it updates the
       display (probably a legacy of old terminals where you cannot
       update a screen line without first moving the cursor there).
       However, cursor movement is expensive on MSDOS (it calls a slow
       BIOS function and requires 2 mode switches), while actual screen
       updates access the video memory directly and don't depend on
       cursor position.  To avoid slowing down the redisplay, we cheat:
       all functions that move the cursor only set internal variables
       which record the cursor position, whereas the cursor is only
       moved to its final position whenever screen update is complete.

       `IT_cmgoto' is called from the keyboard reading loop and when the
       frame update is complete.  This means that we are ready for user
       input, so we update the cursor position to show where the point is,
       and also make the mouse pointer visible.

       Special treatment is required when the cursor is in the echo area,
       to put the cursor at the end of the text displayed there.  */

7.

.. code:: c

    /* Define a lot of environment variables if not already defined.  Don't
       remove anything unless you know what you're doing -- lots of code will
       break if one or more of these are missing.  */

6.

.. code:: c

    /* Time zone determined from country code.  To make this possible, the
       country code may not span more than one time zone.  In other words,
       in the USA, you lose.  */

5.

.. code:: c

    /* FIXME: I'm not sure the above will run at all on DOS/V.  But let's
       be defensive anyway.  */
    if (screen_virtual_segment)
      dosv_refresh_virtual_screen (0, *cols * *rows);

4.

.. code:: c

    /* Simulation of X's menus.  Nothing too fancy here -- just make it work
       for now.

       Actually, I don't know the meaning of all the parameters of the functions
       here -- I only know how they are called by xmenu.c.  I could of course
       grab the nearest Xlib manual (down the hall, second-to-last door on the
       left), but I don't think it's worth the effort.  */

3.

.. code:: c

    /* In some sense all dos users have root privileges, so...  */
    setenv ("USER", "root", 0);
    setenv ("NAME", getenv ("USER"), 0);

2.

.. code:: c

    /* Don't restore the screen if we are exiting less than 2 seconds
       after startup: we might be crashing, and the screen might show
       some vital clues to what's wrong.  */

1.

.. code:: c

    /* We have a situation here.  ScreenUpdate has just restored the
       screen contents as it was before we started drawing this menu.
       That includes any echo area message that could have been
       displayed back then.  (In reality, that echo area message will
       almost always be the ``keystroke echo'' that echoes the sequence
       of menu items chosen by the user.)  However, if the menu had some
       help messages, then displaying those messages caused Emacs to
       forget about the original echo area message.  So when
       ScreenUpdate restored it, it created a discrepancy between the
       actual screen contents and what Emacs internal data structures
       know about it.

       To avoid this conflict, we force Emacs to restore the original
       echo area message as we found it when we entered this function.
       The irony of this is that we then erase the restored message
       right away, so the only purpose of restoring it is so that
       erasing it works correctly...  */

.. _support the full range of Windows operating systems: https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Which-versions-of-Windows.html
.. _works on MS-DOS: https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Other-versions-of-Emacs.html
.. _officially unsupported: https://support.microsoft.com/en-us/gp/lifeobsoleteproducts
.. _is still contributing to it: http://git.savannah.gnu.org/cgit/emacs.git/log/src/msdos.c?qt=author&q=Eli+Zaretski
.. _DJGPP: http://www.delorie.com/djgpp/
.. _msdos.c: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/msdos.c?id=2f0d41ea4fd47bbc53a53d7634869b21cf03c1a0
