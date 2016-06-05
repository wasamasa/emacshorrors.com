((title . "\"One XNOOP in 100 loops will make Emacs terminate\"")
 (date . "2016-06-05 11:50:15 +0200"))

.. code:: c

    /* On some systems, an X bug causes Emacs to get no more events
       when the window is destroyed.  Detect that.  (1994.)  */
    if (! event_found)
      {
        /* Emacs and the X Server eats up CPU time if XNoOp is done every time.
           One XNOOP in 100 loops will make Emacs terminate.
           B. Bretthauer, 1994 */
        x_noop_count++;
        if (x_noop_count >= 100)
          {
            x_noop_count=0;

            if (next_noop_dpyinfo == 0)
              next_noop_dpyinfo = x_display_list;

            XNoOp (next_noop_dpyinfo->display);

            /* Each time we get here, cycle through the displays now open.  */
            next_noop_dpyinfo = next_noop_dpyinfo->next;
          }
      }

I had some difficulties believing that `an over 20 years old comment`_
still holds true, so I snipped this functionality out for fun:

.. code:: diff

    --- a/xterm.c       2016-06-05 11:38:14.471347194 +0200
    +++ b/xterm.c       2016-06-05 11:36:37.398817889 +0200
    @@ -8786,26 +8786,10 @@ XTread_socket (struct terminal *terminal
         }
     #endif /* USE_GTK */

    -  /* On some systems, an X bug causes Emacs to get no more events
    -     when the window is destroyed.  Detect that.  (1994.)  */
       if (! event_found)
         {
    -      /* Emacs and the X Server eats up CPU time if XNoOp is done every time.
    -    One XNOOP in 100 loops will make Emacs terminate.
    -    B. Bretthauer, 1994 */
           x_noop_count++;
    -      if (x_noop_count >= 100)
    -   {
    -     x_noop_count=0;
    -
    -     if (next_noop_dpyinfo == 0)
    -       next_noop_dpyinfo = x_display_list;
    -
    -     XNoOp (next_noop_dpyinfo->display);
    -
    -     /* Each time we get here, cycle through the displays now open.  */
    -     next_noop_dpyinfo = next_noop_dpyinfo->next;
    -   }
    +      fprintf(stderr, "XNoOp %d occurred\n", x_noop_count);
         }

       /* If the focus was just given to an auto-raising frame,

No difference when used on a GTK or Lucid build.  Other than nearly
every event causing the diagnostic message to print...

.. _an over 20 years old comment: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/xterm.c?id=700afe62a4cbd9ecf24551ddc4747e6319fb51a2#n8789
