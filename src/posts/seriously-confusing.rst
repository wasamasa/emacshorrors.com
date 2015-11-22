((title . "Seriously Confusing")
 (date . "2015-11-22 22:44:00 +0100"))

.. code:: c

    /* Display an echo area message in window W.  Value is true if W's
       height is changed.  If display_last_displayed_message_p,
       display the message that was last displayed, otherwise
       display the current message.  */

    static bool
    display_echo_area (struct window *w)
    {
      bool no_message_p, window_height_changed_p;

      /* Temporarily disable garbage collections while displaying the echo
         area.  This is done because a GC can print a message itself.
         That message would modify the echo area buffer's contents while a
         redisplay of the buffer is going on, and seriously confuse
         redisplay.  */
      ptrdiff_t count = inhibit_garbage_collection ();

      /* If there is no message, we must call display_echo_area_1
         nevertheless because it resizes the window.  But we will have to
         reset the echo_area_buffer in question to nil at the end because
         with_echo_area_buffer will sets it to an empty buffer.  */
      bool i = display_last_displayed_message_p;
      no_message_p = NILP (echo_area_buffer[i]);

      window_height_changed_p
        = with_echo_area_buffer (w, display_last_displayed_message_p,
                                 display_echo_area_1,
                                 (intptr_t) w, Qnil);

      if (no_message_p)
        echo_area_buffer[i] = Qnil;

      unbind_to (count, Qnil);
      return window_height_changed_p;
    }

Source_.

.. _Source: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/xdisp.c?id=ea78129522f428888607151e4f91ade1f4839f3f#n10714
