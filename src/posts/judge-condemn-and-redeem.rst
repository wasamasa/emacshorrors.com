((title . "Judge, Condemn and Redeem")
 (date . "2016-05-26 20:43:02 +0200"))

.. code:: c

    /* The following three hooks are used when we're doing a thorough
       redisplay of the frame.  We don't explicitly know which scroll bars
       are going to be deleted, because keeping track of when windows go
       away is a real pain - "Can you say set-window-configuration, boys
       and girls?"  Instead, we just assert at the beginning of redisplay
       that *all* scroll bars are to be removed, and then save a scroll bar
       from the fiery pit when we actually redisplay its window.  */

    /* Arrange for all scroll bars on FRAME to be removed at the next call
       to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
       `*redeem_scroll_bar_hook' is applied to its window before the judgment.  */

    static void
    w32_condemn_scroll_bars (struct frame *frame)
    {
      if (!NILP (FRAME_SCROLL_BARS (frame)))
        {
          if (!NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
            {
              /* Prepend scrollbars to already condemned ones.  */
              Lisp_Object last = FRAME_SCROLL_BARS (frame);

              while (!NILP (XSCROLL_BAR (last)->next))
                last = XSCROLL_BAR (last)->next;

              XSCROLL_BAR (last)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
              XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = last;
            }

          fset_condemned_scroll_bars (frame, FRAME_SCROLL_BARS (frame));
          fset_scroll_bars (frame, Qnil);
        }
    }


    /* Un-mark WINDOW's scroll bar for deletion in this judgment cycle.
       Note that WINDOW isn't necessarily condemned at all.  */

    static void
    w32_redeem_scroll_bar (struct window *w)
    {
      struct scroll_bar *bar;
      Lisp_Object barobj;
      struct frame *f;

      /* We can't redeem this window's scroll bar if it doesn't have one.  */
      if (NILP (w->vertical_scroll_bar) && NILP (w->horizontal_scroll_bar))
        emacs_abort ();

      if (!NILP (w->vertical_scroll_bar) && WINDOW_HAS_VERTICAL_SCROLL_BAR (w))
        {
          bar = XSCROLL_BAR (w->vertical_scroll_bar);
          /* Unlink it from the condemned list.  */
          f = XFRAME (WINDOW_FRAME (w));
          if (NILP (bar->prev))
            {
              /* If the prev pointer is nil, it must be the first in one of
                 the lists.  */
              if (EQ (FRAME_SCROLL_BARS (f), w->vertical_scroll_bar))
                /* It's not condemned.  Everything's fine.  */
                goto horizontal;
              else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
                           w->vertical_scroll_bar))
                fset_condemned_scroll_bars (f, bar->next);
              else
                /* If its prev pointer is nil, it must be at the front of
                   one or the other!  */
                emacs_abort ();
            }
          else
            XSCROLL_BAR (bar->prev)->next = bar->next;

          if (! NILP (bar->next))
            XSCROLL_BAR (bar->next)->prev = bar->prev;

          bar->next = FRAME_SCROLL_BARS (f);
          bar->prev = Qnil;
          XSETVECTOR (barobj, bar);
          fset_scroll_bars (f, barobj);
          if (! NILP (bar->next))
            XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
        }

     horizontal:
      if (!NILP (w->horizontal_scroll_bar) && WINDOW_HAS_HORIZONTAL_SCROLL_BAR (w))
        {
          bar = XSCROLL_BAR (w->horizontal_scroll_bar);
          /* Unlink it from the condemned list.  */
          f = XFRAME (WINDOW_FRAME (w));
          if (NILP (bar->prev))
            {
              /* If the prev pointer is nil, it must be the first in one of
                 the lists.  */
              if (EQ (FRAME_SCROLL_BARS (f), w->horizontal_scroll_bar))
                /* It's not condemned.  Everything's fine.  */
                return;
              else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
                           w->horizontal_scroll_bar))
                fset_condemned_scroll_bars (f, bar->next);
              else
                /* If its prev pointer is nil, it must be at the front of
                   one or the other!  */
                emacs_abort ();
            }
          else
            XSCROLL_BAR (bar->prev)->next = bar->next;

          if (! NILP (bar->next))
            XSCROLL_BAR (bar->next)->prev = bar->prev;

          bar->next = FRAME_SCROLL_BARS (f);
          bar->prev = Qnil;
          XSETVECTOR (barobj, bar);
          fset_scroll_bars (f, barobj);
          if (! NILP (bar->next))
            XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
        }
    }

    /* Remove all scroll bars on FRAME that haven't been saved since the
       last call to `*condemn_scroll_bars_hook'.  */

    static void
    w32_judge_scroll_bars (struct frame *f)
    {
      Lisp_Object bar, next;

      bar = FRAME_CONDEMNED_SCROLL_BARS (f);

      /* Clear out the condemned list now so we won't try to process any
         more events on the hapless scroll bars.  */
      fset_condemned_scroll_bars (f, Qnil);

      for (; ! NILP (bar); bar = next)
        {
          struct scroll_bar *b = XSCROLL_BAR (bar);

          x_scroll_bar_remove (b);

          next = b->next;
          b->next = b->prev = Qnil;
        }

      /* Now there should be no references to the condemned scroll bars,
         and they should get garbage-collected.  */
    }

Source_.

.. _Source: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/w32term.c?id=21e87ece97e90f5500bd49b84dca08d97bd4f155#n4012
