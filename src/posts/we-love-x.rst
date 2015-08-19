((title . "We Love X")
 (date . "2015-05-13 15:55:32"))

.. code:: c

    /* Some window managers support a focus-follows-mouse style with
       delayed raising of frames.  Imagine a partially obscured frame,
       and moving the mouse into partially obscured mouse-face on that
       frame.  The visible part of the mouse-face will be highlighted,
       then the WM raises the obscured frame.  With at least one WM, KDE
       2.1, Emacs is not getting any event for the raising of the frame
       (even tried with SubstructureRedirectMask), only Expose events.
       These expose events will draw text normally, i.e. not
       highlighted.  Which means we must redo the highlight here.
       Subsume it under ``we love X''.  --gerd 2001-08-15  */
    /* Included in Windows version because Windows most likely does not
       do the right thing if any third party tool offers
       focus-follows-mouse with delayed raise.  --jason 2001-10-12  */
    if (mouse_face_overwritten_p && !FRAME_GARBAGED_P (f))
      {
        Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
        if (f == hlinfo->mouse_face_mouse_frame)
          {
            int mouse_x = hlinfo->mouse_face_mouse_x;
            int mouse_y = hlinfo->mouse_face_mouse_y;
            clear_mouse_face (hlinfo);
            note_mouse_highlight (f, mouse_x, mouse_y);
          }
      }

`Windows, too <http://git.savannah.gnu.org/cgit/emacs.git/tree/src/xdisp.c?id=8a9ba4d67bfb3b9cf96cff2917fec1fa7a168724#n30343>`_.
