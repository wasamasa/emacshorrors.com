((title . "Bloodcurdling")
 (date . "2015-03-09 14:40:09 +0200"))

.. code:: c

   #ifdef USE_LUCID
         /* Bloodcurdling hack alert: The Lucid menu bar widget's
   	 redisplay procedure is not called when a tip frame over menu
   	 items is unmapped.  Redisplay the menu manually...  */
         {
           Widget w;
   	struct frame *f = SELECTED_FRAME ();
   	w = f->output_data.x->menubar_widget;

   	if (!DoesSaveUnders (FRAME_DISPLAY_INFO (f)->screen)
   	    && w != NULL)
   	  {
   	    block_input ();
   	    xlwmenu_redisplay (w);
   	    unblock_input ();
   	  }
         }
   #endif /* USE_LUCID */

This_ is part of the code responsible for tooltips.  In case you were
wondering why certain kinds of tooltips persist upon switching
workspaces, it's because they're proper windows that are subject to
the whims of your window manager.  I've still got to investigate
whether that's just the case for non-system popups or all kinds of
them.

So, Emacs creates a new, very bare-bones frame and controls it which
works very well for the default tooltips displayed upon hovering on
menu bar buttons, but less well for tooltips created by libraries such
as pos-tip_.  The previous code snippet demonstrates another case when
this design becomes obvious for the Lucid toolkit which apparently did
not redraw the menu bar on hiding the tooltip.

.. _This: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/xfns.c?id=b7ed48c3ce8e77acc08d4948684333bef3238d2d#n5721
.. _pos-tip: https://github.com/pitkali/pos-tip
