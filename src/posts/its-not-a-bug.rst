((title . "It's Not A Bug")
 (date . "2016-01-07 21:19:06 +0100"))

There's more than just the splash screen to disable for a minimalistic
looking Emacs setup.  The menu, scroll and tool bar are a close
second.  While taking a look how one would customize the buttons of
the latter, I've found `a rather curious code snippet`_:

.. code:: elisp

    (if (featurep 'move-toolbar)
        (defcustom tool-bar-position 'top
          "Specify on which side the tool bar shall be.
    Possible values are `top' (tool bar on top), `bottom' (tool bar at bottom),
    `left' (tool bar on left) and `right' (tool bar on right).
    Customize `tool-bar-mode' if you want to show or hide the tool bar."
          :version "24.1"
          :type '(choice (const top)
                         (const bottom)
                         (const left)
                         (const right))
          :group 'frames
          :initialize 'custom-initialize-default
          :set (lambda (sym val)
                 (set-default sym val)
                 (modify-all-frames-parameters
                  (list (cons 'tool-bar-position val))))))

Emacs comes with rather rudimentary library support.  You can use
``provide`` at the end of a library to inform the current Emacs
session of it and ``require`` to load the library from ``load-path``
if it hasn't been loaded before.  If you've spotted usage of a library
you wish to inspect, ``M-x find-library`` is your friend.  Now, what's
unusual about the code snippet above is that ``move-toolbar`` is most
definitely not a library and even weirder, it doesn't seem to be
unconditionally provided.  Just what the hell is going on there?

.. code:: c

    #ifdef USE_GTK
      /* Provide x-toolkit also for GTK.  Internally GTK does not use Xt so it
         is not an X toolkit in that sense (USE_X_TOOLKIT is not defined).
         But for a user it is a toolkit for X, and indeed, configure
         accepts --with-x-toolkit=gtk.  */
      Fprovide (intern_c_string ("x-toolkit"), Qnil);
      Fprovide (intern_c_string ("gtk"), Qnil);
      Fprovide (intern_c_string ("move-toolbar"), Qnil);

      DEFVAR_LISP ("gtk-version-string", Vgtk_version_string,
                   doc: /* Version info for GTK+.  */);
      {
        char gtk_version[sizeof ".." + 3 * INT_STRLEN_BOUND (int)];
        int len = sprintf (gtk_version, "%d.%d.%d",
                           GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
        Vgtk_version_string = make_pure_string (gtk_version, len, len, false);
      }
    #endif /* USE_GTK */

So, this_ is basically an ``#ifdef`` that's been exported to the Lisp
level.  I can't help but wonder why this couldn't have just been made
a variable...

.. _a rather curious code snippet: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/tool-bar.el?id=4580671f5e2a68885e0fca93eeaf9daaeebe82b3#n286
.. _this: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/xfns.c?id=4580671f5e2a68885e0fca93eeaf9daaeebe82b3#n6902
