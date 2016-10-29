((title . "PSA: Emacs is not a proper GTK application")
 (date . "2016-10-29 15:30:38 +0200"))

`Daniel Colascione's excellent write-up on bringing double-buffered
rendering to Emacs`_ has prompted me to do the same on a set of
questions that can be occasionally spotted on ``#emacs``:

- Which GUI build of Emacs shall I choose?
- What's the difference between the GTK build and the other builds of
  Emacs on Linux?
- Does the GTK build run on Wayland?
- Does the GTK build run on Broadway_?
- Why does Emacs not render as nicely as ``<insert more modern text editor>``?

If you've ever programmed a GUI application with a modern/popular GUI
toolkit, you'll have noticed that while it gives you loads of
desirable features, it forces you to structure your application around
its idea of an event loop.  In other words, your application is forced
to react asynchronously to user events.  Games are pretty much the
only major kind of graphical application that can get away with doing
their own event loop, but end up doing their own GUI instead.

Now, the issue with Emacs is that it does its own event loop,
pretending that the frontend is a textual or graphical terminal.  It's
pretty much the graphical equivalent of a REPL and at the time it only
had a text terminal frontend, this way of doing things worked out
fairly well.  However, by the time users demanded having pretty GTK
widgets in Emacs, it became clear that more involved hacks were needed
to make that happen.  This is why Emacs runs the GTK event loop one
iteration at a time, pushes its own user events into it (to make
widgets react) and a plethora of more hacks to reconcile their
rendering with everything done by X11.

In other words, Emacs is more of a X11 application plus your favorite
widgets.  The choice of GUI toolkit to build it with is mostly
irrelevant, save an infamous bug with the GTK3 frontend that can crash
the daemon.  Emacs will therefore not run on a pure Wayland system or
under Broadway in the browser.  If anyone would want to make that
happen, either the GTK frontend would need to yank out everything X11
(unlikely as it's deeply entrenched) or to create a new frontend doing
platform-agnostic drawing (the Cairo feature being a prime candidate
for that).

Further reading material:

- SIGIO: http://ajaxxx.livejournal.com/62378.html
- blockinput.h
- xterm.c
- gtkutil.c (more specifically, ``xg_display_open`` used from ``x_term_init``)

.. _Daniel Colascione's excellent write-up on bringing double-buffered rendering to Emacs: https://www.facebook.com/notes/daniel-colascione/buttery-smooth-emacs/10155313440066102
.. _Broadway: https://developer.gnome.org/gtk3/stable/gtk-broadway.html
