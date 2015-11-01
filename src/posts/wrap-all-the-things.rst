((title . "Wrap All The Things")
 (date . "2015-11-01 20:09:10 +0100"))

There are things one can do in code that may look pointless, but come
in handy later for their author.  Wrapping a variable in a function
with a similiar name would be `an example`_.

.. code:: elisp

    (defun artist-t-if-fill-char-set ()
      "Return the value of the variable `artist-fill-char-set'."
      artist-fill-char-set)

The excuse here is that an accessor makes up part of an API which must
not be changed.  Even if it's debatable how much of an API
artist-mode_ is.  But let's not sweat the small details and look at
`the abstruse examples`_ instead:

.. code:: elisp

    (defun artist-t ()
      "Always return t."
      t)

    (defun artist-nil ()
      "Always return nil."
      nil)

    (defun artist-arrows ()
      "Say yes to arrows!"
      t)

    (defun artist-no-arrows ()
      "Say no to arrows!"
      nil)

Please say no to wholly useless abstractions.

.. _an example: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/textmodes/artist.el?id=3a769e173ebaaff768497dae9c430ac03aedeb94#n1483
.. _artist-mode: http://www.emacswiki.org/emacs/ArtistMode
.. _the abstruse examples: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/textmodes/artist.el?id=3a769e173ebaaff768497dae9c430ac03aedeb94#n1487
