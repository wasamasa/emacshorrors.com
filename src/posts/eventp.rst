((title . "eventp")
 (date . "2015-10-29 09:24:44 +0100"))

I've been reading "Coders At Work" lately, in the faint hope of
obsessing over something else than Emacs for a change.  Things do of
course rarely go the way we plan them to, so the first chapter did of
course have to cover a famous Emacs hacker, namely Jamie Zawinski.

Somewhere in the middle of the chapter [1]_ he complains that Emacs
isn't particularly elegant about events:

    […]there should be event objects instead of a list with a number in
    it.  Having an event object be a list with a number in it—that's
    just tasteless.  It's icky.

A pretty understandable thing to disagree on.  Especially if you
consider how markers and window configurations are opaque objects for
no real reason.  Not particularly consistent design if you ask me.
Surely that one mistake has been fixed in the following decade, right?

.. code:: elisp

    (defun eventp (obj)
      "True if the argument is an event object."
      (when obj
        (or (integerp obj)
            (and (symbolp obj) obj (not (keywordp obj)))
            (and (consp obj) (symbolp (car obj))))))

Nope_.  It has become even worse.

.. [1] Peter Seibel, *Coders At Work*, p.35

.. _Nope: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/subr.el?id=2765945d616f9661dd0aa641f4ecd328dd8768d7#n1003
