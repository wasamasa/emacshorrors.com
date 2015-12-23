((title . "Forget Me Not")
 (date . "2015-12-23 14:30:35 +0100"))

.. code:: c

    /* PLEASE DO NOT DELETE THIS COMMENTED-OUT VERSION!
       This is the old version of expand-file-name, before it was thoroughly
       rewritten for Emacs 10.31.  We leave this version here commented-out,
       because the code is very complex and likely to have subtle bugs.  If
       bugs _are_ found, it might be of interest to look at the old code and
       see what did it do in the relevant situation.

       Don't remove this code: it's true that it will be accessible
       from the repository, but a few years from deletion, people will
       forget it is there.  */

Let me check, just how old is Emacs 10.31?  According to
`the Emacs timeline`_ Emacs 13.0x was the first public release -
in 1985.  Feel free to study `the over 30 years old sources`_, simply
because they're significantly more readable than what's making up
``expand-file-name`` these days.

Oh and yes, there's of course been fools who tried removing this.
The comment itself was added `in 2001`_ after such an incident.

.. _the Emacs timeline: https://www.jwz.org/doc/emacs-timeline.html
.. _the over 30 years old sources: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/fileio.c?id=1dcf9a5d2a727913bc53c68756bdaf6db5573671#n1447
.. _in 2001: http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=4887597a1c52b8277eabeb4d9484c75b1bc9429d
