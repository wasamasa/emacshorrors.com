((title . "deactivate-mark")
 (date . "2016-06-18 17:54:59 +0200"))

This is a codeless post that will instead focus on a design issue
present in all (at the time of writing) stable releases of Emacs.  Be
assured that you will not have to work around it in the upcoming Emacs
25 release.

Have you ever wondered why some commands deactivate the region
afterwards, although there's no explicit call to the
``deactivate-mark`` function?  It turns out that this is intentional
behavior as can be seen in the documentation of the
``deactivate-mark`` variable:

.. code::

    If an editing command sets this to t, deactivate the mark afterward.
    The command loop sets this to nil before each command,
    and tests the value when the command returns.
    Buffer modification stores t in this variable.

So, any command modifying a buffer will deactivate the region.  Makes
sense and if you for some reason need the region again, it's a ``C-x
C-x`` away.  There is a major problem with this though, it doesn't
matter which buffer is modified...

`This bit me hard with eyebrowse`_.  I am using a modeline indicator
to visualize its state which is using the built-in ``format-spec``
package.  As that package is using a temporary buffer for turning a
format string into a formatted string and the modeline indicator is
recalculated *very* often, this led to the region being deactivated on
any command.  It took me quite a bit to figure this one out.  I
consider it madness for anyone to expect this behavior when writing
functions that should not interfere with the region, so I'm glad it
has been fixed in Emacs 25 by making the variable buffer-local.

.. _This bit me hard with eyebrowse: https://github.com/wasamasa/eyebrowse/issues/34
