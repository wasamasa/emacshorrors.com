((title . "Unjustified Indirection")
 (date . "2017-10-19 23:44:23 +0200"))

I finally made `that EPUB mode`_.  This adventure mostly taught me
that eww, or rather, shr.el isn't quite reusable.  That itself is not
really a problem, but I `handed in a patch`_ to improve the situation.
An old saying among programmers is that every problem can be solved by
applying an extra level of indirection, so that's what I did after
discussing it out on the bug tracker, however after my patch got
merged it was deemed `too much`_:

.. code:: elisp

    ;; We don't use shr-indirect-call here, since shr-descend is
    ;; the central bit of shr.el, and should be as fast as
    ;; possible.  Having one more level of indirection with its
    ;; negative effect on performance is deemed unjustified in
    ;; this case.

Hadn't I spoken up about inclusion of this comment, an unsuspecting
future hacker wouldn't even know why there's duplicated code not using
the helper.  I can only wonder how production-ready browser engines
solve this kind of problem...

.. _that EPUB mode: https://github.com/wasamasa/nov.el
.. _handed in a patch: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28402
.. _too much: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/net/shr.el?id=658853aebb0ae2ee243276e04a7672fa7525ec5c#n505
