((title . "Speed Matters")
 (date . "2015-12-19 23:58:28 +0100"))

Searching for a regex is one thing, but sometimes you may not even
need that and can get away with checking whether a specific regex
follows point with ``looking-at``.  This is a very powerful feature,
not unlike lookahead in parsing.  There's obviously ``looking-back``
as well which would allow you to look the other way, but the docs
recommend against using it.  Why?

It turns out ``looking-at`` is written in C, does a regex match and
then undoes some state changes.  ``looking-back`` however_...

.. code:: elisp

    (defun looking-back (regexp &optional limit greedy)
      "Return non-nil if text before point matches regular expression REGEXP.
    Like `looking-at' except matches before point, and is slower.
    LIMIT if non-nil speeds up the search by specifying a minimum
    starting position, to avoid checking matches that would start
    before LIMIT.

    If GREEDY is non-nil, extend the match backwards as far as
    possible, stopping when a single additional previous character
    cannot be part of a match for REGEXP.  When the match is
    extended, its starting position is allowed to occur before
    LIMIT.

    As a general recommendation, try to avoid using `looking-back'
    wherever possible, since it is slow."
      (declare
       (advertised-calling-convention (regexp limit &optional greedy) "25.1"))
      (let ((start (point))
            (pos
             (save-excursion
               (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                    (point)))))
        (if (and greedy pos)
            (save-restriction
              (narrow-to-region (point-min) start)
              (while (and (> pos (point-min))
                          (save-excursion
                            (goto-char pos)
                            (backward-char 1)
                            (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
                (setq pos (1- pos)))
              (save-excursion
                (goto-char pos)
                (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
        (not (null pos))))

It's no wonder that you get `weird warnings`_ suggesting you to use
the ``LIMIT`` argument to mitigate a failing lookup for an anchor to
do the matching against.  I doubt most people encountering this
understand it though and will just put ``(point-min)`` there once it
becomes mandatory instead of reconsidering a meaningful value or
rewriting the code to not use ``looking-back``...

.. _however: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/subr.el?id=138480a97bfc1104143b5fc10dfc962b95b78ae8#n3524
.. _weird warnings: https://github.com/quelpa/quelpa/pull/94#issuecomment-152658320
