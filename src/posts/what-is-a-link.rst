((title . "What Is A Link?")
 (date . "2015-10-16 10:56:56 +0200"))

I'll assume most of you will just roll your eyes at this title.  After
all, links are an essential part of the World Wide Web.  Not even
Emacs is safe from underlined text taking you elsewhere when clicking
it.  Except that sometimes an ordinary click won't do and will just
position point at the clicked position whereas a middle-click
activates the link.  To understand what exactly is going wrong there,
I'll go a bit more into depth than usual.

First of all, Emacs doesn't call clickable text links.  This is to be
expected as it predates a hefty amount of technology we're used to,
but can throw off beginners who don't know they'll want to use `the
button package`_.  The only problem being there that clickable text
produced this way will not obey the convention of an ordinary click
activating the link...

I've accidentally found the solution to this problem when staring at
the code of ``info.el``, wondering how it did achieve this feat of
making its references behave like the links I was used to:

.. code:: elisp

    (define-key map [follow-link] 'mouse-face)

It isn't unheard of for Emacs to know unusual symbols representing
keys on exotic keyboards, but ``follow-link`` is new to me.  Even more
so is putting a face into the definition part.  The manual doesn't
know of this being a valid value for keybindings either.  Unbinding
that definition did make normal clicks no longer work, so I did search
the sources to find `the following`_:

.. code:: elisp

    (defun mouse-on-link-p (pos)
      "..."
      (let ((action
             (and (or (not (consp pos))
                      mouse-1-click-in-non-selected-windows
                      (eq (selected-window) (posn-window pos)))
                  (or (mouse-posn-property pos 'follow-link)
                      (let ((area (posn-area pos)))
                        (when area
                          (key-binding (vector area 'follow-link) nil t pos)))
                      (key-binding [follow-link] nil t pos)))))
        (cond
         ((eq action 'mouse-face)
          (and (mouse-posn-property pos 'mouse-face) t))
         ((functionp action)
          ;; FIXME: This seems questionable if the click is not in a buffer.
          ;; Should we instead decide that `action' takes a `posn'?
          (if (consp pos)
              (with-current-buffer (window-buffer (posn-window pos))
                (funcall action (posn-point pos)))
            (funcall action pos)))
         (t action))))

Looks like this predicate checks whether there's a ``follow-link``
property or key binding at the position, then explicitly compares its
value with ``mouse-face``.  Mystery solved.  As for why they did it
that way, my hunch is on the mouse handling code being an incredible
mess and adding extra meaning to keymaps did allow them to easily
define alternative clicking behaviour for an entire mode.  As usual,
`the manual`_ explains the finer details.

.. _the button package: https://www.gnu.org/software/emacs/manual/html_node/elisp/Buttons.html
.. _the following: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/mouse.el?id=1c8ccba72f715a342b98d58a4063e42e5a1a1ab9#n706
.. _the manual: https://www.gnu.org/software/emacs/manual/html_node/elisp/Clickable-Text.html
