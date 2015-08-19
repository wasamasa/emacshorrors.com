((title . "Yes or No?")
 (date . "2015-02-23 20:51:15"))

Sometimes an Emacs command isn't sure what to do next.  Any person who
went beyond the tutorial will instantly recognize the ``(yes or no)``
prompt commonly associated with potentially destructive actions, such
as saving files.

I happen to work at a place where the majority of programmers prefer
using Emacs and even manage to convince new employees to give that
hell of a text editor a try.  One day one of my colleagues asked me
for help because they couldn't understand why clicking a button when
installing a package replaced the familiar ``(yes or no)`` prompt with
a modal dialog for this and every other action.

A cursory look at the sources of the yes-or-no-p_ function revealed
what was going on:

.. code:: c

    DEFUN ("yes-or-no-p", Fyes_or_no_p, Syes_or_no_p, 1, 1, 0,
           doc: /* Ask user a yes-or-no question.
    Return t if answer is yes, and nil if the answer is no.
    PROMPT is the string to display to ask the question.  It should end in
    a space; `yes-or-no-p' adds \"(yes or no) \" to it.

    The user must confirm the answer with RET, and can edit it until it
    has been confirmed.

    If dialog boxes are supported, a dialog box will be used
    if `last-nonmenu-event' is nil, and `use-dialog-box' is non-nil.  */)

Everything just went according to plan.

.. image:: /static/keikaku.jpg

Fortunately this quirk can be disabled:

.. code:: elisp

    (setq use-dialog-box nil)

After explaining what happened to my bewildered colleague, I felt like
dismissing this whimsy behavior for just feeling like an interface
improvement that got tacked on as afterthought, however it turned out
to be `far more dangerous than that`_ for users of `a popular
commercial Unix operating system`_.

Not even the shortened `y-or-n-p`_ variant is safe!

.. code:: elisp

    ;; ¡Beware! when I tried to edebug this code, Emacs got into a weird state
    ;; where all the keys were unbound (i.e. it somehow got triggered
    ;; within read-key, apparently).  I had to kill it.


But it's more featureful than plain old ``yes-or-no-p`` [1]_, so I'm
fine with someone leaving a most certainly obsolete bug report in its
sources instead of just using the bug tracker.

.. [1] Apparently most of its code was written to allow one to scroll
       around in the buffer from where the prompt was invoked without
       leaving it, a feature shared with the ``query-replace``
       command.

.. _yes-or-no-p: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/fns.c?id=3ebf06300b8186feac5e9b436ca263dc908ed886#n2670
.. _far more dangerous than that: http://debbugs.gnu.org/db/17/17638.html
.. _a popular commercial Unix operating system: https://www.apple.com/osx/
.. _y-or-n-p: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/subr.el?id=19ee7875db8b154a3ba49a98da2d3c24b03fff1e#n2240
