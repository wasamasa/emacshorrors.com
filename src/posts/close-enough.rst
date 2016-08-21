((title . "Close Enough")
 (date . "2016-08-21 19:42:02 +0200"))

``byte-opt.el`` is one of those files Jamie Zawinski laid his golden
hands on.  It seems that back in the days, there wasn't much of a
concern about Emacs Lisp execution speed until he got annoyed enough
to bolt on an optimizer.  Its sources start with a wonderful quote:

    "No matter how hard you try, you can't make a racehorse out of a pig.
    You can, however, make a faster pig."

I recommend reading it to get an idea what compiler jargon like
"peephole optimizer" could possibly mean.  During my last study, I
found `this curious piece of code`_:

.. code:: elisp

    (defun byte-optimize-approx-equal (x y)
      (<= (* (abs (- x y)) 100) (abs (+ x y))))

So, according to this 99 and 100 are equal.  Awesome!

.. _this curious piece of code: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/emacs-lisp/byte-opt.el?id=14a86f837762af8d16eef57c315da93b56699901#n710
