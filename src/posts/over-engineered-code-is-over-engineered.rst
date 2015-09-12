((title . "Over-engineered code is over-engineered")
 (date . "2015-08-10 22:20:26 +0200"))

Lars_ is one of the few `mononymous people`_ in the Emacs universe.
His greatest contribution to Emacs would be Gnus_, a 100k SLOC heavy
NNTP_ reader people commonly customize to read Email.  I found `the
following`_ after taking a quick look at ``gnus.el``:

.. code:: elisp

    (defun gnus-interactive (string &optional params)
      "Return a list that can be fed to `interactive'.
    See `interactive' for full documentation.

    Adds the following specs:

    y -- The current symbolic prefix.
    Y -- A list of the current symbolic prefix(es).
    A -- Article number.
    H -- Article header.
    g -- Group name."
      (let ((i 0)
            out c prompt)
        (while (< i (length string))
          (string-match ".\\([^\n]*\\)\n?" string i)
          (setq c (aref string i))
          (when (match-end 1)
            (setq prompt (match-string 1 string)))
          (setq i (match-end 0))
          ;; We basically emulate just about everything that
          ;; `interactive' does, but add the specs listed above.
          (push
           (cond
            ((= c ?a)
             (completing-read prompt obarray 'fboundp t))
            ((= c ?b)
             (read-buffer prompt (current-buffer) t))
            ((= c ?B)
             (read-buffer prompt (other-buffer (current-buffer))))
            ((= c ?c)
             (read-char))
            ((= c ?C)
             (completing-read prompt obarray 'commandp t))
            ((= c ?d)
             (point))
            ((= c ?D)
             (read-directory-name prompt nil default-directory 'lambda))
            ((= c ?f)
             (read-file-name prompt nil nil 'lambda))
            ((= c ?F)
             (read-file-name prompt))
            ((= c ?k)
             (read-key-sequence prompt))
            ((= c ?K)
             (error "Not implemented spec"))
            ((= c ?e)
             (error "Not implemented spec"))
            ((= c ?m)
             (mark))
            ((= c ?N)
             (error "Not implemented spec"))
            ((= c ?n)
             (string-to-number (read-from-minibuffer prompt)))
            ((= c ?p)
             (prefix-numeric-value current-prefix-arg))
            ((= c ?P)
             current-prefix-arg)
            ((= c ?r)
             'gnus-prefix-nil)
            ((= c ?s)
             (read-string prompt))
            ((= c ?S)
             (intern (read-string prompt)))
            ((= c ?v)
             (read-variable prompt))
            ((= c ?x)
             (read-minibuffer prompt))
            ((= c ?x)
             (eval-minibuffer prompt))
            ;; And here the new specs come.
            ((= c ?y)
             gnus-current-prefix-symbol)
            ((= c ?Y)
             gnus-current-prefix-symbols)
            ((= c ?g)
             (gnus-group-group-name))
            ((= c ?A)
             (gnus-summary-skip-intangible)
             (or (get-text-property (point) 'gnus-number)
                 (gnus-summary-last-subject)))
            ((= c ?H)
             (gnus-data-header (gnus-data-find (gnus-summary-article-number))))
            (t
             (error "Non-implemented spec")))
           out)
          (cond
           ((= c ?r)
            (push (if (< (point) (mark)) (point) (mark)) out)
            (push (if (> (point) (mark)) (point) (mark)) out))))
        (setq out (delq 'gnus-prefix-nil out))
        (nreverse out)))

First of all, a bit of background.  Emacs Lisp knows functions, like
many other programming languages.  However, commands in Emacs often
allow arguments to be supplied, be it by a prefix argument or reading
in extra data.  This is achieved by adding an ``interactive``
specification after its docstring; ``command-execute`` or
``call-interactively`` will then fetch its ``interactive``
specification via ``interactive-form`` and execute the respective C
code to fill in the unspecified function arguments.  To allow for
non-standard ways of providing arguments, a form returning a list of
the arguments can be used instead of a string.

So, what's Gnus doing?  Instead of using said alternative form, it
reinvents the majority of ``call-interactively``, leaves out a few
supported codes, adds a few of its own, alters the behaviour of the
``r`` code, then spits out a list.

Is this code actually used?  Well, if ``ag`` is to be trusted, there's
exactly one variant of ``gnus-interactively`` sprinkled all over its
code, ``(interactive (gnus-interactive "P\ny"))``.  This could be
replaced by a function returning a list containing
``current-prefix-arg`` and whatever the relevant code of
``gnus-symbolic-argument`` for its `symbolic prefixes`_ feature is,
but would that have been as fun as this nonsensical exercise in NIH_?
Surely not!

.. _Lars: http://lars.ingebrigtsen.no/about/
.. _mononymous people: https://en.wikipedia.org/wiki/Mononymous_person
.. _Gnus: http://www.gnus.org/
.. _NNTP: https://en.wikipedia.org/wiki/Network_News_Transfer_Protocol
.. _the following: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/gnus/gnus.el?id=59676ae9e9e0dd1a094f0a928582f6c2a495864f#n3281
.. _symbolic prefixes: http://www.gnus.org/manual/gnus_113.html#Symbolic-Prefixes
.. _NIH: https://en.wikipedia.org/wiki/Not_invented_here
