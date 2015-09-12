((title . "web-mode")
 (date . "2015-02-26 19:55:23 +0200"))

.. code:: elisp

    (defun web-mode-process-blocks (reg-beg reg-end func)
      (let ((i 0) (continue t) (block-beg reg-beg) (block-end nil))
        (while continue
          (setq block-end nil)
          (unless (get-text-property block-beg 'block-beg)
            (setq block-beg (web-mode-block-next-position block-beg)))
          (when (and block-beg (< block-beg reg-end))
            (setq block-end (web-mode-block-end-position block-beg)))
          (cond
           ((> (setq i (1+ i)) 2000)
            (message "process-blocks ** crazy loop (%S) **" (point))
            (setq continue nil))
           ((or (null block-end) (> block-end reg-end))
            (setq continue nil))
           (t
            (setq block-end (1+ block-end))
            (funcall func block-beg block-end)
            (setq block-beg block-end)
            ) ;t
           ) ;cond
          ) ;while
        ))

Fine_.  Some people haven't got the memo that closing parens feel
lonely unless packed together.  Some people like putting comments on
those to remind themselves what they belong to.  But crazy loops?
That's crazy talk!

.. code:: elisp

    (defun web-mode-unfontify-region (beg end)
    ;;  (message "unfontify: %S %S" beg end)
      )

You'd expect someone to figure out how to write a debug helper or
conditionals after writing over ten thousand lines of repetitive code
that looks like it has been mechanically translated from another
language.  Apparently other things have taken greater priority, at
least for `this particular snippet`_.

But it's OK. web-mode_ is our best hope at getting something
resembling a multi-mode for editing web-related code.  Why?  Well,
I'll just let `the code`_ speak for itself:

.. code:: elisp

    ;; =============================================================================
    ;; WEB-MODE is sponsored by Kernix: Great Digital Agency (Web & Mobile) in Paris
    ;; =============================================================================

I'm impressed.  Not only did someone convince their boss to sponsor
the development of a very much needed Emacs package, no, it's even
production-ready!  So, screw french comments [1]_, dead code [2]_ and
text property parser state [3]_, I'll continue using it for editing
the templates making up `this blog`_.

.. [1] Just search for "Ici" and "todo", you'll find a bunch.
.. [2] See the second code example for a less obvious case of this.
.. [3] Search for "put-text-property".  Every relevant parsing step
       stores the information gathered about a token found in the
       buffer in its text properties.  This is not only a horrible
       hack for manipulating the text you're editing, but leads to fun
       bugs such as syntax highlighting not working in org documents
       and copying of buffer text breaking the parser in more or less
       subtle ways.

.. _Fine: https://github.com/fxbois/web-mode/blob/6a22ac35d4d0d3b5c0ab3ff609f78a664dd88a49/web-mode.el#L2845-L2866
.. _this particular snippet: https://github.com/fxbois/web-mode/blob/6a22ac35d4d0d3b5c0ab3ff609f78a664dd88a49/web-mode.el#L4686-L4688
.. _web-mode: http://web-mode.org/
.. _the code: https://github.com/fxbois/web-mode/blob/6a22ac35d4d0d3b5c0ab3ff609f78a664dd88a49/web-mode.el#L16-L18
.. _this blog: https://github.com/wasamasa/microblog
