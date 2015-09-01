((title . "Where Is My Mind?")
 (date . "2015-09-01 22:44:03"))

I've recently evaluated how I'd go at adding a TOC to my init.org_.
Fortunately, toc-org_ is a thing.  If only things were as
straight-forward as using a package and not immediately running into
bugs...

One of the problems I've encountered was that whenever I wrote changes
to disk, the TOC header became expanded, no matter what its previous
folding state was.  This thing happens because hiding is implemented
by putting an overlay with the ``invisible`` text property over the
text, deleting that text made that overlay disappear and any newly
inserted text was no longer hidden.  So I've gone for the seemingly
clever fix of remembering the folding state and cycling the header
twice to ensure that the previous state was restored.

After reinventing code for determining the folding state of a heading
(yes, Org doesn't have abstractions for interacting with it
programmatically except for its exporters) I've stumbled over two
rather surprising interactions, the first one being that even with all
of my hacks applied, I'd still get a rather annoying scrolling change
after saving.  This_ is Org's ``org-cycle-hook`` running
``org-optimize-window-after-visibility-change``:

.. code:: elisp

    (defun org-optimize-window-after-visibility-change (state)
      "Adjust the window after a change in outline visibility.
    This function is the default value of the hook `org-cycle-hook'."
      (when (get-buffer-window (current-buffer))
        (cond
         ((eq state 'content)  nil)
         ((eq state 'all)      nil)
         ((eq state 'folded)   nil)
         ((eq state 'children) (or (org-subtree-end-visible-p) (recenter 1)))
         ((eq state 'subtree)  (or (org-subtree-end-visible-p) (recenter 1))))))

The other problem wasn't as easy to spot.  For some reason the tests I
wrote for checking the folding state after cycling repeatedly did
succeed when run non-interactively, but failed in ``M-x ert``.
``org-cycle`` dispatches to a number of things depending on context
and how it was called, but in my case it is always calling
``org-cycle-internal-local``.  Here_'s the snippet exhibiting the
broken behaviour:

.. code:: elisp

    ((or children-skipped
         (and (eq last-command this-command)
              (eq org-cycle-subtree-status 'children)))
     ;; We just showed the children, or no children are there,
     ;; now show everything.
     (unless (org-before-first-heading-p)
       (run-hook-with-args 'org-pre-cycle-hook 'subtree))
     (outline-flag-region eoh eos nil)
     (org-unlogged-message
      (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
     (setq org-cycle-subtree-status 'subtree)
     (unless (org-before-first-heading-p)
       (run-hook-with-args 'org-cycle-hook 'subtree)))

See the problem?  For yet unknown reasons, not only is it necessary
that the direct children were displayed, no, the last command has to
be the same as the currently executed command.  Executing
``org-cycle`` in an interactive session by hitting ``TAB`` repeatedly
will satisfy this condition, executing a different command in-between
won't.  So, hitting ``TAB``, moving back and forth, then hitting
``TAB`` again will just close the subtree, same goes for ``M-x ert``.
I'm not sure why, but I couldn't ever get batch mode to return
something meaningful for either of ``last-command`` or
``this-command``, so it passes this test with flying colours.

The workaround is as ugly as the implementation:

.. code:: elisp

    (let ((last-command this-command))
      ...)

.. _init.org: https://github.com/wasamasa/dotemacs/blob/master/init.org
.. _toc-org: https://github.com/snosov1/toc-org
.. _This: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/org/org.el?id=1a3518e7c361a9ceaa017c1334a83d14e0651a4e#n6954
.. _Here: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/org/org.el?id=1a3518e7c361a9ceaa017c1334a83d14e0651a4e#n6824
