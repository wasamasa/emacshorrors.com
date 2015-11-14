((title . "newline")
 (date . "2015-11-14 23:34:35 +0100"))

Adding text support to zone-nyan_ was relatively simple after figuring
out a suitable representation.  To render the data, I iterate over a
two-dimensional array, look up the current cell in the color mapping,
then insert an appropriate representation of it.  It was tempting to
use ``(newline)`` after each line, but that works differently from the
seemingly more bothersome ``(insert "\n")``.  A look at `the sources`_
reveals why:

.. code:: elisp

    (defun newline (&optional arg interactive)
      "Insert a newline, and move to left margin of the new line if it's blank.
    If option `use-hard-newlines' is non-nil, the newline is marked with the
    text-property `hard'.
    With ARG, insert that many newlines.

    If `electric-indent-mode' is enabled, this indents the final new line
    that it adds, and reindents the preceding line.  To just insert
    a newline, use \\[electric-indent-just-newline].

    Calls `auto-fill-function' if the current column number is greater
    than the value of `fill-column' and ARG is nil.
    A non-nil INTERACTIVE argument means to run the `post-self-insert-hook'."
      (interactive "*P\np")
      (barf-if-buffer-read-only)
      ;; Call self-insert so that auto-fill, abbrev expansion etc. happens.
      ;; Set last-command-event to tell self-insert what to insert.
      (let* ((was-page-start (and (bolp) (looking-at page-delimiter)))
             (beforepos (point))
             (last-command-event ?\n)
             ;; Don't auto-fill if we have a numeric argument.
             (auto-fill-function (if arg nil auto-fill-function))
             (postproc
              ;; Do the rest in post-self-insert-hook, because we want to do it
              ;; *before* other functions on that hook.
              (lambda ()
                (cl-assert (eq ?\n (char-before)))
                ;; Mark the newline(s) `hard'.
                (if use-hard-newlines
                    (set-hard-newline-properties
                     (- (point) (prefix-numeric-value arg)) (point)))
                ;; If the newline leaves the previous line blank, and we
                ;; have a left margin, delete that from the blank line.
                (save-excursion
                  (goto-char beforepos)
                  (beginning-of-line)
                  (and (looking-at "[ \t]$")
                       (> (current-left-margin) 0)
                       (delete-region (point)
                                      (line-end-position))))
                ;; Indent the line after the newline, except in one case:
                ;; when we added the newline at the beginning of a line which
                ;; starts a page.
                (or was-page-start
                    (move-to-left-margin nil t)))))
        (unwind-protect
            (if (not interactive)
            ;; FIXME: For non-interactive uses, many calls actually just want
            ;; (insert "\n"), so maybe we should do just that, so as to avoid
            ;; the risk of filling or running abbrevs unexpectedly.
            (let ((post-self-insert-hook (list postproc)))
              (self-insert-command (prefix-numeric-value arg)))
          (unwind-protect
              (progn
                (add-hook 'post-self-insert-hook postproc nil t)
                (self-insert-command (prefix-numeric-value arg)))
            ;; We first used let-binding to protect the hook, but that was naive
            ;; since add-hook affects the symbol-default value of the variable,
            ;; whereas the let-binding might only protect the buffer-local value.
            (remove-hook 'post-self-insert-hook postproc t)))
          (cl-assert (not (member postproc post-self-insert-hook)))
          (cl-assert (not (member postproc (default-value 'post-self-insert-hook))))))
      nil)

Even assuming that the FIXME gets dealt with, you should resist the
urge and use ``(insert "\n")`` for programmatically added newlines.

.. _zone-nyan: https://github.com/wasamasa/zone-nyan
.. _the sources: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/simple.el?id=dcd5877a76557f4ce08bebee0d8919ad951a9f13#n389
