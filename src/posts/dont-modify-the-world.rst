((title . "Don't Modify the World")
 (date . "2015-07-14 11:21:15 +0200"))

Emacs comes with `a few thousand lines of code for calendar
calculations`_.  Surely timezone calculations must be a properly
solved problem as well!  At least, that's what one would expect given
the existence of the very handy ``M-x display-time-world``. Except,
`it's not`_:

.. code:: elisp

    (defun display-time-world-display (alist)
      "Replace current buffer text with times in various zones, based on ALIST."
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (old-tz (getenv "TZ"))
            (max-width 0)
            result fmt)
        (erase-buffer)
        (unwind-protect
            (dolist (zone alist)
              (let* ((label (cadr zone))
                     (width (string-width label)))
                (setenv "TZ" (car zone))
                (push (cons label
                            (format-time-string display-time-world-time-format))
                      result)
                (when (> width max-width)
                  (setq max-width width))))
          (setenv "TZ" old-tz))
        (setq fmt (concat "%-" (int-to-string max-width) "s %s\n"))
        (dolist (timedata (nreverse result))
          (insert (format fmt (car timedata) (cdr timedata))))
        (delete-char -1)))

Tough luck if all you've done was `customizing Emacs' current idea of
the timezone`_ like wgreenhouse_ did.  Ouch.

edit: Looks like this got fixed properly!

.. _a few thousand lines of code for calendar calculations: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/calendar
.. _it's not: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/time.el?id=6a7e718916d00dbacaa765669f389b86f33075f5#n522
.. _customizing Emacs' current idea of the timezone: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21020
.. _wgreenhouse: https://github.com/wgreenhouse
