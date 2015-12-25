((title . "Avoiding the Issue")
 (date . "2015-12-25 22:17:21 +0100"))

Mailcap files are commonly used to tell your MUA how to open
attachments.  In Emacs this mechanism is used by Gnus, but outside of
it as well, like in Org's "Open exported file" export option.  I
wasn't amused about it picking ``gv`` of all the things to open PDF
files, so I've edited ``~/.mailcap`` and expected that to solve the
problem once and for all.  Except it didn't.

.. code:: elisp

    (defvar mailcap-parsed-p nil)

    (defun mailcap-parse-mailcaps (&optional path force)
      "Parse out all the mailcaps specified in a path string PATH.
    Components of PATH are separated by the `path-separator' character
    appropriate for this system.  If FORCE, re-parse even if already
    parsed.  If PATH is omitted, use the value of environment variable
    MAILCAPS if set; otherwise (on Unix) use the path from RFC 1524, plus
    /usr/local/etc/mailcap."
      (interactive (list nil t))
      (when (or (not mailcap-parsed-p)
                force)
        (cond
         (path nil)
         ((getenv "MAILCAPS") (setq path (getenv "MAILCAPS")))
         ((memq system-type mailcap-poor-system-types)
          (setq path '("~/.mailcap" "~/mail.cap" "~/etc/mail.cap")))
         (t (setq path
                  ;; This is per RFC 1524, specifically
                  ;; with /usr before /usr/local.
                  '("~/.mailcap" "/etc/mailcap" "/usr/etc/mailcap"
                    "/usr/local/etc/mailcap"))))
        (let ((fnames (reverse
                       (if (stringp path)
                           (split-string path path-separator t)
                         path)))
              fname)
          (while fnames
            (setq fname (car fnames))
            (if (and (file-readable-p fname)
                     (file-regular-p fname))
                (mailcap-parse-mailcap fname))
            (setq fnames (cdr fnames))))
          (setq mailcap-parsed-p t)))

Laaaaars_.  Why would you cache the parse result, but never ever
invalidate it?  It's not like it's hard_ or `not been done before`_.

Merry Grav-Mass_!

.. _Laaaaars: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/gnus/mailcap.el?id=608b15d59d08ff7054fdc5458d1d2ae54dd043e2#n384
.. _hard: http://martinfowler.com/bliki/TwoHardThings.html
.. _not been done before: http://emacshorrors.com/posts/unexpected-security-features.html
.. _Grav-Mass: https://www.stallman.org/grav-mass.html
