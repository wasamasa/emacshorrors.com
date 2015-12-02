((title . "Heavily Encrypted")
 (date . "2015-12-02 20:18:59 +0100"))

.. code:: elisp

    (if (and netrc-cache
             (equal (car netrc-cache) (nth 5 (file-attributes file))))
        (insert (base64-decode-string (rot13-string (cdr netrc-cache))))
      (insert-file-contents file)
      (when (string-match "\\.gpg\\'" file)
        ;; Store the contents of the file heavily encrypted in memory.
        (setq netrc-cache (cons (nth 5 (file-attributes file))
                                (rot13-string
                                 (base64-encode-string
                                  (buffer-string)))))))

Whenever I see `such code`_, a number of questions comes to my mind:

- Is this a joke?
- Why is this "encryption" only applied to GPG-encrypted file
  contents?
- Why is this caching only applied to GPG-encrypted file contents?
- Which scenario does this apply to?  I mean, scrambling a buffer
  representation doesn't make any sense on my Linux box, simply
  because root access is necessary for accessing other process
  memory.  Windows systems have more grave issues than any process
  being able to read the memory of other processes.
- Who the hell wrote this?

At least the last one is easy to answer.  Lars splitted this one out
from Gnus, our favourite Emacs mail program.  It hardly is alone in
this department, I'm aware of Thunderbird_ storing permanently cached
credentials properly encrypted, but leaving the key in a
base64-encoded file.

.. _such code: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/net/netrc.el?id=5874cd46e9cd8b20c61190ad56055d73816c2303#n65
.. _Thunderbird: https://github.com/mozilla-services/services-central-legacy/blob/master/security/nss/cmd/pwdecrypt/pwdecrypt.c
