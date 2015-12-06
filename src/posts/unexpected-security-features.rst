((title . "Unexpected Security Features")
 (date . "2015-12-06 22:57:43 +0100"))

Like any good ``#emacs`` denizen, I did report `a bug`_ after
submitting `my last post`_, though for a different reason.  I've
actually tried out using ``netrc.el`` and found out it couldn't handle
my Heroku_ credentials, simply because they were spread over multiple
lines.

It turns out there's four(!) official implementations of a task as
niche as parsing a ``.netrc``/``.authinfo`` file.  The remaining three
ones can be found in ``auth-source.el``, ``tramp.el`` and
``ange-ftp.el``.  I've summarized my experimentation so far in a
table:

================== ========== ============== ======
Location           Multiline? Obfuscated PW? Rating
================== ========== ============== ======
``netrc.el``       No         Sometimes      10/10
------------------ ---------- -------------- ------
``auth-source.el`` Yes        Yes            2/10
------------------ ---------- -------------- ------
``tramp.el``       No         No need        0/10
------------------ ---------- -------------- ------
``ange-ftp.el``    Yes        No             4/10
================== ========== ============== ======

A few notes on my findings: ``auth-source.el`` is the most Emacs-like
implementation.  It does implement additional features not present in
other parsers for this format (including quoting, other tokens, etc.),
but the docstrings are incomprehensible and I don't seem to get
anything returned from a plain-text ``.authinfo``.  Yet it's the
recommended way to go, so I dunno really.

TRAMP_ coming with a ``.netrc`` parser is unexpected, but I figured
I'd give it a try.  Unfortunately it is immediately disqualified for
just returning machine-user pairs.  Boo.

``ange-ftp.el`` is the closest thing to a TRAMP precursor that
specializes on FTP exclusively.  As ``.netrc`` is a phenomenon
belonging to FTP, it's no wonder it can parse the file as well.  The
implementation doesn't seem to be doing anything special, but the form
of the parse result is pretty weird.  It's a hash table where machine
and user are munged together and the password is a propertized string
with the ``fontified`` property set to ``nil``.  So much about
reusability.

-----

Back to the scary part of this post. ``auth-source.el`` does
`significantly better`_ at obfuscating passwords than ``netrc.el``:

.. code:: elisp

    ;; cache all netrc files (used to be just .gpg files)
    ;; Store the contents of the file heavily encrypted in memory.
    ;; (note for the irony-impaired: they are just obfuscated)
    (auth-source--aput
     auth-source-netrc-cache file
     (list :mtime (nth 5 (file-attributes file))
           :secret (lexical-let ((v (mapcar #'1+ (buffer-string))))
                     (lambda () (apply #'string (mapcar #'1- v))))))

I'm not speaking of shifting each character of the buffer by one char
and back, oh no.  It's the part about turning the result into a
closure unobfuscating it once it's called again.  That way not even
viewing this value will print out something potentially sensitive.
Finally there's something better for demonstrating why closures are
useful than `counters and stuff`_.  The reason a normal lambda won't
do is because the lexical scoping necessary to create closures was
only introduced as of Emacs 24.3, so ``lexical-let`` is used instead
which fakes them.

.. _a bug: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=22081
.. _my last post: http://emacshorrors.com/posts/heavily-encrypted.html
.. _Heroku: https://www.heroku.com/
.. _TRAMP: https://www.gnu.org/software/tramp/
.. _significantly better: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/gnus/auth-source.el?id=8f725564b6b546c9660551327cecb97c5a362e13#n1003
.. _counters and stuff: http://www.letoverlambda.com/index.cl/guest/chap2.html
