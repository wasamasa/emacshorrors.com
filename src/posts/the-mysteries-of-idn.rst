((title . "The Mysteries of IDN")
 (date . "2016-04-17 18:43:05 +0200"))

Chances are that you've taken a glance at the colourful output of
``htop`` before and wondered why exactly your Emacs process has
spawned a persistent ``idn`` child process.  In this post I'll attempt
showing how I've found out about the reasons for this mystery and what
it tells us about Emacs.

First of all, let's search the Emacs sources for ``"idn``.  This
yields `a hit`_ in ``message.el``:

.. code:: elisp

    (defcustom message-use-idna
      (and (or (mm-coding-system-p 'utf-8)
               (condition-case nil
                   (let (mucs-ignore-version-incompatibilities)
                     (require 'un-define))
                 (error)))
           (condition-case nil
               (require 'idna)
             (file-error)
             (invalid-operation))
           idna-program
           (executable-find idna-program)
           (string= (idna-to-ascii "räksmörgås") "xn--rksmrgs-5wao1o")
           t)
      "Whether to encode non-ASCII in domain names into ASCII according to IDNA.
    GNU Libidn, and in particular the elisp package \"idna.el\" and
    the external program \"idn\", must be installed for this
    functionality to work."
      :version "22.1"
      :group 'message-headers
      :link '(custom-manual "(message)IDNA")
      :type '(choice (const :tag "Ask" ask)
                     (const :tag "Never" nil)
                     (const :tag "Always" t)))

This is interesting.  ``idna.el`` is provided by the ``idn`` program
here, so this file will only be available if the ``site-lisp``
directory has been loaded up before in this session.  It explains why
I couldn't reproduce this behaviour when using ``emacs -Q`` as this
disables loading that directory.  It's very likely that you'll have it
installed on your system as it is a dependency to ``curl``, ``wget``,
``systemd``, ``mutt`` and more.  And due to the way ``idna.el`` works,
using ``idna-to-ascii`` in this customizable will start a process and
keep it around until one calls ``idna-shutdown``.  So, never.  Unless
you've customized it before to prevent it from being used in the first
place.  This is hardly the only case of non-trivial customization
variables doing unexpected things to detect optional functionality,
but it doesn't appear to attract any attention unless it `worsens your
init file startup time`_...

Now, what exactly in my setup would cause ``message.el`` to be loaded?
After all, I'm not using any Email client inside Emacs, so I had to
bisect my init file to figure out that head scratcher.  It turned out
that magit_ was the culprit.  If you're wondering what part of it
would need *that*, it avoids loading it initially, but declares
``message-goto-body`` for usage in ``magit-remote.el`` to do pull
requests.  You know, real pull requests, not the GitHub thing.

.. _a hit: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/gnus/message.el?id=3de30674d7aa79c9f366f65c9f30aec2c8963b54#n1759
.. _worsens your init file startup time: https://github.com/emacs-helm/helm/issues/1000
.. _magit: https://magit.vc/
