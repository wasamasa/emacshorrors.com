((title . "The World")
 (date . "2015-03-13 11:18:11 +0200"))

Sometimes we forget that despite what observation of the emacs-devel_
mailing list might suggest [1]_, Emacs core contributors are aware
that there is something outside the ecosystem controlled by GNU.

I've found the most apparent proof for this fact when strolling
through tramp-sh.el_ for debugging the
``tramp-open-connection-setup-interactive-shell`` function.

.. code:: elisp

    ;; IRIX64 bash expands "!" even when in single quotes.  This
    ;; destroys our shell functions, we must disable it.  See
    ;; <http://stackoverflow.com/questions/3291692/irix-bash-shell-expands-expression-in-single-quotes-yet-shouldnt>.

How unexpected to not only see a fair amount of commentary explaining
the probing done, but even a stackoverflow.com_ URL backing up one of
the applied hacks.  I personally wouldn't have bothered as IRIX_ is an
obsolete operating system and support for it will be removed in a
future Emacs Release [2]_, however the TRAMP_ developers either
haven't got the memo yet or think otherwise (which is understandable,
while they may not *want* to use obsolete operating systems, they
might *need* to).

Another proof for my heretic theory can be found in subr.el_ in the
sources of ``shell-quote-argument``, the gate keeper of everything that
wants to interact with shells in an orderly manner:

.. code:: elisp

    ;; First, quote argument so that CommandLineToArgvW will
    ;; understand it.  See
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft%28v=vs.85%29.aspx
    ;; After we perform that level of quoting, escape shell
    ;; metacharacters so that cmd won't mangle our argument.  If the
    ;; argument contains no double quote characters, we can just
    ;; surround it with double quotes.  Otherwise, we need to prefix
    ;; each shell metacharacter with a caret.

MSDN_ for Windows references?  Very good!

.. [1] See the `LLDB support discussion`_ which is based on the
       assumption that inclusion of such code could only happen in
       Emacs and the GNU ELPA package repository.
.. [2] See the 25.1 NEWS_

.. _emacs-devel: http://lists.gnu.org/archive/html/emacs-devel/
.. _tramp-sh.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/net/tramp-sh.el?id=b91eafe31a524b391d5cec079cf8f36c2f9d5f30#n4109
.. _stackoverflow.com: http://stackoverflow.com/
.. _IRIX: https://en.wikipedia.org/wiki/IRIX
.. _TRAMP: https://www.gnu.org/software/tramp/
.. _subr.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/subr.el?id=b91eafe31a524b391d5cec079cf8f36c2f9d5f30#n2663
.. _MSDN: https://msdn.microsoft.com/en-US/
.. _LLDB support discussion: http://lists.gnu.org/archive/html/emacs-devel/2015-02/msg00360.html
.. _NEWS: http://git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS?id=b91eafe31a524b391d5cec079cf8f36c2f9d5f30#n32
