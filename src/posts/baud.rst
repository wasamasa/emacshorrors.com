((title . "2400 Baud")
 (date . "2015-07-09 12:15:16"))

.. code:: elisp

    ;;! This has been commented out; I currently find the behavior when
    ;;! split-window-keep-point is nil disturbing, but if I can get used
    ;;! to it, then it would be better to eliminate the option.
    ;;! ;; Choose a good default value for split-window-keep-point.
    ;;! (setq split-window-keep-point (> baud-rate 2400))


``M-x magit-blame`` revealed that `this piece of code`_ has been kept
in its pristine state since 1992.  You'd expect Bauds_ not to matter
in this time and age, but guess what, both Vim and Emacs still have
code to do their terminal redisplay differently when you're using a
slow terminal.  Except that Emacs does sometimes check for the value
of ``baud-rate`` in high-level Emacs Lisp code...

.. _this piece of code: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/startup.el?id=0bec5a22cea517a15cf7eb5674094b94d1a7ead8#n826
.. _Bauds: https://en.wikipedia.org/wiki/Baud
