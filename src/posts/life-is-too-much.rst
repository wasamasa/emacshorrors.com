((title . "Life Is Too Much")
 (date . "2015-06-25 12:32:17 +0200"))

I tried figuring out once what the right function would be to quit
eshell_ programmatically.  Judging by the naming convention for other
commands going by ``eshell/ls`` and alike, I expected it to be
``eshell/quit``, but got a funky error instead.  Digging deeper
revealed `the real quit function`_:

.. code:: elisp

    (defun eshell-life-is-too-much ()
      "Kill the current buffer (or bury it).  Good-bye Eshell."
      (interactive)
      (if (not eshell-kill-on-exit)
          (bury-buffer)
        (kill-buffer (current-buffer))))

I have no idea what its author was thinking when devising that name.
They've refused a comment on it as well, so I'll just assume
``eshell`` to be sort of suicidal.

.. _eshell: https://www.gnu.org/software/emacs/manual/html_mono/eshell.html
.. _the real quit function: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/eshell/esh-mode.el?id=319eeeb0fb154a0cd1d36ec33c68029ff9d6c290#n912
