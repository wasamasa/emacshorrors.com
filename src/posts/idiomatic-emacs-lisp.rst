((title . "Idiomatic Emacs Lisp")
 (date . "2016-07-12 23:21:19 +0200"))

    <JordiGH> Strictly speaking, isn't "idiomatic lisp" whatever rms writes?

I'm afraid this is not the case.  See `this snippet`_.

.. code:: elisp

    ;;Function that handles term messages: code by rms (and you can see the
    ;;difference ;-) -mm

    (defun term-handle-ansi-terminal-messages (message)
      ;; Is there a command here?
      (while (string-match "\eAnSiT.+\n" message)
        ;; Extract the command code and the argument.
        (let* ((start (match-beginning 0))
               (command-code (aref message (+ start 6)))
               (argument
                (save-match-data
                  (substring message
                             (+ start 8)
                             (string-match "\r?\n" message
                                           (+ start 8)))))
               ignore)
          ;; Delete this command from MESSAGE.
          (setq message (replace-match "" t t message))

          ;; If we recognize the type of command, set the appropriate variable.
          (cond ((= command-code ?c)
                 (setq term-ansi-at-dir argument))
                ((= command-code ?h)
                 (setq term-ansi-at-host argument))
                ((= command-code ?u)
                 (setq term-ansi-at-user argument))
                ;; Otherwise ignore this one.
                (t
                 (setq ignore t)))

          ;; Update default-directory based on the changes this command made.
          (if ignore
              nil
            (setq default-directory
                  (file-name-as-directory
                   (if (and (string= term-ansi-at-host (system-name))
                                            (string= term-ansi-at-user (user-real-login-name)))
                       (expand-file-name term-ansi-at-dir)
                     (if (string= term-ansi-at-user (user-real-login-name))
                         (concat "/" term-ansi-at-host ":" term-ansi-at-dir)
                       (concat "/" term-ansi-at-user "@" term-ansi-at-host ":"
                               term-ansi-at-dir)))))

            ;; I'm not sure this is necessary,
            ;; but it's best to be on the safe side.
            (if (string= term-ansi-at-host (system-name))
                (progn
                  (setq ange-ftp-default-user term-ansi-at-save-user)
                  (setq ange-ftp-default-password term-ansi-at-save-pwd)
                  (setq ange-ftp-generate-anonymous-password term-ansi-at-save-anon))
              (setq term-ansi-at-save-user ange-ftp-default-user)
              (setq term-ansi-at-save-pwd ange-ftp-default-password)
              (setq term-ansi-at-save-anon ange-ftp-generate-anonymous-password)
              (setq ange-ftp-default-user nil)
              (setq ange-ftp-default-password nil)
              (setq ange-ftp-generate-anonymous-password nil)))))
      message)

This isn't bad code by any means, just clumsy and careful as opposed
to the highly compressed nature of the surrounding code.  The "I'm not
sure this is necessary, but it's best to be on the safe side." comment
reminds me of `The Daily WTF`_.

.. _this snippet: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/term.el?id=9c8c3a5478db6ff4b245e9128cbf24bd722ab1d6#n2631
.. _The Daily WTF: http://thedailywtf.com/
