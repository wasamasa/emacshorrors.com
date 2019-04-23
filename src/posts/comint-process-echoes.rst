((title . "comint-process-echoes")
 (date . "2019-04-23 22:09:46 +0200"))

Originally I planned to blog about a fun hack, porting the infamous
`cloud-to-butt`_ browser extension to Emacs.  The idea was that whenever
you interact with subprocesses instances of "cloud" would be replaced
with "butt", I picked ``shell.el`` for ease of hacking [1]_.  The
following snippet is loosely modeled after
``ansi-color-process-output``, so pardon any weirdness.

.. code:: elisp

    (defun my-filter-shell-output (string)
      (let ((start-marker comint-last-output-start)
            (end-marker (process-mark (get-buffer-process (current-buffer)))))
        (save-excursion
          (goto-char start-marker)
          (while (search-forward "cloud" end-marker t)
            (replace-match "butt")))))

    (with-eval-after-load 'shell
      (add-hook 'comint-output-filter-functions 'my-filter-shell-output t))

The API is somewhat murky.  A comint output filter function receives a
string argument and is expected to modify the buffer.  There's no
documentation on how to retrieve the positions of the last output, so
I did whatever aforementioned exemplary function does and restrict the
search and replace operations to two markers.  How could this possibly
go wrong?  See for yourself in the following test session:

.. code:: shell-session

    [wasa@box ~]$ echo cloud
    echo butt
    butt
    [wasa@box ~]$ echo butt
    butt
    [wasa@box ~]$ echo ponies
    ponies

Something is definitely wrong here, an extra line is printed if and
only if the replacement would have happened.  Most curiously, it
doesn't mirror the user input, but has the replacement as well.  After
debugging this a bit [2]_ I remembered that long time ago I've set
``comint-process-echoes`` because ``M-x shell`` kept printing the user
input after sending it to the shell.  Time to gaze into the abyss:

.. code:: elisp

    ;; Optionally delete echoed input (after checking it).
    (when (and comint-process-echoes (not artificial))
      (let ((echo-len (- comint-last-input-end
                         comint-last-input-start)))
        ;; Wait for all input to be echoed:
        (while (and (> (+ comint-last-input-end echo-len)
                       (point-max))
                    (accept-process-output proc)
                    (zerop
                     (compare-buffer-substrings
                      nil comint-last-input-start
                      (- (point-max) echo-len)
                      ;; Above difference is equivalent to
                      ;; (+ comint-last-input-start
                      ;;    (- (point-max) comint-last-input-end))
                      nil comint-last-input-end (point-max)))))
        (if (and
             (<= (+ comint-last-input-end echo-len)
                 (point-max))
             (zerop
              (compare-buffer-substrings
               nil comint-last-input-start comint-last-input-end
               nil comint-last-input-end
               (+ comint-last-input-end echo-len))))
            ;; Certain parts of the text to be deleted may have
            ;; been mistaken for prompts.  We have to prevent
            ;; problems when `comint-prompt-read-only' is non-nil.
            (let ((inhibit-read-only t))
              (delete-region comint-last-input-end
                             (+ comint-last-input-end echo-len))
              (when comint-prompt-read-only
                (save-excursion
                  (goto-char comint-last-input-end)
                  (comint-update-fence)))))))

Echoes are canceled by adhering to the following procedure:

- Waiting for process output until enough characters have been emitted
- Comparing the emitted text with the last user input
- Only if they match that echoed text is deleted
- A hack is applied to not delete the prompt

Unfortunately my output filter is run before that, so it makes the
last check fail.  I can only wonder whether it's even possible to
use this API meaningfully and whether it will involve breaking
changes.  Yet everyone and their dog keep proclaiming loudly how great
Emacs and its approach to text processing are...

.. _cloud-to-butt: https://github.com/panicsteve/cloud-to-butt
.. [1] ``term.el`` is out because it doesn't offer anything that
       deserves to be called an API, ``eshell.el`` doesn't even have
       documentation and is huge, ``shell.el`` is small and simple.
.. [2] I recommend adding a ``(sit-for 1)`` between functions doing
       buffer manipulation to visualize what's going on in the
       buffer.  Note that edebug supports doing this for everything
       by switching to ``edebug-trace-mode``.
