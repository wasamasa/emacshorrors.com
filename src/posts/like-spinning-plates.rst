((title . "Like Spinning Plates")
 (date . "2016-02-04 22:13:49 +0100"))

The tried and true way of doing network requests is sending an
asynchronous one with a callback to be used upon completion or
failure.  That's why we have ``url-retrieve`` and
``url-retrieve-synchronously`` instead of ``url-retrieve`` and
``url-retrieve-asynchronously``.  It's fairly obvious how the
asynchronous variant is implemented [1]_, the synchronous one less so.

.. code:: elisp

    (defun url-retrieve-synchronously (url &optional silent inhibit-cookies)
      "Retrieve URL synchronously.
    Return the buffer containing the data, or nil if there are no data
    associated with it (the case for dired, info, or mailto URLs that need
    no further processing).  URL is either a string or a parsed URL."
      (url-do-setup)

      (let ((retrieval-done nil)
            (asynch-buffer nil))
        (setq asynch-buffer
              (url-retrieve url (lambda (&rest ignored)
                                  (url-debug 'retrieval "Synchronous fetching done (%S)" (current-buffer))
                                  (setq retrieval-done t
                                        asynch-buffer (current-buffer)))
                            nil silent inhibit-cookies))
        (if (null asynch-buffer)
            ;; We do not need to do anything, it was a mailto or something
            ;; similar that takes processing completely outside of the URL
            ;; package.
            nil
          (let ((proc (get-buffer-process asynch-buffer)))
            ;; If the access method was synchronous, `retrieval-done' should
            ;; hopefully already be set to t.  If it is nil, and `proc' is also
            ;; nil, it implies that the async process is not running in
            ;; asynch-buffer.  This happens e.g. for FTP files.  In such a case
            ;; url-file.el should probably set something like a `url-process'
            ;; buffer-local variable so we can find the exact process that we
            ;; should be waiting for.  In the mean time, we'll just wait for any
            ;; process output.
            (while (not retrieval-done)
              (url-debug 'retrieval
                         "Spinning in url-retrieve-synchronously: %S (%S)"
                         retrieval-done asynch-buffer)
              (if (buffer-local-value 'url-redirect-buffer asynch-buffer)
                  (setq proc (get-buffer-process
                              (setq asynch-buffer
                                    (buffer-local-value 'url-redirect-buffer
                                                        asynch-buffer))))
                (if (and proc (memq (process-status proc)
                                    '(closed exit signal failed))
                         ;; Make sure another process hasn't been started.
                         (eq proc (or (get-buffer-process asynch-buffer) proc)))
                    ;; FIXME: It's not clear whether url-retrieve's callback is
                    ;; guaranteed to be called or not.  It seems that url-http
                    ;; decides sometimes consciously not to call it, so it's not
                    ;; clear that it's a bug, but even then we need to decide how
                    ;; url-http can then warn us that the download has completed.
                    ;; In the mean time, we use this here workaround.
                    ;; XXX: The callback must always be called.  Any
                    ;; exception is a bug that should be fixed, not worked
                    ;; around.
                    (progn ;; Call delete-process so we run any sentinel now.
                      (delete-process proc)
                      (setq retrieval-done t)))
                ;; We used to use `sit-for' here, but in some cases it wouldn't
                ;; work because apparently pending keyboard input would always
                ;; interrupt it before it got a chance to handle process input.
                ;; `sleep-for' was tried but it lead to other forms of
                ;; hanging.  --Stef
                (unless (or (with-local-quit
                              (accept-process-output proc))
                            (null proc))
                  ;; accept-process-output returned nil, maybe because the process
                  ;; exited (and may have been replaced with another).  If we got
                  ;; a quit, just stop.
                  (when quit-flag
                    (delete-process proc))
                  (setq proc (and (not quit-flag)
                                  (get-buffer-process asynch-buffer)))))))
          asynch-buffer)))

If you still had doubts whether using the asynchronous interface is
worth it, there_'s your answer.

.. [1] Emacs has asynchronous "network processes" which unlike your
       usual asynchronous processes are not really processes, but
       rather a combination of ``select(2)`` and ``connect(2)`` with
       the semantics of an Emacs process.

.. _there: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/url/url.el?id=7a7164ea3eb7b3b7d2f7cfaec4ef73a90e14f735#n224
