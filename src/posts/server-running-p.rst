((title . "Determining if the server is started, or the wonders of server-running-p")
 (date . "2018-04-01 13:43:26 +02:00"))

*(This is a contributed post by thblt_)*

.. _thblt: https://github.com/thblt/

Trivia: How can you determine if the current Emacs instance has the
Emacs server running?

A quick search gives us three potential candidates: ``server-mode``,
``(daemonp)`` and ``(server-running-p)``.  That's way too much, but
surely one of them is the right one, isn't it?  Well, no.  Because the
real answer to this trivial question is: *you can't*.

 - ``server-mode`` is ``t`` if, and only if, the server was started
using the function with the same name.  But there are other ways to
run the server, like ``M-x server-start`` or ``emacs --daemon``.

 - ``(daemonp)`` returns t if, and only if, Emacs was started in
   daemon mode.

What about ``(server-running-p)``, then?  Well, it may look friendly,
but here be monsters.

It starts by looking promising: after ``M-x server-start``,
``(server-running-p)`` now returns ``t``!  Do we have a winner?  Not yet!
Let's pop a *new* Emacs instance and eval ``(server-running-p)`` without
starting the server.  ``t`` again!

What's happening?  The truth is that ``(server-running-p)`` is not
what it seems to be.  Here's its complete source code:

.. code:: elisp
    (defun server-running-p (&optional name)
      "Test whether server NAME is running.

        Return values:
          nil              the server is definitely not running.
          t                the server seems to be running.
          something else   we cannot determine whether it's running without using
                           commands which may have to wait for a long time."
      (unless name (setq name server-name))
      (condition-case nil
          (if server-use-tcp
        	    (with-temp-buffer
        	      (insert-file-contents-literally (expand-file-name name server-auth-dir))
        	      (or (and (looking-at "127\\.0\\.0\\.1:[0-9]+ \\([0-9]+\\)")
        		             (assq 'comm
        			                 (process-attributes
        			                  (string-to-number (match-string 1))))
        		             t)
        		        :other))
        	  (delete-process
        	   (make-network-process
        	    :name "server-client-test" :family 'local :server nil :noquery t
        	    :service (expand-file-name name server-socket-dir)))
        	  t)
        (file-error nil)))

The horror starts as soon as the docstring.  The ``-p`` suffix in the
name promises a predicate, that is, a boolean function.  But in
``server-running-p``, non-``nil`` is not a loud and clear "Yes!", it's a
mumbled "well, maybe, who knows?".  Ternary logic, because Emacs is
above the law of excluded middle.

But what does this function *do*?  It tries to determine if a server
called ``NAME`` is running, by assuming that this server would be
configured exactly the same as the running instance.  It may end up
looking at the socket file of the current server, or it may try to
initiate a TCP connection, which is extremely expensive.
``server-running-p`` is the kind of function you may be tempted to
call while building the mode line: try it, and get an instant and
unrecoverable Emacs freeze.  What it's supposed to be useful for is
extremely unclear.  It's unable to determine if the running instance
has a server --- but it uses this server's config to search for a
potentially completely different server.
