((title . "Computer Says No")
 (date . "2015-11-29 19:58:31 +0100"))

Like any other self-respecting text editing environment, Emacs allows
you to open links in a more suitable program, like your prefered
browser.  On Linux it's typical to let ``xdg-open`` handle the
surprisingly tricky task of figuring out what the default browser (or
file manager or mail program) is, so I did expect Emacs to figure this
out as well.  This is clearly not the case on my system.  Why?

.. code:: elisp

    (defun browse-url-can-use-xdg-open ()
      "Return non-nil if the \"xdg-open\" program can be used.
    xdg-open is a desktop utility that calls your preferred web browser.
    This requires you to be running either Gnome, KDE, Xfce4 or LXDE."
      (and (getenv "DISPLAY")
           (executable-find "xdg-open")
           ;; xdg-open may call gnome-open and that does not wait for its child
           ;; to finish.  This child may then be killed when the parent dies.
           ;; Use nohup to work around.  See bug#7166, bug#8917, bug#9779 and
           ;; http://lists.gnu.org/archive/html/emacs-devel/2009-07/msg00279.html
           (executable-find "nohup")
           (or (getenv "GNOME_DESKTOP_SESSION_ID")
               ;; GNOME_DESKTOP_SESSION_ID is deprecated, check on Dbus also.
               (condition-case nil
                   (eq 0 (call-process
                          "dbus-send" nil nil nil
                                      "--dest=org.gnome.SessionManager"
                                      "--print-reply"
                                      "/org/gnome/SessionManager"
                                      "org.gnome.SessionManager.CanShutdown"))
                 (error nil))
               (equal (getenv "KDE_FULL_SESSION") "true")
               (condition-case nil
                   (eq 0 (call-process
                          "/bin/sh" nil nil nil
                          "-c"
                          ;; FIXME use string-match rather than grep.
                          "xprop -root _DT_SAVE_MODE|grep xfce4"))
                 (error nil))
               (member (getenv "DESKTOP_SESSION") '("LXDE" "Lubuntu"))
               (equal (getenv "XDG_CURRENT_DESKTOP") "LXDE"))))

That's why.  Emacs expects me to be using a popular DE.  `This
snippet`_ is wrong for a number of reasons:

- ``xdg-open`` does attempt detecting your DE already
- ``xdg-open`` uses this information to do a better job at opening a
  resource for you, not to decide whether to open it at all (unlike
  this snippet)
- ``xdg-open`` does better at detecting the DE than this snippet and
  supports more DEs

Now, I wouldn't suggest the code of ``xdg-open`` is much better than
that, but it is there and it is the authoritative source most of this
was taken from.  Therefore ``browse-url-can-use-xdg-open`` can be
safely replaced with:

.. code:: elisp

    (defun browse-url-can-use-xdg-open ()
      "Return non-nil if the \"xdg-open\" program can be used.
    xdg-open is a desktop utility that calls your preferred web browser."
      (and (getenv "DISPLAY") (executable-find "xdg-open")))

I'd normally feel inclined to discuss this on ``emacs-devel`` weren't
they infamous for their endless bikeshedding and thread derailment.
Perhaps this one is better left to a bug report anyways.

.. _This snippet: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/net/browse-url.el?id=f4ded42cf84ba349a7187d0f27ed8a9025b9b54c#n907
