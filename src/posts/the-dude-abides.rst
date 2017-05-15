((title . "The Dude Abides")
 (date . "2017-05-15 21:59:37 +0200"))

While crafting `another patch for cc-mode`_, I stumbled upon `the
following`_:

.. code:: elisp

    ;; RMS says don't make these the default.
    ;; (April 2006): RMS has now approved these commands as defaults.
    (unless (memq 'argumentative-bod-function c-emacs-features)
      (define-key c-mode-base-map "\e\C-a"    'c-beginning-of-defun)
      (define-key c-mode-base-map "\e\C-e"    'c-end-of-defun))

I've seen this kind of thing before_, but it keeps puzzling me.  Why
would you:

- Put a comment there about not making the following a default
- Put a clarifying comment how these have been approved as defaults
  now
- Keep both comments to trip up future readers

Any other project I'm involved in would immediately remove that
commentary.  Emacs is the only one where suggesting such drastic
action would yield a lengthy bikeshedding discussion about the merits
of pre-VCS traditions and how keeping them will toughen up whoever may
join in the collaborative development effort.  Yes, seriously_.

.. _another patch for cc-mode: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26658
.. _the following: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/cc-mode.el?id=d23a486ba27405acfda67a4dc387ade5e399a29b#n305
.. _before: http://emacshorrors.com/posts/legal-limit.html
.. _seriously: https://lists.gnu.org/archive/html/emacs-devel/2016-03/msg00210.html
