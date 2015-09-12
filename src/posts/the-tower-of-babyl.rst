((title . "The Tower Of BABYL")
 (date . "2015-09-12 10:42:25 +0200"))

I've finally found the proof of file-local variables being a hack:
Every non-interactive function and variable dealing with them uses
``hack-`` as prefix!  ``hack-local-variables`` does the heavy lifting,
``hack-local-variables-confirm`` nags you into acknowledging the
potential dangers of such variables, etc.

Does the same hold true for directory-local variables?  I'm pretty
sure of it, given the existence of `the following`_:

.. code:: elisp

    ;; This is an odd variable IMO.
    ;; You might wonder why it is needed, when we could just do:
    ;; (set (make-local-variable 'enable-local-variables) nil)
    ;; These two are not precisely the same.
    ;; Setting this variable does not cause -*- mode settings to be
    ;; ignored, whereas setting enable-local-variables does.
    ;; Only three places in Emacs use this variable: tar and arc modes,
    ;; and rmail.  The first two don't need it.  They already use
    ;; inhibit-local-variables-regexps, which is probably enough, and
    ;; could also just set enable-local-variables locally to nil.
    ;; Them setting it has the side-effect that dir-locals cannot apply to
    ;; eg tar files (?).  FIXME Is this appropriate?
    ;; AFAICS, rmail is the only thing that needs this, and the only
    ;; reason it uses it is for BABYL files (which are obsolete).
    ;; These contain "-*- rmail -*-" in the first line, which rmail wants
    ;; to respect, so that find-file on a BABYL file will switch to
    ;; rmail-mode automatically (this is nice, but hardly essential,
    ;; since most people are used to explicitly running a command to
    ;; access their mail; M-x gnus etc).  Rmail files may happen to
    ;; contain Local Variables sections in messages, which Rmail wants to
    ;; ignore.  So AFAICS the only reason this variable exists is for a
    ;; minor convenience feature for handling of an obsolete Rmail file format.
    (defvar local-enable-local-variables t
      "Like `enable-local-variables' but meant for buffer-local bindings.
    The meaningful values are nil and non-nil.  The default is non-nil.
    If a major mode sets this to nil, buffer-locally, then any local
    variables list in a file visited in that mode will be ignored.

    This variable does not affect the use of major modes specified
    in a -*- line.")

.. _the following: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/files.el?id=fc9dc032906d840f63d29fbdcfd25e36ca3451b8#n520
