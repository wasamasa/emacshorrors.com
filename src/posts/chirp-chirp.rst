((title . "Chirp, chirp")
 (date . "2015-08-02 11:30:14"))

V155_ casually mentioned on ``##emacs.de`` that twittering-mode_ has
over ten thousand lines of code.  To further aid him in his evaluation
of Twitter clients for Emacs, I did take a cursory look at its
sources.

I don't really mind that its very first `compatibility check for an
Emacs version of 21`_ [1]_ loads up `the bundled entirety of url.el`_,
how it's using `inline certificates`_ [2]_ and that like most antique
japanese Emacs Lisp floating around it reinvents half of the
network-related functionality recent Emacs releases come with
including a pure Emacs Lisp implementation of `SHA1 hashing`_, an
obscure `GPG helper`_ [3]_ and hacks on existing JSON and XML parsers
to support fallback to an `uncommon UTF-8 variant`_.  I won't even
speak about the way its actually Twitter-related functionality is
implemented.  No, I'd just like you to marvel at `the very last form
in its sources`_:

.. code:: elisp

             (progn  (when  (
              boundp  (  intern (
               mapconcat 'identity '
               ("twittering" "oauth"
                 "consumer" "key" ) "-"
                  )  )  )   (eval  ` (
                   setq ,(intern (mapconcat
                    (quote identity) (quote
                     ("twittering"    "oauth"
                      "consumer" "key")  )"-"
                      ))  (base64-decode-string
                    (apply  'string  (mapcar   '1-
                   (quote (83 88 75 114 88 73 79 117
                 101 109 109 105 82 123 75 120 78 73
                105 122 83 69 67 78   98 49 75 109 101
              120 62 62))))))))(       when ( boundp  (
             intern (mapconcat '      identity'("twittering"
            "oauth" "consumer"         "secret") "-")))(eval `
           (setq  ,(intern   (         mapconcat 'identity '(
          "twittering" "oauth"          "consumer" "secret") "-"))
         (base64-decode-string          (apply 'string (mapcar '1-
        (quote   (91   70                    113 87 83 123 75 112
       87 123 75 117 87 50                109 50  102  85 83 91 101
      49 87 116 100 73 101                  106 82 107 67 113  90 49
     75 68  99  52  79 120                   80 89  91  51  79 85 71
    110 101  110 91  49                      100 49   58  71)))))) )))

First time I've ever seen Lisp obfuscation.  I'll leave taking it
apart as exercise for the inclined reader.

edit: In case you're wondering how the evaluation went, twaddle_ was
considered easier to hack and understand.

.. [1] Mind you that Emacs 21 was released in 2000 which is over
       fifteen years ago and makes it about as hard to support as
       Windows XP.
.. [2] There's even a shell script included in the project root
       directory for updating these certificates with ``sed``...
.. [3] Why would you even name it ``alpaca-kill-buffer`` when it ends
       up calling ``alpaca-save-buffer``?  The ``alpaca-delete-file``
       function is another WTF.

.. _V155: https://github.com/V155
.. _twittering-mode: https://github.com/hayamiz/twittering-mode
.. _compatibility check for an Emacs version of 21: https://github.com/hayamiz/twittering-mode/blob/b04a3afd0a2efb6ac17ef84b343a80339be10a03/twittering-mode.el#L51-L64
.. _the bundled entirety of url.el: https://github.com/hayamiz/twittering-mode/tree/master/url-emacs21
.. _inline certificates: https://github.com/hayamiz/twittering-mode/blob/b04a3afd0a2efb6ac17ef84b343a80339be10a03/twittering-mode.el#L1456-L1735
.. _SHA1 hashing: https://github.com/hayamiz/twittering-mode/blob/b04a3afd0a2efb6ac17ef84b343a80339be10a03/emacs21/sha1.el
.. _GPG helper: https://github.com/OldhamMade/alpaca.el/blob/master/alpaca.el
.. _uncommon UTF-8 variant: https://en.wikipedia.org/wiki/CESU-8
.. _the very last form in its sources: https://github.com/hayamiz/twittering-mode/blob/b04a3afd0a2efb6ac17ef84b343a80339be10a03/twittering-mode.el#L12530-L12555
.. _twaddle: https://github.com/nicferrier/emacs-twaddle
