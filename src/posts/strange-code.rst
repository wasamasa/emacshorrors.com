((title . "Strange Code")
 (date . "2017-02-08 01:01:18 +0100"))

.. code:: elisp

    (defface w3m-history-current-url
      ;; The following strange code compounds the attributes of the
      ;; `secondary-selection' face and the `w3m-arrived-anchor' face,
      ;; and generates the new attributes for this face.
      (let ((base 'secondary-selection)
            (fn (if (featurep 'xemacs)
                    'face-custom-attributes-get
                  'custom-face-attributes-get));; What a perverseness it is.
            ;; Both `face-custom-attributes-get' in XEmacs and
            ;; `custom-face-attributes-get' in CUSTOM 1.9962 attempt to
            ;; require `font' in Emacs/w3 and `cl' arbitrarily. :-/
            (features (cons 'font features))
            base-attributes attributes attribute)
        (setq base-attributes (funcall fn base nil)
              attributes (funcall fn 'w3m-arrived-anchor nil))
        (while base-attributes
          (setq attribute (car base-attributes))
          (unless (memq attribute '(:foreground :underline))
            (setq attributes (plist-put attributes attribute
                                        (cadr base-attributes))))
          (setq base-attributes (cddr base-attributes)))
        (list (list t attributes)))
      "Face used to highlight the current url in the \"about://history/\" page."
      :group 'w3m-face)

These days one would simply use ``:inherit`` for both
``w3m-arrived-anchor`` and ``secondary-selection``.
