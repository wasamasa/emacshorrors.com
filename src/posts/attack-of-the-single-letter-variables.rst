((title . "Attack of the Single-Letter Variables")
 (date . "2015-04-06 00:28:51 +0200"))

.. code:: elisp

    (defvar org-m nil)
    (defvar org-l nil)
    (defvar org-f nil)
    (defun org-get-level-face (n)
      "Get the right face for match N in font-lock matching of headlines."
      (setq org-l (- (match-end 2) (match-beginning 1) 1))
      (if org-odd-levels-only (setq org-l (1+ (/ org-l 2))))
      (if org-cycle-level-faces
          (setq org-f (nth (% (1- org-l) org-n-level-faces) org-level-faces))
        (setq org-f (nth (1- (min org-l org-n-level-faces)) org-level-faces)))
      (cond
       ((eq n 1) (if org-hide-leading-stars 'org-hide org-f))
       ((eq n 2) org-f)
       (t (if org-level-color-stars-only nil org-f))))

`This code`_ figures out what to apply to the leading section stars to
make them invisible when ``org-hide-leading-stars`` is enabled.  What
I still don't get though are the three one-letter variables.
``org-m`` is unused.  ``org-l`` and ``org-f`` are supposedly for the
level and face? But why did they not get proper names despite them
only being used in that function?  And why did they even get prefixed,
it's not like that's making it any better?

So many questions.  So little time.  A study on programming skills of
Emacs Lisp hackers might be interesting to unravel their answers.

.. _This code: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/org/org.el?id=7514b24b6a512d85b762c603e9e0107d2c8a52f1#n6377
