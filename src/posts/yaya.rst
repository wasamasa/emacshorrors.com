((title . "YaYa")
 (date . "2015-03-17 10:59:59"))

I originally started writing this post because I'm using the pretty
awesome Evil_ package and decided to look into the infamous Yasnippet_
package to have something more convenient than looking up `Org-mode's
code block syntax`_ every time I'm adding something new to `my
literate Emacs configuration`_.

Evil uses the ``TAB`` key by default to bind_ its
``evil-jump-forward`` command to it.  This key corresponds to both the
physical tabulator key and the ``C-i`` keybinding.  Binding a command
to either ``TAB`` or ``C-i`` in Emacs [1]_ results in a command you
can invoke by pressing either of these.  However, in GUI Emacs
instances you have an extra option, you can bind a command to
``<tab>``.  This rather curious notation is for the tabulator key
symbol, a special keyboard event that is only sent by pressing the
tabulator key.  Binding a command to it and a different command to
``TAB`` therefore results in the former command being called upon
pressing the tabulator key and the latter upon pressing ``C-i``.  This
can be easily proven by enabling ``evil-mode`` in an Org file and
checking the resolved commands with ``F1 k``.

In a perfect world having both ``C-i`` and the tabulator key doing
different things would work out of the box.  Unfortunately this was
not the case for me.  Some modes bind ``TAB``, some bind ``C-i``, some
bind both.  I initially unbound ``TAB`` in Evil for having Emacs
defaults exposed and later went over to `a bit of keyboard wizardry`_
to tell Emacs to discern both keybindings at the input decoding stage
and defined a ``<C-i>`` symbol that way which I've bound to
``evil-jump-forward`` in the appropriate Evil keymap and otherwise to
do what ``TAB`` would.

I assumed this would solve problems once and for all.  This is where
Yasnippet enters the stage.  For a yet undiscovered reason it undoes
my carefully set up input decoding hack.  My first reaction was
checking whether this was a phenomenon manifesting itself immediately
after init.  It wasn't.  No, it is triggered at a later point in my
editing sessions and remains hard to reproduce.  Searching in its
sources only made me learn about `yet another obscure keyboard event
notation`_ which `apparently originates`_ from `Lucid Emacs`_.  I
ended up unbinding everything looking like an offending key in its
keymaps and documented_ the not so trivial procedure of making an
alternative trigger key work in my Emacs configuration.

The bug was finally gone.  Still angry over what I've went through (in
the end I had only figured out a workaround), I started reading
Yasnippet's sources.  I found enough horrific things in it to compile
my Top 5.  Enjoy!

5. `Update the docs, damnit`_

.. code:: elisp

    (defun yas--fallback-translate-input (keys)
      "Emulate `read-key-sequence', at least what I think it does.
    Keys should be an untranslated key vector. Returns a translated
    vector of keys. FIXME not thoroughly tested."
      (let ((retval [])
            (i 0))
        (while (< i (length keys))
          (let ((j i)
                (translated local-function-key-map))
            (while (and (< j (length keys))
                        translated
                        (keymapp translated))
              (setq translated (cdr (assoc (aref keys j) (remove 'keymap translated)))
                    j (1+ j)))
            (setq retval (vconcat retval (cond ((symbolp translated)
                                                `[,translated])
                                               ((vectorp translated)
                                                translated)
                                               (t
                                                (substring keys i j)))))
            (setq i j)))
        retval))

4. `"I love it :)"`_

.. code:: elisp

    (defun yas--save-backquotes ()
      "Save all the \"`(lisp-expression)`\"-style expressions
    with their evaluated value into `yas--backquote-markers-and-strings'."
      (while (re-search-forward yas--backquote-lisp-expression-regexp nil t)
        (let ((current-string (match-string-no-properties 1)) transformed)
          (save-restriction (widen)
                            (delete-region (match-beginning 0) (match-end 0)))
          (setq transformed (yas--eval-lisp (yas--read-lisp (yas--restore-escapes current-string '(?`)))))
          (goto-char (match-beginning 0))
          (when transformed
            (let ((marker (make-marker)))
              (save-restriction
                (widen)
                (insert "Y") ;; quite horrendous, I love it :)
                (set-marker marker (point))
                (insert "Y"))
              (push (cons marker transformed) yas--backquote-markers-and-strings))))))

3. Troublemakers_

.. code:: elisp

    (defun yas--indent-according-to-mode (snippet-markers)
      "Indent current line according to mode, preserving SNIPPET-MARKERS."
      ;;; Apropos indenting problems....
      ;;
      ;; `indent-according-to-mode' uses whatever `indent-line-function'
      ;; is available. Some implementations of these functions delete text
      ;; before they insert. If there happens to be a marker just after
      ;; the text being deleted, the insertion actually happens after the
      ;; marker, which misplaces it.
      ;;
      ;; This would also happen if we had used overlays with the
      ;; `front-advance' property set to nil.
      ;;
      ;; This is why I have these `trouble-markers', they are the ones at
      ;; they are the ones at the first non-whitespace char at the line
      ;; (i.e. at `yas--real-line-beginning'. After indentation takes place
      ;; we should be at the correct to restore them to. All other
      ;; non-trouble-markers have been *pushed* and don't need special
      ;; attention.
      ;;
      (goto-char (yas--real-line-beginning))
      (let ((trouble-markers (remove-if-not #'(lambda (marker)
                                                (= marker (point)))
                                            snippet-markers)))
        (save-restriction
          (widen)
          (condition-case _
              (indent-according-to-mode)
            (error (yas--message 3 "Warning: `yas--indent-according-to-mode' having problems running %s" indent-line-function)
                   nil)))
        (mapc #'(lambda (marker)
                  (set-marker marker (point)))
              trouble-markers)))

2. ZOMGPERFORMANCE_

.. code:: elisp

    ;; Apropos markers-to-points:
    ;;
    ;; This was found useful for performance reasons, so that an
    ;; excessive number of live markers aren't kept around in the
    ;; `buffer-undo-list'. However, in `markers-to-points', the
    ;; set-to-nil markers can't simply be discarded and replaced with
    ;; fresh ones in `points-to-markers'. The original marker that was
    ;; just set to nil has to be reused.
    ;;
    ;; This shouldn't bring horrible problems with undo/redo, but it
    ;; you never know

1. Stupid_

.. code:: elisp

    ;; FIXME: the more I look at this data-structure the more I think I'm
    ;; stupid. There has to be an easier way (but beware lots of code
    ;; depends on this).


edit: It turned out that Yasnippet was not the culprit despite its
keybinding antics, here's `the commit fixing the described issue`_.

.. [1] The precise details of why ``C-i`` equals ``TAB`` are covered
       in `catern's blog post`_ about terminals and keyboard input.

.. _Evil: https://bitbucket.org/lyro/evil/wiki/Home
.. _Yasnippet: https://github.com/capitaomorte/yasnippet
.. _Org-mode's code block syntax: http://orgmode.org/manual/Literal-examples.html#Literal-examples
.. _my literate Emacs configuration: https://github.com/wasamasa/dotemacs/blob/master/init.org
.. _bind: https://bitbucket.org/lyro/evil/src/b14b5f13d29449620d54f0f8a5d8392a167b7184/evil-maps.el?at=default#cl-321
.. _catern's blog post: http://catern.com/posts/terminal_quirks.html
.. _a bit of keyboard wizardry: https://github.com/wasamasa/dotemacs/blob/56b6618d257d9ab957730570c43bc2def044babd/init.org#evil
.. _yet another obscure keyboard event notation: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/keymap.c?id=508049aae95c42a3e6fe989ff403bf7cb6f88433#n1250
.. _apparently originates: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/keyboard.c?id=508049aae95c42a3e6fe989ff403bf7cb6f88433#n6770
.. _Lucid Emacs: http://www.jwz.org/doc/lemacs.html
.. _documented: https://github.com/wasamasa/dotemacs/commit/56b6618d257d9ab957730570c43bc2def044babd
.. _Update the docs, damnit: https://github.com/capitaomorte/yasnippet/blob/21ffe4b797e7c54f128fdf3ee205273e74f1be33/yasnippet.el#L2289-L2311
.. _"I love it :)": https://github.com/capitaomorte/yasnippet/blob/21ffe4b797e7c54f128fdf3ee205273e74f1be33/yasnippet.el#L3990-L4006
.. _Troublemakers: https://github.com/capitaomorte/yasnippet/blob/21ffe4b797e7c54f128fdf3ee205273e74f1be33/yasnippet.el#L3872-L3904
.. _ZOMGPERFORMANCE: https://github.com/capitaomorte/yasnippet/blob/21ffe4b797e7c54f128fdf3ee205273e74f1be33/yasnippet.el#L3264-L3274
.. _Stupid: https://github.com/capitaomorte/yasnippet/blob/21ffe4b797e7c54f128fdf3ee205273e74f1be33/yasnippet.el#L1037-L1039
.. _the commit fixing the described issue: https://github.com/wasamasa/dotemacs/commit/8503aa99856d2f0142ee205b15f9177eb4886f6e
