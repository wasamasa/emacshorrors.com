((title . "No Obvious Way To Do It Properly")
 (date . "2017-01-05 20:28:31 +0100"))

You can often catch me griping way more about an "Emacs API" than
Emacs Lisp, often without any explanation how the interfaces Emacs
provides could be possibly worse than the language used for this.
Here comes a not too obvious example of the issue at hand that will
not go away if you were to rewrite the code in question in *<insert
favorite toy language>*:

.. code:: elisp

    (defvar flyspell-prog-text-faces
      '(font-lock-string-face font-lock-comment-face font-lock-doc-face)
      "Faces corresponding to text in programming-mode buffers.")

    (defun flyspell-generic-progmode-verify ()
      "Used for `flyspell-generic-check-word-predicate' in programming modes."
      ;; (point) is next char after the word. Must check one char before.
      (let ((f (get-text-property (- (point) 1) 'face)))
        (memq f flyspell-prog-text-faces)))

`The above`_ is helper code for Flyspell to detect whether point is in a
string or comment.  While this is most certainly the easiest way to
achieve this goal, it isn't what the manual recommends.  What you're
supposed to do instead is to use the syntax table parser by accessing
its state via ``syntax-ppss`` and checking slots 3 and 4 [1]_.  The
issue with that approach is that it's not *that* simple, see the
sources of Smartparens for the ugly details, more specifically the
``sp-point-in-string-or-comment`` function.

Let's take a step back.  Why exactly do we need to check the
syntactical state of Emacs?  To understand what's going on, you'll
need to know that the first step when building a major mode for a
programming language is to set up the syntax tables so that Emacs can
recognize strings and comments properly.  Once this has been done,
both syntax highlighting and movement work correctly.  This hack has
been done back then to have that special case dealt with in a
performant manner, albeit it only works properly for languages looking
like C or Lisp.  Single characters can be marked as having a special
syntax, with extra slots for two-character sequences.  In Lisp, a
semicolon would start the comment and every newline would terminate
one, in C the comment starters and enders would be ``/*`` and ``*/``.
If you're using a language with more complicated rules than that,
there is the crutch of writing a custom ``syntax-propertize-function``
that walks over an arbitrary buffer region and applies syntax
properties manually.  This is how triple-quoted strings are
implemented in ``python.el``, the same technique is used to highlight
a S-expression comment in ``scheme.el`` which starts with ``#;`` and
extends to the following S-expression.

Unfortunately, not all modes play well.  Some resort to just painting
over the thing to be a comment or string by applying the right face to
it and sacrifice proper movement over symbols and S-expressions.  It
shouldn't be surprising that given the choice to fix faulty modes or
circumvent a cumbersome API, plenty Emacs hackers pick the latter and
get more robust handling for free.  After all, practicality beats
purity, right?

.. _The above: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/textmodes/flyspell.el?id=99af58d74e431da6b55f21272bf72a9f56ce0900#n409

.. [1] I've seen at least one mode checking slot 8 instead.  A
       detailed analysis of the difference is left to the reader as
       exercise.
