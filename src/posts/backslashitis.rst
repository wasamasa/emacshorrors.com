((title . "Backslashitis")
 (date . "2015-10-10 13:06:24 +0200"))

Regular expressions are of utmost importance in Emacs, simply because
two of the basic operations inside a buffer are searching for one
(``re-search-forward``) and deciding whether point precedes a certain
regular expression (``looking-at``).  This allows one to do basic
parsing, syntax highlighting, traversing buffers and much more.

A common annoyance with regular expressions is that if they're given
in string form, both special constructs and backslashes themselves
need to be escaped, depending on the context they're used in even
multiple times.  The amount of backslashes can be reduced by either
introducing a special string or regular expression type, usually known
as "raw string".  A number of languages do have this feature,
including `C++11`_, Ruby_, Python_, Go_, Perl_, `C#`_ and many more, but
Emacs Lisp doesn't.  There has been a_ few_ proposals on the
`emacs-devel mailing list`_, but so far each one has been dropped with
the reasoning that the code scanning over strings for movement in
buffers (like, ``M-f`` and ``M-b``) would need to be adjusted as well.

Makes one wonder how `the most severe case of backslashitis`_ looks
like...

.. code:: elisp

    "...
    - If a string, use as regexp to search *backward* from the label.  Context
      is then the text following the end of the match.  E.g. setting this to
      \"\\\\\\\\caption[[{]\" will use the caption in a figure or table
      environment.
      \"\\\\\\\\begin{eqnarray}\\\\|\\\\\\\\\\\\\\\\\" works for eqnarrays.
    ..."

Seventeen backslashes in a row!  One is for the quote, the remaining
sixteen get turned into eight when viewing this docstring in a help
buffer, the Emacs Lisp reader makes four of them and search operates
on two, simply because one uses two backslashes inside an ``eqnarray``
environment for a linebreak.

.. _C++11: http://en.cppreference.com/w/cpp/language/string_literal
.. _Ruby: http://ruby-doc.org/core-2.2.3/Regexp.html
.. _Python: https://docs.python.org/3/library/re.html
.. _Go: https://golang.org/ref/spec#String_literals
.. _Perl: http://perldoc.perl.org/perlop.html#Quote-and-Quote-like-Operators
.. _C#: https://msdn.microsoft.com/en-us/library/362314fe.aspx
.. _a: http://lists.gnu.org/archive/html/emacs-devel/2012-08/msg00094.html
.. _few: http://lists.gnu.org/archive/html/emacs-devel/2014-07/msg00313.html
.. _emacs-devel mailing list: http://lists.gnu.org/archive/html/emacs-devel/index.html
.. _the most severe case of backslashitis: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/textmodes/reftex-vars.el?id=f655d09fd5b49652f11ab91a31b920dbc36eb10f#n542
