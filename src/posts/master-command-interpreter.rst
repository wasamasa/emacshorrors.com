((title . "Master Command Interpreter")
 (date . "2015-05-27 07:20:10"))

The `command interpreter`_ abstraction is one of the noteworthy
Emacs features that convinced me to turn away from Vim as editor,
simply because having one common user interface over all REPL-like
process interaction is very convenient.  While it doesn't power every
conceivable interaction mode (see CIDER_ or eshell_), plenty others
make use of it and therefore behave mostly the same.  In other words,
there is a margin for non-standard behaviour, the very area where the
uglier parts can be observed...

`Alain Kalker`_ made me investigate in a case of such a non-standard
feature gone wrong.  His customization of a read-only prompt did break
the ability of tcl.el_ to evaluate regions, sending program code
normally however did still work.  Fortunately, there is `a hint`_
leading us to gud.el_ and revealing what exactly is going on here.

The reveal is that ``comint``'s basic mode of operation is looking for
prompts to both discern input from output and tell when output has
ended.  How?  Of course by using regular expressions!  And your prompt
better be not two lines, because ``comint`` assumes `it is on its own
line`_, silly.  This assumption is so deeply ingrained that even if
text is sent to the process without getting echoed back as it is the
case for sending a region, the prompt (and any other text, like
evaluation warnings) are returned, so sending multiple things over
results in multiple prompts in a row.  Some people like the ``gud``
authors were clearly annoyed by this, so what did they do, silencing
the process output temporarily?  Of course not, instead the buffer
contents are selectively deleted and replaced with a single prompt.
And doing that in ``tcl.el`` without any precautions `used to error
out`_ because the prompts were read-only in this specific incident.
Sigh.

.. _command interpreter: https://www.masteringemacs.org/article/comint-writing-command-interpreter
.. _CIDER: https://github.com/clojure-emacs/cider
.. _eshell: https://www.gnu.org/software/emacs/manual/html_mono/eshell.html
.. _Alain Kalker: https://github.com/ackalker/
.. _tcl.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/tcl.el
.. _a hint: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/tcl.el?id=82e2ce9d792e6bd76cd517589d5b89144497ecf8#n1019
.. _gud.el: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/gud.el?id=d3155315c85212f224fc5df0239182dafdfd6284#n2453
.. _it is on its own line: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/comint.el?id=d3155315c85212f224fc5df0239182dafdfd6284#n1264
.. _used to error out: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=20549
