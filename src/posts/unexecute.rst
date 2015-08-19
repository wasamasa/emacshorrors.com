((title . "Unexecute")
 (date . "2015-02-28 17:38:05"))

`This here`_ is the epitome of craziness.  I could very well give up
my search for more scary things in Emacs because I very much doubt
I'll find anything more fear-inducing, but first of all, let's explain
what's wrong with it.  Unlike my previous examples it's not something
I can show you by just pointing at code snippets, no, the problem is
of a much more fundamental nature than that.

The exact details of Emacs' build process have always been a bit of
mystery to me.  Sure, compiling the C sources to obtain an Emacs Lisp
interpreter and using it to byte-compile all Emacs Lisp files makes
sense.  But loading these files with it to "dump" the Emacs
binary?  The only time I've heard of that was when reading a
discussion about creating "executables" with Common Lisp which is
apparently achieved by serializing the program's current state to
disk.

However, it works a bit differently for Emacs.  After loading up the
bare minimum ``temacs`` executable with all necessary Emacs Lisp
files, the so-called ``unexec`` function reads in it own executable
it's originating from, scans over the entire process memory, then
modifies the file to contain the state obtained and saves it into the
``emacs`` executable.  In other words, this file contains a partial
and very platform-specific reimplementation of a linker_.  You can
take a look at other existing ones by searching for files names
``unex*.c``, this will turn up support for Linux, BSD, Windows, DOS,
Cygwin, the HP 9000 Series 800, OS X, AIX, IRIX, SGI and Solaris.

Not only is this a pretty gross and unportable hack which will
probably fail on systems using `memory security measures`_, no, it is
something only a few people will fully understand, let alone be able
to fix when faced with problems at this stage of the build process.

Personally I can only see its value for obtaining Emacs executables
containing a desired state, something people ask for who have to deal
with long startup times.  Too bad that cannot be done anymore in
sufficiently recent versions of Emacs where dumping an already dumped
``emacs`` executable is `strictly forbidden`_.

I hope at least `Guile Emacs`_ will `try getting rid of it`_.  Either
making startup fast enough or dumping to a platform-independent file
that's loaded by the ``emacs`` executable could work.  Please nuke
this nightmare from orbit as fast as possible.

edit: Guile Emacs got rid of it, the code is still included though.

.. _This here: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/unexelf.c?id=1a50945fa4c666ae2ab5cd9419d23ad063ea1249
.. _linker: https://en.wikipedia.org/wiki/Linker_(computing)
.. _memory security measures: https://en.wikipedia.org/wiki/Address_space_layout_randomization
.. _strictly forbidden: http://lists.gnu.org/archive/html/emacs-diffs/2014-03/msg00212.html
.. _Guile Emacs: http://www.emacswiki.org/emacs/GuileEmacs
.. _try getting rid of it: http://www.emacswiki.org/emacs/GuileEmacsTodo
