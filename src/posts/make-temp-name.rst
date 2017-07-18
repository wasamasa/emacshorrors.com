((title . "make-temp-name")
 (date . "2017-07-18 09:14:57 +0200"))

For someone not terribly experienced in writing safe programs, one can
only hope that building blocks like ``make-temp-file`` are doing the
right thing and cannot be subverted by a malicious third party.  The
general advice here is that it's preferable to use the primitive for
creating the temporary file instead of the primitive to generate its
name.  Now, does Emacs reuse ``mkstemp(3)`` for this?  Or at least
``tmpnam(3)``?  Of course not!  Where we go, we can just invent `our
own source of randomness`_:

``make-temp-file`` looks `as follows`_:

.. code:: c

    static const char make_temp_name_tbl[64] =
    {
      'A','B','C','D','E','F','G','H',
      'I','J','K','L','M','N','O','P',
      'Q','R','S','T','U','V','W','X',
      'Y','Z','a','b','c','d','e','f',
      'g','h','i','j','k','l','m','n',
      'o','p','q','r','s','t','u','v',
      'w','x','y','z','0','1','2','3',
      '4','5','6','7','8','9','-','_'
    };

    static unsigned make_temp_name_count, make_temp_name_count_initialized_p;

    /* Value is a temporary file name starting with PREFIX, a string.

       The Emacs process number forms part of the result, so there is
       no danger of generating a name being used by another process.
       In addition, this function makes an attempt to choose a name
       which has no existing file.  To make this work, PREFIX should be
       an absolute file name.

       BASE64_P means add the pid as 3 characters in base64
       encoding.  In this case, 6 characters will be added to PREFIX to
       form the file name.  Otherwise, if Emacs is running on a system
       with long file names, add the pid as a decimal number.

       This function signals an error if no unique file name could be
       generated.  */

    Lisp_Object
    make_temp_name (Lisp_Object prefix, bool base64_p)
    {
      Lisp_Object val, encoded_prefix;
      ptrdiff_t len;
      printmax_t pid;
      char *p, *data;
      char pidbuf[INT_BUFSIZE_BOUND (printmax_t)];
      int pidlen;

      CHECK_STRING (prefix);

      /* VAL is created by adding 6 characters to PREFIX.  The first
         three are the PID of this process, in base 64, and the second
         three are incremented if the file already exists.  This ensures
         262144 unique file names per PID per PREFIX.  */

      pid = getpid ();

      if (base64_p)
        {
          pidbuf[0] = make_temp_name_tbl[pid & 63], pid >>= 6;
          pidbuf[1] = make_temp_name_tbl[pid & 63], pid >>= 6;
          pidbuf[2] = make_temp_name_tbl[pid & 63], pid >>= 6;
          pidlen = 3;
        }
      else
        {
    #ifdef HAVE_LONG_FILE_NAMES
          pidlen = sprintf (pidbuf, "%"pMd, pid);
    #else
          pidbuf[0] = make_temp_name_tbl[pid & 63], pid >>= 6;
          pidbuf[1] = make_temp_name_tbl[pid & 63], pid >>= 6;
          pidbuf[2] = make_temp_name_tbl[pid & 63], pid >>= 6;
          pidlen = 3;
    #endif
        }

      encoded_prefix = ENCODE_FILE (prefix);
      len = SBYTES (encoded_prefix);
      val = make_uninit_string (len + 3 + pidlen);
      data = SSDATA (val);
      memcpy (data, SSDATA (encoded_prefix), len);
      p = data + len;

      memcpy (p, pidbuf, pidlen);
      p += pidlen;

      /* Here we try to minimize useless stat'ing when this function is
         invoked many times successively with the same PREFIX.  We achieve
         this by initializing count to a random value, and incrementing it
         afterwards.

         We don't want make-temp-name to be called while dumping,
         because then make_temp_name_count_initialized_p would get set
         and then make_temp_name_count would not be set when Emacs starts.  */

      if (!make_temp_name_count_initialized_p)
        {
          make_temp_name_count = time (NULL);
          make_temp_name_count_initialized_p = 1;
        }

      while (1)
        {
          unsigned num = make_temp_name_count;

          p[0] = make_temp_name_tbl[num & 63], num >>= 6;
          p[1] = make_temp_name_tbl[num & 63], num >>= 6;
          p[2] = make_temp_name_tbl[num & 63], num >>= 6;

          /* Poor man's congruential RN generator.  Replace with
             ++make_temp_name_count for debugging.  */
          make_temp_name_count += 25229;
          make_temp_name_count %= 225307;

          if (!check_existing (data))
            {
              /* We want to return only if errno is ENOENT.  */
              if (errno == ENOENT)
                return DECODE_FILE (val);
              else
                /* The error here is dubious, but there is little else we
                   can do.  The alternatives are to return nil, which is
                   as bad as (and in many cases worse than) throwing the
                   error, or to ignore the error, which will likely result
                   in looping through 225307 stat's, which is not only
                   dog-slow, but also useless since eventually nil would
                   have to be returned anyway.  */
                report_file_error ("Cannot create temporary name for prefix",
                                   prefix);
              /* not reached */
            }
        }
    }

    DEFUN ("make-temp-name", Fmake_temp_name, Smake_temp_name, 1, 1, 0,
           doc: /* Generate temporary file name (string) starting with PREFIX (a string).
    The Emacs process number forms part of the result, so there is no
    danger of generating a name being used by another Emacs process
    \(so long as only a single host can access the containing directory...).

    This function tries to choose a name that has no existing file.
    For this to work, PREFIX should be an absolute file name.

    There is a race condition between calling `make-temp-name' and creating the
    file, which opens all kinds of security holes.  For that reason, you should
    normally use `make-temp-file' instead.  */)
      (Lisp_Object prefix)
    {
      return make_temp_name (prefix, 0);
    }

The generated file name is therefore a combination of the prefix, the
Emacs PID and three characters from the above table.  This makes about
200.000 possible temporary files that can be generated with a given
prefix in an Emacs session.  This range can be traversed in a
negligible amount of time to recreate the state of the RNG and
accurately predict the next temporary file name.

.. code:: elisp

    (defun make-temp-file (prefix &optional dir-flag suffix)
      "Create a temporary file.
    The returned file name (created by appending some random characters at the end
    of PREFIX, and expanding against `temporary-file-directory' if necessary),
    is guaranteed to point to a newly created empty file.
    You can then use `write-region' to write new data into the file.

    If DIR-FLAG is non-nil, create a new empty directory instead of a file.

    If SUFFIX is non-nil, add that at the end of the file name."
      ;; Create temp files with strict access rights.  It's easy to
      ;; loosen them later, whereas it's impossible to close the
      ;; time-window of loose permissions otherwise.
      (with-file-modes ?\700
        (let (file)
          (while (condition-case ()
                     (progn
                       (setq file
                             (make-temp-name
                              (if (zerop (length prefix))
                                  (file-name-as-directory
                                   temporary-file-directory)
                                (expand-file-name prefix
                                                  temporary-file-directory))))
                       (if suffix
                           (setq file (concat file suffix)))
                       (if dir-flag
                           (make-directory file)
                         (write-region "" nil file nil 'silent nil 'excl))
                       nil)
                   (file-already-exists t))
            ;; the file was somehow created by someone else between
            ;; `make-temp-name' and `write-region', let's try again.
            nil)
          file)))

It's interesting that the docstring of this function states that the
return value "is guaranteed to point to a newly created empty file.".
If there were to exist a file for every possible combination for a
prefix, this function would just fall into an infinite loop and block
Emacs for no apparent reason.  Both of these issues have been solved
in a better way in glibc_.

At least the impact of predicting the name is lessened if one uses
``make-temp-file`` instead of ``make-temp-name`` on its own.  An
attacker cannot create a symlink pointing to a rogue location with the
predicted name as that would trigger a ``file-already-exists`` error
and make the function use the next random name.  All they could do is
read out the file afterwards iff they have the same permission as the
user Emacs runs with.  A symlink attack can only be executed
successfully with a careless ``make-temp-name`` user, thankfully I've
not been able to find one worth subverting on GitHub yet.

Thanks to ``dale`` on ``#emacs`` for bringing this to my attention!

.. _our own source of randomness: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/fileio.c#n626
.. _as follows: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/files.el?id=0083123499cc29e301c197218d3809b225675e57#n1407
.. _glibc: https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/posix/tempname.c;h=b00bd588ec458cbe3bc9bd162515995c0104248b;hb=HEAD
