((title . "Less Than Exceptional")
 (date . "2017-02-26 18:46:53 +0100"))

As of a few weeks ago, I've become part of `Evil's new maintainer
team`_.  Most of the bugs we get are not quite Evil's fault, but
rather that of other components it relies upon, such as Emacs.  `#785`_
is a perfect example of this observation.  Take a look at `the
sources`_ of the function responsible for the reported behavior and
see whether you can find what's wrong about it:

.. code:: c

    DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 3, 0,
           doc: ...)
      (Lisp_Object keymap, Lisp_Object key, Lisp_Object accept_default)
    {
      ptrdiff_t idx;
      Lisp_Object cmd;
      Lisp_Object c;
      ptrdiff_t length;
      bool t_ok = !NILP (accept_default);

      keymap = get_keymap (keymap, 1, 1);

      length = CHECK_VECTOR_OR_STRING (key);
      if (length == 0)
        return keymap;

      idx = 0;
      while (1)
        {
          c = Faref (key, make_number (idx++));

          if (CONSP (c) && lucid_event_type_list_p (c))
            c = Fevent_convert_list (c);

          /* Turn the 8th bit of string chars into a meta modifier.  */
          if (STRINGP (key) && XINT (c) & 0x80 && !STRING_MULTIBYTE (key))
            XSETINT (c, (XINT (c) | meta_modifier) & ~0x80);

          /* Allow string since binding for `menu-bar-select-buffer'
             includes the buffer name in the key sequence.  */
          if (!INTEGERP (c) && !SYMBOLP (c) && !CONSP (c) && !STRINGP (c))
            message_with_string ("Key sequence contains invalid event %s", c, 1);

          cmd = access_keymap (keymap, c, t_ok, 0, 1);
          if (idx == length)
            return cmd;

          keymap = get_keymap (cmd, 0, 1);
          if (!CONSP (keymap))
            return make_number (idx);

          maybe_quit ();
        }
    }

If you haven't spotted it, the function does never signal an exception
for the exceptional case of an invalid key sequence.  Instead, it
returns one of the few types that aren't allowed in a definition,
namely an integer.  It's up to the caller to handle this case on their
own, something the majority of Emacs packages rightfully neglect to do
as even ``+`` informs you about invalid input.  This design decision
reeks of `the many ways of signalling errors in C`_ and worse, cannot be
properly fixed as there's code inside Emacs relying on the integer
return type.  There is even a ``lookup-key-ignore-too-long`` in Emacs
core that looks a lot like what I wrote, but it's part of
``menu-bar.el`` for whatever reason and not advertised at all.

tl;dr: In-band signalling sucks big time.

.. _Evil's new maintainer team: https://github.com/orgs/emacs-evil/people
.. _#785: https://github.com/emacs-evil/evil/pull/785
.. _the sources: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/keymap.c?id=6b6cc56e728a4d8b5ccac86ac393be7cd29207e2#n1198
.. _the many ways of signalling errors in C: http://www.tedunangst.com/flak/post/to-errno-or-to-error
