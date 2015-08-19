((title . "For The Sole Reason of Efficiency")
 (date . "2015-07-26 15:56:46"))

.. code:: c

    /* Primitives for work of the "widget" library.
       In an ideal world, this section would not have been necessary.
       However, lisp function calls being as slow as they are, it turns
       out that some functions in the widget library (wid-edit.el) are the
       bottleneck of Widget operation.  Here is their translation to C,
       for the sole reason of efficiency.  */

    DEFUN ("plist-member", Fplist_member, Splist_member, 2, 2, 0,
           doc: /* Return non-nil if PLIST has the property PROP.
    PLIST is a property list, which is a list of the form
    \(PROP1 VALUE1 PROP2 VALUE2 ...\).  PROP is a symbol.
    Unlike `plist-get', this allows you to distinguish between a missing
    property and a property with the value nil.
    The value is actually the tail of PLIST whose car is PROP.  */)
      (Lisp_Object plist, Lisp_Object prop)
    {
      while (CONSP (plist) && !EQ (XCAR (plist), prop))
        {
          QUIT;
          plist = XCDR (plist);
          plist = CDR (plist);
        }
      return plist;
    }

    DEFUN ("widget-put", Fwidget_put, Swidget_put, 3, 3, 0,
           doc: /* In WIDGET, set PROPERTY to VALUE.
    The value can later be retrieved with `widget-get'.  */)
      (Lisp_Object widget, Lisp_Object property, Lisp_Object value)
    {
      CHECK_CONS (widget);
      XSETCDR (widget, Fplist_put (XCDR (widget), property, value));
      return value;
    }

    DEFUN ("widget-get", Fwidget_get, Swidget_get, 2, 2, 0,
           doc: /* In WIDGET, get the value of PROPERTY.
    The value could either be specified when the widget was created, or
    later with `widget-put'.  */)
      (Lisp_Object widget, Lisp_Object property)
    {
      Lisp_Object tmp;

      while (1)
        {
          if (NILP (widget))
            return Qnil;
          CHECK_CONS (widget);
          tmp = Fplist_member (XCDR (widget), property);
          if (CONSP (tmp))
            {
              tmp = XCDR (tmp);
              return CAR (tmp);
            }
          tmp = XCAR (widget);
          if (NILP (tmp))
            return Qnil;
          widget = Fget (tmp, Qwidget_type);
        }
    }

    DEFUN ("widget-apply", Fwidget_apply, Swidget_apply, 2, MANY, 0,
           doc: /* Apply the value of WIDGET's PROPERTY to the widget itself.
    ARGS are passed as extra arguments to the function.
    usage: (widget-apply WIDGET PROPERTY &rest ARGS)  */)
      (ptrdiff_t nargs, Lisp_Object *args)
    {
      /* This function can GC.  */
      struct gcpro gcpro1, gcpro2;
      Lisp_Object widget = args[0];
      Lisp_Object property = args[1];
      Lisp_Object propval = Fwidget_get (widget, property);
      Lisp_Object trailing_args = Flist (nargs - 2, args + 2);
      GCPRO2 (propval, trailing_args);
      Lisp_Object result = CALLN (Fapply, propval, widget, trailing_args);
      UNGCPRO;
      return result;
    }

Credits for `this scare`_ go to `Tom Tromey`_.

Here's my take on translating these functions to Emacs Lisp again:

.. code:: elisp

    (defun widget-put (widget property value)
      (setcdr widget (plist-put (cdr widget) property value)))

    (defun widget-get (widget property)
      (let ((plist (plist-member (cdr widget) property)))
        (if plist
            (cadr plist)
          (let ((widget-name (car widget)))
            (and widget-name (widget-get (get widget-name 'widget-type)
                                          property))))))

    (defun widget-apply (widget property &rest args)
      (apply (widget-get widget property) widget args))

edit: I've done a simple benchmark by launching an Emacs instance via
``emacs -Q`` and running the following code going over all known
customization groups:

.. code:: elisp

    (defun my-customization-groups ()
      (let (groups)
        (mapatoms (lambda (symbol)
                    (let ((group (get symbol 'custom-group)))
                      (when group
                        (push symbol groups)))))
        groups))

    (length (my-customization-groups)) ;=> 81

    (benchmark 1 '(mapc 'customize-group (my-customization-groups)))

This reveals that for a rather unimpressive number of customization
groups, it takes Emacs either 10 or 11 seconds on my old thinkpad to
create their buffers, depending on whether I'm using the built-in or
my self-made functions.  I think it's rather telling that one second
of speed-up is negligible for a rarely used command and it's more
interesting that creating a customization buffer takes a comparatively
long time.  Perhaps I'll turn this into a patch some day and find out
even more scary things about these functions...

.. _this scare: http://git.savannah.gnu.org/cgit/emacs.git/tree/src/fns.c?id=fac8492664246c49ee145802cc124aa9e1636e7b#n2915
.. _Tom Tromey: https://github.com/tromey
