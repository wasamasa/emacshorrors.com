((title . "\"titlecantbewhitespaceohthehorror\"")
 (date . "2016-01-22 23:48:23 +0100")
 (updated . "2016-10-29 12:40:01 +0200"))

**Update**: Emacs is now using webkit2gtk and removed the title hack
in favor of asynchronously executed JavaScript.  I expect that change
to land in Emacs 25.2.

``xwidget.el`` has `made it`_ into Emacs 25!  You might wonder how
exactly the interaction between a webkit widget and Emacs works.  The
answer to this is simple, while lower-level tasks require bindings to
the library exposing webkit, higher-level tasks are solved in
JavaScript.  This is why ``xwidget-webkit-execute-script`` is used
throughout the sources, it binds webkit_view_execute_script_.  Now,
what's weird about that one is that while it has an argument for the
code to evaluate, there is no mechanism to retrieve the return value
at all.  This oversight has been fixed in a later version of webkit by
introducing webkit_web_view_run_javascript_ for kicking off and
webkit_web_view_run_javascript_finish_ for finalizing and retrieving
the return value.  Given this information, how the heck does
``xwidget.el`` solve that problem with the earlier version of the
library?

That's how:

.. code:: elisp

    (defun xwidget-webkit-execute-script-rv (xw script &optional default)
      "Same as 'xwidget-webkit-execute-script' but but with return value.
    XW is the webkit instance.  SCRIPT is the script to execute.
    DEFAULT is the defaultreturn value."
      ;; Notice the ugly "title" hack.  It is needed because the Webkit
      ;; API at the time of writing didn't support returning values.  This
      ;; is a wrapper for the title hack so it's easy to remove should
      ;; Webkit someday support JS return values or we find some other way
      ;; to access the DOM.

      ;; Reset webkit title.  Not very nice.
      (let* ((emptytag "titlecantbewhitespaceohthehorror")
             title)
        (xwidget-webkit-execute-script xw (format "document.title=\"%s\";"
                                                  (or default emptytag)))
        (xwidget-webkit-execute-script xw (format "document.title=%s;" script))
        (setq title (xwidget-webkit-get-title xw))
        (if (equal emptytag title)
            (setq title ""))
        (unless title
          (setq title default))
        title))

`The above hack`_ poses a number of questions:

- Why does the title need to be a non-blank string?
- Why is the previous title not saved and restored?
- Why are you forcing me to use an optional default parameter to
  distinguish a return value serializing to an empty string from the
  empty string as return value?
- Why do I need to decode JSON to make sense of the result?
- The injection is just wrong.  If you pass ``1+1;2+2`` you only get
  the return value of the first expression, but both are evaluated
  anyways.  Nothing the good old ``function(){...}()`` trick couldn't
  fix though...
- Under what other circumstances will this hack break?

Definitely not what I've expected the title to be used for when you're
not looking.

.. _made it: http://thread.gmane.org/gmane.emacs.devel/196096/
.. _webkit_view_execute_script: http://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-webkitwebview.html#webkit-web-view-execute-script
.. _webkit_web_view_run_javascript: http://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html#webkit-web-view-run-javascript
.. _webkit_web_view_run_javascript_finish: http://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html#webkit-web-view-run-javascript-finish

.. _The above hack: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/xwidget.el?h=emacs-25&id=6ff8b45f18619c2dc95dfb1d92a5c48b14049973#n491
