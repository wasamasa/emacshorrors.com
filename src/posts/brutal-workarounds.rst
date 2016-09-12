((title . "Brutal Workarounds")
 (date . "2016-09-12 10:15:07 +0200"))

Today I took another stab at my abandoned Emacs Lisp IRC bot and
thought to myself that it would be nice if it were able to notify me
on RSS and Atom feed updates.  Now, I'm not terribly fond of
reinventing the wheel, so like every good programmer I dared taking a
look at existing solutions, like `News Ticker`_:

.. code:: elisp

    (defun newsticker--do-xml-workarounds ()
      "Fix all issues which `xml-parse-region' could be choking on."

      ;; a very very dirty workaround to overcome the
      ;; problems with the newest (20030621) xml.el:
      ;; remove all unnecessary whitespace
      (goto-char (point-min))
      (while (re-search-forward ">[ \t\r\n]+<" nil t)
        (replace-match "><" nil t))
      ;; and another brutal workaround (20031105)!  For some
      ;; reason the xml parser does not like the colon in the
      ;; doctype name "rdf:RDF"
      (goto-char (point-min))
      (if (re-search-forward "<!DOCTYPE[ \t\n]+rdf:RDF" nil t)
          (replace-match "<!DOCTYPE rdfColonRDF" nil t))
      ;; finally.... ~##^°!!!!!
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
        (replace-match "\n" nil t))
      ;; still more brutal workarounds (20040309)!  The xml
      ;; parser does not like doctype rss
      (goto-char (point-min))
      (if (re-search-forward "<!DOCTYPE[ \t\n]+rss[ \t\n]*>" nil t)
          (replace-match "" nil t))
      ;; And another one (20050618)! (Fixed in GNU Emacs 22.0.50.18)
      ;; Remove comments to avoid this xml-parsing bug:
      ;; "XML files can have only one toplevel tag"
      (goto-char (point-min))
      (while (search-forward "<!--" nil t)
        (let ((start (match-beginning 0)))
          (unless (search-forward "-->" nil t)
            (error "Can't find end of comment"))
          (delete-region start (point))))
      ;; And another one (20050702)! If description is HTML
      ;; encoded and starts with a `<', wrap the whole
      ;; description in a CDATA expression.  This happened for
      ;; http://www.thefreedictionary.com/_/WoD/rss.aspx?type=quote
      (goto-char (point-min))
      (while (re-search-forward
              "<description>\\(<img.*?\\)</description>" nil t)
        (replace-match
         "<description><![CDATA[ \\1 ]]></description>"))
      ;; And another one (20051123)! XML parser does not
      ;; like this: <yweather:location city="Frankfurt/Main"
      ;; region="" country="GM" />
      ;; try to "fix" empty attributes
      ;; This happened for
      ;; http://xml.weather.yahoo.com/forecastrss?p=GMXX0040&u=f
      (goto-char (point-min))
      (while (re-search-forward "\\(<[^>]*\\)=\"\"" nil t)
        (replace-match "\\1=\" \""))
      ;;
      (set-buffer-modified-p nil))

I guess I'll not be using this_ and go for a special-purpose solution
instead without over a decade old workarounds.  After all, I'm not
stuck in 2005 with Emacs 21...

.. _News Ticker: https://www.emacswiki.org/emacs/NewsTicker
.. _this: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/net/newst-backend.el?id=74c5b735212ccd5f335312db693fd4754fddbce1#n1021
