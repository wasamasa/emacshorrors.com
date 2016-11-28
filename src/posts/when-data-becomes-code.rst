((title . "When Data Becomes Code")
 (date . "2016-11-28 10:31:54 +0100"))

If you've hung out on ``#emacs`` for a while, chances are you've been
recommended ``ibuffer`` for advanced buffer management (like,
killing many buffers at once).  There is a horror lurking beneath its
pretty interface though and it starts out with `a customizable`_:

.. code:: elisp

    (defcustom ibuffer-always-compile-formats (featurep 'bytecomp)
      "If non-nil, then use the byte-compiler to optimize `ibuffer-formats'.
    This will increase the redisplay speed, at the cost of loading the
    elisp byte-compiler."
      :type 'boolean
      :group 'ibuffer)

Uh-oh:

.. code:: elisp

    (defun ibuffer-compile-make-eliding-form (strvar elide from-end-p)
      (let ((ellipsis (propertize ibuffer-eliding-string 'font-lock-face 'bold)))
        (if (or elide (with-no-warnings ibuffer-elide-long-columns))
            `(if (> strlen 5)
                 ,(if from-end-p
                      ;; FIXME: this should probably also be using
                      ;; `truncate-string-to-width' (Bug#24972)
                      `(concat ,ellipsis
                               (substring ,strvar
                                          (string-width ibuffer-eliding-string)))
                    `(concat
                      (truncate-string-to-width
                       ,strvar (- strlen (string-width ,ellipsis)) nil ?.)
                      ,ellipsis))
               ,strvar)
          strvar)))

    (defun ibuffer-compile-make-substring-form (strvar maxvar from-end-p)
      (if from-end-p
          ;; FIXME: not sure if this case is correct (Bug#24972)
          `(truncate-string-to-width str strlen (- strlen ,maxvar) nil ?\s)
        `(truncate-string-to-width ,strvar ,maxvar nil ?\s)))

    (defun ibuffer-compile-make-format-form (strvar widthform alignment)
      (let* ((left `(make-string tmp2 ?\s))
             (right `(make-string (- tmp1 tmp2) ?\s)))
        `(progn
           (setq tmp1 ,widthform
                 tmp2 (/ tmp1 2))
           ,(pcase alignment
              (:right `(concat ,left ,right ,strvar))
              (:center `(concat ,left ,strvar ,right))
              (:left `(concat ,strvar ,left ,right))
              (_ (error "Invalid alignment %s" alignment))))))

    (defun ibuffer-compile-format (format)
      (let ((result nil)
            ;; We use these variables to keep track of which variables
            ;; inside the generated function we need to bind, since
            ;; binding variables in Emacs takes time.
            (vars-used ()))
        (dolist (form format)
          (push
           ;; Generate a form based on a particular format entry, like
           ;; " ", mark, or (mode 16 16 :right).
           (if (stringp form)
               ;; It's a string; all we need to do is insert it.
               `(insert ,form)
             (let* ((form (ibuffer-expand-format-entry form))
                    (sym (nth 0 form))
                    (min (nth 1 form))
                    (max (nth 2 form))
                    (align (nth 3 form))
                    (elide (nth 4 form)))
               (let* ((from-end-p (when (cl-minusp min)
                                    (setq min (- min))
                                    t))
                      (letbindings nil)
                      (outforms nil)
                      minform
                      maxform
                      min-used max-used strlen-used)
                 (when (or (not (integerp min)) (>= min 0))
                   ;; This is a complex case; they want it limited to a
                   ;; minimum size.
                   (setq min-used t)
                   (setq strlen-used t)
                   (setq vars-used '(str strlen tmp1 tmp2))
                   ;; Generate code to limit the string to a minimum size.
                   (setq minform `(progn
                                    (setq str
                                          ,(ibuffer-compile-make-format-form
                                            'str
                                            `(- ,(if (integerp min)
                                                     min
                                                   'min)
                                                strlen)
                                            align)))))
                 (when (or (not (integerp max)) (> max 0))
                   (setq max-used t)
                   (cl-pushnew 'str vars-used)
                   ;; Generate code to limit the string to a maximum size.
                   (setq maxform `(progn
                                    (setq str
                                          ,(ibuffer-compile-make-substring-form
                                            'str
                                            (if (integerp max)
                                                max
                                              'max)
                                            from-end-p))
                                    (setq strlen (string-width str))
                                    (setq str
                                          ,(ibuffer-compile-make-eliding-form
                                            'str elide from-end-p)))))
                 ;; Now, put these forms together with the rest of the code.
                 (let ((callform
                        ;; Is this an "inline" column?  This means we have
                        ;; to get the code from the
                        ;; `ibuffer-inline-columns' alist and insert it
                        ;; into our generated code.  Otherwise, we just
                        ;; generate a call to the column function.
                        (ibuffer-aif (assq sym ibuffer-inline-columns)
                            (nth 1 it)
                          `(,sym buffer mark)))
                       ;; You're not expected to understand this.  Hell, I
                       ;; don't even understand it, and I wrote it five
                       ;; minutes ago.
                       (insertgenfn
                        (if (get sym 'ibuffer-column-summarizer)
                            ;; I really, really wish Emacs Lisp had closures.
                            ;; FIXME: Elisp does have them now.
                            (lambda (arg sym)
                              `(insert
                                (let ((ret ,arg))
                                  (put ',sym 'ibuffer-column-summary
                                       (cons ret (get ',sym
                                                      'ibuffer-column-summary)))
                                  ret)))
                          (lambda (arg _sym)
                            `(insert ,arg))))
                       (mincompform `(< strlen ,(if (integerp min)
                                                    min
                                                  'min)))
                       (maxcompform `(> strlen ,(if (integerp max)
                                                    max
                                                  'max))))
                   (if (or min-used max-used)
                       ;; The complex case, where we have to limit the
                       ;; form to a maximum or minimum size.
                       (progn
                         (when (and min-used (not (integerp min)))
                           (push `(min ,min) letbindings))
                         (when (and max-used (not (integerp max)))
                           (push `(max ,max) letbindings))
                         (push
                          (if (and min-used max-used)
                              `(if ,mincompform
                                   ,minform
                                 (if ,maxcompform
                                     ,maxform))
                            (if min-used
                                `(when ,mincompform
                                   ,minform)
                              `(when ,maxcompform
                                 ,maxform)))
                          outforms)
                         (push `(setq str ,callform
                                      ,@(when strlen-used
                                          `(strlen (string-width str))))
                               outforms)
                         (setq outforms
                               (append outforms
                                       (list (funcall insertgenfn 'str sym)))))
                     ;; The simple case; just insert the string.
                     (push (funcall insertgenfn callform sym) outforms))
                   ;; Finally, return a `let' form which binds the
                   ;; variables in `letbindings', and contains all the
                   ;; code in `outforms'.
                   `(let ,letbindings
                      ,@outforms)))))
           result))
        ;; We don't want to unconditionally load the byte-compiler.
        (funcall (if (or ibuffer-always-compile-formats
                         (featurep 'bytecomp))
                     #'byte-compile
                   #'identity)
                 ;; Here, we actually create a lambda form which
                 ;; inserts all the generated forms for each entry
                 ;; in the format string.
                 `(lambda (buffer mark)
                    (let ,vars-used
                      ,@(nreverse result))))))

    (defun ibuffer-recompile-formats ()
      "Recompile `ibuffer-formats'."
      (interactive)
      (setq ibuffer-compiled-formats
            (mapcar #'ibuffer-compile-format ibuffer-formats))
      (when (boundp 'ibuffer-filter-format-alist)
        (setq ibuffer-compiled-filter-formats
              (mapcar (lambda (entry)
                        (cons (car entry)
                              (mapcar (lambda (formats)
                                        (mapcar #'ibuffer-compile-format formats))
                                      (cdr entry))))
                      ibuffer-filter-format-alist))))

    (defun ibuffer-clear-summary-columns (format)
      (dolist (form format)
        (when (and (consp form)
                   (get (car form) 'ibuffer-column-summarizer))
          (put (car form) 'ibuffer-column-summary nil))))

    (defun ibuffer-check-formats ()
      (when (null ibuffer-formats)
        (error "No formats!"))
      (let ((ext-loaded (featurep 'ibuf-ext)))
        (when (or (null ibuffer-compiled-formats)
                  (null ibuffer-cached-formats)
                  (not (eq ibuffer-cached-formats ibuffer-formats))
                  (null ibuffer-cached-eliding-string)
                  (not (equal ibuffer-cached-eliding-string ibuffer-eliding-string))
                  (eql 0 ibuffer-cached-elide-long-columns)
                  (not (eql ibuffer-cached-elide-long-columns
                            (with-no-warnings ibuffer-elide-long-columns)))
                  (and ext-loaded
                       (not (eq ibuffer-cached-filter-formats
                                ibuffer-filter-format-alist))
                       (and ibuffer-filter-format-alist
                            (null ibuffer-compiled-filter-formats))))
          (message "Formats have changed, recompiling...")
          (ibuffer-recompile-formats)
          (setq ibuffer-cached-formats ibuffer-formats
                ibuffer-cached-eliding-string ibuffer-eliding-string
                ibuffer-cached-elide-long-columns (with-no-warnings ibuffer-elide-long-columns))
          (when ext-loaded
            (setq ibuffer-cached-filter-formats ibuffer-filter-format-alist))
          (message "Formats have changed, recompiling...done"))))

Another weird one is that the extracted autoloads for
``ibuffer-ext.el`` reside in ``ibuffer.el``, but that's the lesser
evil of the two.

Credits go to holomorph_ for discovering `that maintenance nightmare`_.

.. _a customizable: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/ibuffer.el?id=43ec6efa2b41b43a2e55be16434f64bba644271e#n145
.. _that maintenance nightmare: http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/ibuffer.el?id=43ec6efa2b41b43a2e55be16434f64bba644271e#n1554
.. _holomorph: https://github.com/holomorph
