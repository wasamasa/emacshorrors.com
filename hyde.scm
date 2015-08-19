(use hyde
     hyde-atom
     irregex
     html-parser
     sxpath
     sxml-serializer
     posix
     srfi-1)

(define (rst->html)
  (let* ((rst
          (make-external-translator
           "rst2html"
           (lambda () (list "--link-stylesheet" "--smart-quotes=yes"
                            "--trim-footnote-reference-space"))))
         (dom (with-input-from-string (with-output-to-string rst) html->sxml))
         (html (serialize-sxml ((sxpath "//body/div/node()") dom) indent: #f)))
      (display html)))

(translators (cons (list "rst" rst->html) (translators)))

(define post-regex '(: "posts/" (+ any) ".rst"))
(define page-regex '(: (+ any) (or ".rst" ".sxml")))

(default-page-vars `((,post-regex (layouts "post.sxml" "default.sxml"))
                     (,page-regex (layouts "page.sxml" "default.sxml"))))

(define $ (environment-ref (page-eval-env) '$))

(define (filter-posts)
  (filter (lambda (page) (irregex-match post-regex (car page)))
          (pages)))

(define (sort-by-date posts)
  (sort posts (lambda (a b) (string>=? ($ 'date (cdr a))
                                       ($ 'date (cdr b))))))

(define (all-posts)
  (map cdr (sort-by-date (filter-posts))))

(define max-posts 5)
(define (latest-posts)
  (let ((posts (all-posts)))
    (if (<= (length posts) max-posts)
        posts
        (take posts max-posts))))

;; helpers

(define (pretty-date date)
  (time->string (string->time date "%Y-%m-%d %H:%M:%S") "%d/%m/%Y"))

(define (archive-date date)
  (time->string (string->time date "%Y-%m-%d %H:%M:%S") "%Y-%m-%d"))

(define (post-url post)
  (format "/posts/~a.html" (pathify ($ 'title post))))
