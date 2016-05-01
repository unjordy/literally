#lang racket/base

(require html
         xml
         racket/match)

(provide semantic-read-html)

(define (attribute-pair? attr)
  (and (pair? attr)
       (symbol? (car attr))
       (string? (cadr attr))))

(define (apply-html-tags root tags)
  (match root
    [(list tag (list (? attribute-pair? args) ...) children ...)
     (let ([child-tags (for/list ([child children])
                         (apply-html-tags child tags))])
       ((hash-ref tags tag (λ () (λ (t) `(,tag ,args ,@t))))
        child-tags))]
    [(list tag children ...)
     (let ([child-tags (for/list ([child children])
                         (apply-html-tags child tags))])
       ((hash-ref tags tag (λ () (λ (t) `(,tag ,@t))))
        child-tags))]
    [other
     other]))

(define (semantic-read-html doc tags keep-comments)
  (parameterize ([empty-tag-shorthand html-empty-tags]
                 [read-html-comments keep-comments]
                 [read-comments keep-comments]
                 [collapse-whitespace #t]
                 [xexpr-drop-empty-attributes #t])
    (let* ([doc-port (open-input-string doc)]
           [doc-roots (read-html-as-xml doc-port)]
           [doc-roots (map (λ (r) (xml->xexpr r))
                           doc-roots)]
           [out (open-output-string)])
      (write-string "<!DOCTYPE html>\n" out)
      (for ([root doc-roots])
        (let ([processed-root (apply-html-tags root tags)])
          ;(write-xexpr root
          ;             out
          ;             #:insert-newlines? #f))
          (display-xml/content (xexpr->xml processed-root) out)))
      (get-output-string out))))
