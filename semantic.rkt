#lang racket/base

(require (for-syntax racket/base
                     racket/syntax))

(provide known-semantic-processors
         tag
         define-tag
         known-html-tags)

(define keep-comments (make-parameter #t))

(define known-semantic-processors
  (make-parameter (hash 'text (λ (doc tags) doc)
                        'html (λ (doc tags)
                                (define semantic-read-html
                                  (dynamic-require 'literally/semantic/html
                                                   'semantic-read-html))
                                (semantic-read-html doc
                                                    (hash-ref tags 'html '#hasheq())
                                                    (keep-comments))))))

(begin-for-syntax
  (define (tag-syntax type id)
    (format-id id "literally-~a-tag-~a" (syntax-e type) (syntax-e id))))

(define-syntax (tag stx)
  (syntax-case stx ()
    [(tag type id)
     (tag-syntax #'type #'id)]))

(define-syntax (define-tag stx)
  (syntax-case stx ()
    [(define-tag (id args ...) xexprs ...)
     #`(begin
         (define (#,(tag-syntax #'html #'id) args ...
                                           #,(syntax-local-introduce #'children))
           (begin xexprs ...)))]
    [(define-tag id xexprs ...)
     #`(begin
         (define (#,(tag-syntax #'html #'id) #,(syntax-local-introduce #'children))
           (begin xexprs ...)))]))

(define (known-html-tags ns)
  (let ([prefix-regex #rx"literally-html-tag-(.*)"])
    (hasheq 'html
            (for/hasheq ([sym (namespace-mapped-symbols ns)]
                         #:when (regexp-match? prefix-regex
                                               (symbol->string sym)))
              (values (string->symbol
                       (cadr (regexp-match prefix-regex
                                           (symbol->string sym))))
                      (namespace-variable-value sym #t #f ns))))))
