#lang racket/base

(require (for-syntax racket/base
                     syntax/kerncase
                     (only-in scribble/decode
                              whitespace?)))

(provide (for-syntax definition?
                     stoplist
                     kill-whitespace)
         app
         |template applicator|)

(begin-for-syntax
  (define definition-ids ; ids that don't require forcing
    (syntax->list #'(define-values define-syntaxes begin-for-syntax
                      require provide #%require #%provide #%declare)))
  (define stoplist (append definition-ids (kernel-form-identifier-list)))
  (define (definition-id? id)
    (and (identifier? id)
         (ormap (λ (i) (free-identifier=? id i)) definition-ids)))
  (define (definition? x context)
    (syntax-case x ()
      [(id . rest) (cond
                     [(or (free-identifier=? #'id #'begin)
                          (free-identifier=? #'id #'begin0))
                      (ormap
                       (λ (e)
                         (definition? (local-expand e context stoplist) context))
                       (syntax-e #'rest))]
                     [else (and (definition-id? #'id) #'id)])]
      [_ #f]))
  (define (kill-whitespace exprs)
    (cond
      [(null? exprs) '()]
      [(string? (syntax-e (car exprs)))
       (if (whitespace? (syntax-e (car exprs)))
           (kill-whitespace (cdr exprs))
           exprs)]
      [else exprs])))

(define |template applicator| string-append)

(define-syntax-rule (app proc-expr arg ...)
  (cond
    [(string? proc-expr) (|template applicator| proc-expr arg ...)]
    [else (#%app proc-expr arg ...)]))
