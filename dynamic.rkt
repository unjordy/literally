#lang racket/base

(require (only-in scribble/decode
                  whitespace?)
         (for-syntax racket/base
                     (only-in scribble/decode
                              whitespace?))
         racket/string
         literally/semantic
         literally/utils
         literally/module-utils)

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%app)
         (rename-out [module-begin #%module-begin]
                     [app #%app])
         (all-from-out racket/string)
         (all-from-out literally/semantic)
         (all-from-out literally/utils)
         whitespace?
         is-static?)

(define is-static? #f)

(define-syntax (|template engine| stx)
  (define context (syntax-local-context))
  (define (process-template sem exprs definitions templates)
    (if (null? exprs)
        (let ([defs (reverse definitions)]
              [temps (reverse templates)])
          #`(begin
              #,@defs
              (define-namespace-anchor ns-anchor)
              (define (text) (|template applicator| #,@temps))
              (define (output) ((hash-ref (known-semantic-processors)
                                          #,sem) (text) (known-html-tags
                                                         (namespace-anchor->namespace
                                                          ns-anchor))))
              (provide text output)
              (module* configure-runtime #f
                (display (output)))))
        (let ([expr* (local-expand (car exprs) context stoplist)])
          (cond
            [(definition? expr* context)
             (process-template sem
                               (kill-whitespace (cdr exprs))
                               (cons expr* definitions)
                               templates)]
            [else
             (process-template
              sem (cdr exprs) definitions (cons expr* templates))]))))
  (let ([sl (syntax->list stx)])
    (process-template (syntax-e (cadr sl)) (cddr sl) '() '())))

(define-syntax-rule (module-begin semantic-processor expr ...)
  (#%plain-module-begin
   (|template engine| 'semantic-processor expr ...)))
