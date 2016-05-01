#lang s-exp syntax/module-reader

literally/static

#:read template-read
#:read-syntax template-read-syntax
#:whole-body-readers? #t
#:wrapper2 (λ (in rd stx?)
             (let* ([lang (read-line in 'any)]
                    [lang (if (eof-object? lang)
                              ""
                              (open-input-string lang))]
                    [sem (read lang)]
                    [sem (if (symbol? sem) sem 'text)]
                    [mod (if stx? (rd in) (datum->syntax #f (rd in) sem))]
                    [mod (add-semantic-processor mod sem)])
               (if stx? mod (syntax->datum mod))))
#:info (λ (key default default-filter)
         (case key
           [(color-lexer)
            (dynamic-require 'literally/static/lang/lex
                             'template-lexer)]
           [else
            (default-filter key default)]))

(require (prefix-in scribble: scribble/reader)
         (for-syntax racket/base))

(define (template-read in)
  (syntax->datum
   (template-read-syntax #f in)))

(define (template-read-syntax source-name in)
  (scribble:read-syntax-inside source-name
                               in
                               #:command-char #\λ))

(define (add-semantic-processor mod sem)
  (syntax-case mod ()
    [(module name lang (mod-begin . body*))
     (with-syntax ([body
                    (datum->syntax
                     #'body* (cons
                              sem
                              (syntax-e #'body*)))])
       (syntax/loc mod (module name lang (mod-begin . body))))]))
