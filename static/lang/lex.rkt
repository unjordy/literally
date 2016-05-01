#lang racket/base

(require syntax-color/scribble-lexer
         syntax-color/racket-lexer)

(provide template-lexer)

(define template-lexer (make-scribble-inside-lexer
                        #:command-char #\λ))
