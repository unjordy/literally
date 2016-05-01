#lang racket/base

(require racket/string)

(provide current-indent-level
         tab
         t)

(define current-indent-level (make-parameter 0))

(define (tab . text)
  (parameterize ([current-indent-level (+ (current-indent-level) 1)])
    (let ([indent (make-string (current-indent-level) #\tab)])
      (string-append indent
                     (string-replace (string-join text "")
                                     "\n"
                                     (string-append "\n" indent))))))

(define t tab)
