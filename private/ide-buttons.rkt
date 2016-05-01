#lang racket/base

(require racket/gui/base
         racket/class
         drracket/tool-lib
         drracket/syncheck-drracket-button)

(provide drracket-buttons)

(define (make-build-button label icon number)
  (list label
        icon
        (λ (editor-frame)
          (define filename (send
                            (send editor-frame get-definitions-text)
                            get-filename))
          (cond
            [filename
             (parameterize ([drracket:rep:after-expression
                             (λ ()
                               (define submod-builder
                                 (with-handlers ((exn:fail? (λ (x) #f)))
                                   (dynamic-require (eval #'(variable-reference->module-path-index
                                                              (#%variable-reference)))
                                                    'build-all)))
                               (define builder (or submod-builder
                                                   (with-handlers ((exn:fail? (λ (x) #f))) (eval 'build-all))))
                               (when builder
                                 (builder #:print-output #t)))])
               (send editor-frame execute-callback))]
            [else
             (message-box "Literally"
                          "Can't build until you save the project file.")]))))

(define (make-start-server-button label icon number)
  (list label
        icon
        (λ (editor-frame)
          (define filename (send
                            (send editor-frame get-definitions-text)
                            get-filename))
          (cond
            [filename
             (parameterize ([drracket:rep:after-expression
                             (λ ()
                               (define submod-builder
                                 (with-handlers ((exn:fail? (λ (x) #f)))
                                   (dynamic-require (eval #'(variable-reference->module-path-index
                                                              (#%variable-reference)))
                                                    'start-server)))
                               (define server (or submod-builder
                                                   (with-handlers ((exn:fail? (λ (x) #f))) (eval 'start-server))))
                               (when server
                                 (server)))])
               (send editor-frame execute-callback))]
            [else
             (message-box "Literally"
                          "Can't build until you save the project file.")]))))

(define drracket-buttons
  (let ([build-button (make-build-button "Build Project" syncheck-bitmap 99)]
        ;[server-button (make-start-server-button "Start Server" syncheck-bitmap 100)]
        )
    (list build-button
          ;server-button
          )))
