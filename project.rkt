#lang racket/base

(require (for-syntax racket/base
                     racket/dict)
         racket/dict
         web-server/servlet
         web-server/servlet-env
         racket/list
         racket/string
         racket/rerequire
         net/url)

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         dict-map
         build
         make-build-all)

(begin-for-syntax
  (define (syntax->input-string expr)
    (let ([input (symbol->string (syntax-e expr))])
      (substring input 0 (- (string-length input) 1))))
  (define (input-id? expr)
    (if (symbol? (syntax-e expr))
        (let ([input (symbol->string (syntax-e expr))])
          (eq? (string-ref input (- (string-length input) 1)) #\:))
        #f)))

(define (build in out)
  (let-values ([(output-path output-name is-dir) (split-path out)]
               [(input-text) (dynamic-require in 'output)])
    (unless (directory-exists? output-path)
      (make-directory output-path))
    (with-output-to-file out
      (λ ()
        (write-string input-text))
      #:exists 'truncate/replace)))

(define (make-build-all build-map)
  (λ (#:print-output [print-output #f])
    (when print-output
      (printf "Building project...\n\n"))
    (for ([sources build-map])
      (let ([in-path (car sources)]
            [out-path (cdr sources)])
        (when print-output
          (printf "Building ~a\n" (path->string out-path)))
        (build in-path out-path)))
    (when print-output
      (printf "\nDone."))))

(define (project-url path-str port)
  (let ([p-url (string->url path-str)])
    (struct-copy url
                 p-url
                 [scheme "http"]
                 [host "localhost"]
                 [port port])))

(define route-map (make-parameter #hash()))

(define-values (doc-dispatch doc-url)
  (dispatch-rules
   [((string-arg) ...) #:method m
                       (λ (req path)
                         (let* ([route (string-append "/" (string-join path "/"))]
                                [doc-path (hash-ref (route-map) route #f)])
                           (cond
                             [(path? doc-path)
                              (dynamic-rerequire doc-path
                                                 #:verbosity 'none)
                              (define request-processor
                                (dynamic-require doc-path
                                                 'process-request
                                                 (λ () (λ (req) #t))))
                              (define doc
                                (dynamic-require doc-path
                                                 'output
                                                 (λ ()  (λ () "Not found"))))
                              (define cleanup
                                (dynamic-require doc-path
                                                 'cleanup
                                                 (λ () (λ () #t))))
                              
                              (request-processor req)
                              (define doc-text (doc))
                              (cleanup)
                              (response/full
                               200 #"Okay"
                               (current-seconds) TEXT/HTML-MIME-TYPE
                               empty
                               (list (string->bytes/utf-8 doc-text)))]
                             [else
                              (response/full
                               404 #"Not found"
                               (current-seconds) TEXT/HTML-MIME-TYPE
                               empty
                               (list (string->bytes/utf-8 "Not found")))])))]))

(define (make-start-server host-map port visibility)
  (λ () (let ([urls (map (λ (hm) (project-url (car hm) port))
                         host-map)]
              [listen-ip (cond
                           [(eq? visibility 'local)
                            "127.0.0.1"]
                           [(eq? visibility 'global)
                            #f])])
          (for ([url urls])
            (printf "Hosting ~a\n"
                    (url->string url)))
          (printf "\n")
          (parameterize ([route-map (make-hash host-map)])
            (serve/servlet doc-dispatch
                           #:servlet-regexp #rx""
                           #:port port
                           #:listen-ip listen-ip
                           #:quit? #f
                           #:launch-browser? #f)))))

(define-syntax (|project builder| stx)
  (define (build-project exprs
                         [source-map #hash()]
                         [output-dir 'public]
                         [port 8000]
                         [visibility 'local])
    (cond
      [(null? exprs)
       #`(begin
           (define output-dir (string->path (symbol->string '#,output-dir)))
           (define build-map (dict-map
                              #,source-map
                              (λ (o i)
                                (cons (path->complete-path
                                       (string->path
                                        (string-append i ".rkt")))
                                      (path->complete-path
                                       (build-path output-dir
                                                   (string->path o)))))))
           (define host-map (dict-map
                             #,source-map
                             (λ (o i)
                               (cons (string-append "/" o)
                                     (path->complete-path
                                      (string->path
                                       (string-append i ".rkt")))))))                
           (define build-all (make-build-all build-map))
           (define start-server (make-start-server host-map #,port '#,visibility))
           (provide output-dir build-map host-map build-all start-server)
           (module* configure-runtime #f
                (start-server)))]
      [(keyword? (syntax-e (car exprs)))
       (cond
         [(eq? (syntax-e (car exprs)) '#:output)
          (build-project (cddr exprs)
                         source-map
                         (syntax-e (cadr exprs))
                         port
                         visibility)]
         [(eq? (syntax-e (car exprs)) '#:port)
          (build-project (cddr exprs)
                         source-map
                         output-dir
                         (syntax-e (cadr exprs))
                         visibility)]
         [(eq? (syntax-e (car exprs)) '#:visibility)
          (build-project (cddr exprs)
                         source-map
                         output-dir
                         port
                         (syntax-e (cadr exprs)))]
         [else (raise-syntax-error #f
                                   "Invalid configuration declaration"
                                   (car exprs))])]
      [(and (symbol? (syntax-e (car exprs)))
            (input-id? (car exprs)))
       (build-project (cddr exprs)
                      (dict-set source-map
                                (symbol->string (syntax-e (cadr exprs)))
                                (syntax->input-string (car exprs)))
                      output-dir
                      port
                      visibility)]
      [else (raise-syntax-error #f
                                "You gave me a declaration I don't understand"
                                (car exprs))]))
  (build-project (cdr (syntax->list stx))))

(define-syntax-rule (module-begin expr ...)
  (#%plain-module-begin
   (|project builder| expr ...)))
