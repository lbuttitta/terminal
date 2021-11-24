#lang racket/base

(require racket/contract
         racket/match
         racket/string
         racket/system)

(unless (member (system-type) '(unix macosx))
  (error "posix.rkt cannot be required on non-POSIX systems"))

(define-syntax-rule (with-output-port (id expr) body0 body ...)
  (let ([id expr])
    (with-handlers ([values
                     (λ (e)
                       (close-output-port id)
                       (raise e))])
      (begin0
        (begin body0 body ...)
        (close-output-port id)))))
(define-syntax with-output-ports
  (syntax-rules ()
    [(_ ([id expr]) body0 body ...)
     (with-output-port (id expr)
       body0 body ...)]
    [(_ ([id0 expr0] [id expr] ...) body0 body ...)
     (with-output-port (id0 expr0)
       (with-output-ports ([id expr] ...)
         body0 body ...))]))

(provide no-nuls?)
(define no-nuls? (or/c string-no-nuls? bytes-no-nuls?))

(define (system/output command)
  (with-output-ports ([out (open-output-string)]
                      [err (open-output-string)])
      (parameterize ([current-output-port out]
                     [current-error-port  err])
        (define success? (system command))
        (define out-str (get-output-string out))
        (define err-str (get-output-string err))
        (string-trim
         (if (or success?
                 (equal? err-str ""))
             out-str
             err-str)
         "\n"
         #:left? #f))))

(define (system/no-output command)
  (with-output-ports ([out (open-output-string)]
                      [err (open-output-string)])
    (parameterize ([current-output-port out]
                   [current-error-port  err])
      (system command))))

(define (system+ command . args)
  (system (string-join (cons command args))))

(define (system+/output command . args)
  (system/output (string-join (cons command args))))

(define (system+/no-output command . args)
  (system/no-output (string-join (cons command args))))

(provide/contract
 [tty-available?
  (-> boolean?)])
(define (tty-available?)
  (system/no-output "tty"))

(provide/contract
 [stty-get-size
  (-> (or/c #f (list/c exact-nonnegative-integer? exact-nonnegative-integer?)))])
(define (stty-get-size)
  (match (system/output "stty size")
    [(regexp #px"(\\d+) (\\d+)" (list _ h w))
     (list (string->number w) (string->number h))]
    [_ #f]))

(provide/contract
 [stty-get-options
  (-> no-nuls?)])
(define (stty-get-options)
  (system/output "stty -g"))

(provide/contract
 [stty-set-options
  (-> no-nuls?
      boolean?)])
(define (stty-set-options options)
  (system+/no-output "stty" options))

(provide/contract
 [stty-reset-options
  (-> boolean?)])
(define (stty-reset-options)
  (system/no-output "stty sane"))

(provide/contract
 [tty-cook
  (-> boolean?)])
(define (tty-cook)
  (system/no-output "stty cooked"))

(provide/contract
 [tty-uncook
  (-> boolean?)])
(define (tty-uncook)
  (system/no-output "stty raw"))

(provide/contract
 [tty-enable-echo
  (-> boolean?)])
(define (tty-enable-echo)
  (system/no-output "stty echo"))

(provide/contract
 [tty-disable-echo
  (-> boolean?)])
(define (tty-disable-echo)
  (system/no-output "stty -echo"))

(provide saved-stty-options)
(define saved-stty-options (make-parameter #f))

(provide/contract
 [setup-tty
  (-> any)])
(define (setup-tty)
  (unless (tty-available?)
    (error 'setup-terminal "must be called in a terminal environment"))
  (unless (saved-stty-options) ; minimize impact of improper nesting
    (saved-stty-options (stty-get-options)))
  (tty-uncook)
  (file-stream-buffer-mode (current-output-port) 'none)
  (tty-disable-echo))

(provide/contract
 [restore-tty
  (-> any)])
(define (restore-tty)
  (when (saved-stty-options)
    (stty-set-options (saved-stty-options))))
