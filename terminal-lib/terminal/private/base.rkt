#lang racket/base

(require racket/contract
         racket/function
         racket/port
         racket/runtime-path
         (for-syntax racket/base
                     racket/function
                     syntax/parse))

(provide terminal-available?
         get-terminal-size
         setup-terminal
         restore-terminal
         with-terminal
         with-writes-queued)

(define/contract (case/system-type
                  posix-val
                  windows-val
                  [else-val #f])
  (->* (any/c any/c)
       (any/c)
       any/c)
  (case (system-type)
    [(unix macosx) posix-val]
    [(windows)     windows-val]
    [else          else-val]))

(define-runtime-path posix.rkt "posix.rkt")
(define-runtime-path windows.rkt "windows.rkt")
(define module-to-load (case/system-type posix.rkt windows.rkt))

(define terminal-available?
  (dynamic-require
   module-to-load
   (case/system-type 'tty-available? 'console-available?)))

(define get-terminal-size
  (dynamic-require
   module-to-load
   (case/system-type 'stty-get-size 'console-get-size)))

(define setup-terminal
  (dynamic-require
   module-to-load
   (case/system-type 'setup-tty 'setup-console)))

(define restore-terminal
  (dynamic-require
   module-to-load
   (case/system-type 'restore-tty 'restore-console)))

(define-syntax (with-terminal stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(call-with-exception-handler
        (λ (e)
          (restore-terminal)
          e)
        (thunk
         (setup-terminal)
         body ...
         (restore-terminal)))]))

(define-syntax (with-writes-queued stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(begin
         (display
          (with-output-to-string
            (thunk body ...)))
         (flush-output))]))

