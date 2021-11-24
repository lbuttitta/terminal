#lang racket/base

(require racket/contract
         racket/function
         racket/list
         (rename-in ffi/unsafe
                    [-> _>])
         ffi/unsafe/define)

(unless (equal? (system-type) 'windows)
  (error "windows.rkt cannot be required on non-Windows systems"))

(define-ffi-definer define-kernel32
  (ffi-lib "kernel32.dll"
           #:get-lib-dirs (thunk (list "C:\\Windows\\System32"))))

(define-kernel32 GetStdHandle
  (_fun _int
        _> _intptr))

(define-kernel32 GetConsoleMode
  (_fun _intptr
        [result : (_ptr o _uint32)]
        _> _bool
        _> result))

(define-kernel32 SetConsoleMode
  (_fun _intptr
        _uint32
        _> _bool))

(define _COORD (_list-struct _short _short))

(define _SMALL_RECT (_list-struct _short _short _short _short))

(define _CONSOLE_SCREEN_BUFFER_INFO
  (_list-struct
   _COORD
   _COORD
   _word
   _SMALL_RECT
   _COORD))

(define-kernel32 GetConsoleScreenBufferInfo
  (_fun _intptr
        [result : (_ptr o _CONSOLE_SCREEN_BUFFER_INFO)]
        _> _bool
        _> result))

(provide uint32?)
(define uint32? (integer-in 0 4294967295))

(provide int16?)
(define int16? (integer-in -32768 32767))

(provide/contract
 [console-available?
  (-> boolean?)])
(define (console-available?)
  (and (positive? (GetStdHandle -10))
       (positive? (GetStdHandle -11))))

(provide/contract
 [console-get-size
  (-> (vector/c int16? int16?))])
(define (console-get-size)
  (list->vector (first (GetConsoleScreenBufferInfo (GetStdHandle -11)))))

(provide/contract
 [console-get-input-mode
  (-> uint32?)])
(define (console-get-input-mode)
  (GetConsoleMode (GetStdHandle -10)))

(provide/contract
 [console-get-output-mode
  (-> uint32?)])
(define (console-get-output-mode)
  (GetConsoleMode (GetStdHandle -11)))

(provide/contract
 [console-set-input-mode
  (-> uint32?
      boolean?)])
(define (console-set-input-mode mode)
  (SetConsoleMode (GetStdHandle -10) mode))

(provide/contract
 [console-set-output-mode
  (-> uint32?
      boolean?)])
(define (console-set-output-mode mode)
  (SetConsoleMode (GetStdHandle -11) mode))

(provide/contract
 [console-get-input-attribute
  (-> uint32?
      boolean?)])
(define (console-get-input-attribute attrib)
  (define input-mode (console-get-input-mode))
  (and input-mode
       (positive? (bitwise-and input-mode attrib))))

(provide/contract
 [console-get-output-attribute
  (-> uint32?
      boolean?)])
(define (console-get-output-attribute attrib)
  (define input-mode (console-get-output-mode))
  (and input-mode
       (positive? (bitwise-and input-mode attrib) 0)))

(provide/contract
 [console-set-input-attribute
  (->* (uint32?)
       (boolean?)
       boolean?)])
(define (console-set-input-attribute attrib [on? #t])
  (console-set-input-mode
   (if on?
       (bitwise-ior (console-get-input-mode) attrib)
       (bitwise-and (console-get-input-mode) (bitwise-not attrib)))))

(provide/contract
 [console-set-output-attribute
  (->* (uint32?)
       (boolean?)
       boolean?)])
(define (console-set-output-attribute attrib [on? #t])
  (console-set-output-mode
   (if on?
       (bitwise-ior (console-get-output-mode) attrib)
       (bitwise-and (console-get-output-mode) (bitwise-not attrib)))))

(provide/contract
 [console-echo-enabled?
  (-> boolean?)])
(define (console-echo-enabled?)
  (console-get-input-attribute #x0002))

(provide/contract
 [console-enable-echo
  (-> boolean?)])
(define (console-enable-echo)
  (console-set-input-attribute #x0002 #t))

(provide/contract
 [console-disable-echo
  (-> boolean?)])
(define (console-disable-echo)
  (console-set-input-attribute #x0002 #f))

(provide/contract
 [console-input-buffering-enabled?
  (-> boolean?)])
(define (console-input-buffering-enabled?)
  (console-get-input-attribute #x0002))

(provide/contract
 [console-enable-input-buffering
  (-> boolean?)])
(define (console-enable-input-buffering)
  (console-set-input-attribute #x0004 #t))

(provide/contract
 [console-disable-input-buffering
  (-> boolean?)])
(define (console-disable-input-buffering)
  (console-set-input-attribute #x0004 #f))

(provide/contract
 [console-interrupts-enabled?
  (-> boolean?)])
(define (console-interrupts-enabled?)
  (console-get-input-attribute #x0002))

(provide/contract
 [console-enable-interrupts
  (-> boolean?)])
(define (console-enable-interrupts)
  (console-set-input-attribute #x0001 #t))

(provide/contract
 [console-disable-interrupts
  (-> boolean?)])
(define (console-disable-interrupts)
  (console-set-input-attribute #x0001 #f))

(provide/contract
 [console-ansi-escapes-enabled?
  (-> boolean?)])
(define (console-ansi-escapes-enabled?)
  (console-get-output-attribute #x0004))

(provide/contract
 [console-enable-ansi-escapes
  (-> boolean?)])
(define (console-enable-ansi-escapes)
  (console-set-output-attribute #x0004 #t))

(provide/contract
 [console-disable-ansi-escapes
  (-> boolean?)])
(define (console-disable-ansi-escapes)
  (console-set-output-attribute #x0004 #f))

(provide saved-console-input-mode)
(define saved-console-input-mode (make-parameter #f))

(provide saved-console-output-mode)
(define saved-console-output-mode (make-parameter #f))

(provide/contract
 [setup-console
  (-> any)])
(define (setup-console)
  (unless (console-available?)
    (error 'setup-terminal "must be called in a terminal environment"))
  (unless (and (saved-console-input-mode)
               (saved-console-output-mode))
    (saved-console-input-mode (console-get-input-mode))
    (saved-console-output-mode (console-get-output-mode)))
  (console-disable-input-buffering)
  (file-stream-buffer-mode (current-output-port) 'none)
  (console-disable-interrupts)
  (console-disable-echo)
  (console-enable-ansi-escapes))

(provide/contract
 [restore-console
  (-> any)])
(define (restore-console)
  (when (saved-console-input-mode)
    (console-set-input-mode (saved-console-input-mode)))
  (when (saved-console-output-mode)
    (console-set-output-mode (saved-console-output-mode))))
