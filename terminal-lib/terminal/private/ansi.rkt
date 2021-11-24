#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define/contract (move-cursor-up [d 1])
  (->* ()
       (exact-positive-integer?
        void?))
  (printf "\x1b[~aA" d))

(define/contract (move-cursor-down [d 1])
  (->* ()
       (exact-positive-integer?)
       void?)
  (printf "\x1b[~aB" d))

(define/contract (move-cursor-right [d 1])
  (->* ()
       (exact-positive-integer?)
       void?)
  (printf "\x1b[~aC" d))

(define/contract (move-cursor-left [d 1])
  (->* ()
       (exact-positive-integer?)
       void?)
  (printf "\x1b[~aD" d))

(define/contract (move-cursor-down-lines [d 1])
  (->* ()
       (exact-positive-integer?)
       void?)
  (printf "\x1b[~aE" d))

(define/contract (move-cursor-up-lines [d 1])
  (->* ()
       (exact-positive-integer?)
       void?)
  (printf "\x1b[~aF" d))

(define/contract (move-cursor-to-column [y 0])
  (->* ()
       (exact-nonnegative-integer?)
       void?)
  (printf "\x1b[~aH" (add1 y)))

(define/contract (move-cursor-by dx dy)
  (-> exact-integer?
      exact-integer?
      void?)
  (cond
    [(negative? dx) (move-cursor-left (- dx))]
    [(positive? dx) (move-cursor-right dx)])
  (cond
    [(negative? dy) (move-cursor-up (- dy))]
    [(positive? dy) (move-cursor-down dy)]))

(define/contract (move-cursor-to x y)
  (-> exact-nonnegative-integer?
      exact-nonnegative-integer?
      void?)
  (printf "\x1b[~a;~aH" (add1 y) (add1 x)))

(define/contract (erase-to-end)
  (-> void?)
  (display "\x1b[0J"))

(define/contract (erase-to-beginning)
  (-> void?)
  (display "\x1b[1J"))

(define/contract (erase-display)
  (-> void?)
  (display "\x1b[2J"))

(define/contract (erase-all)
  (-> void?)
  (display "\x1b[3J"))

(define/contract (erase-to-line-end)
  (-> void?)
  (display "\x1b[0K"))

(define/contract (erase-to-line-beginning)
  (-> void?)
  (display "\x1b[1K"))

(define/contract (erase-line)
  (-> void?)
  (display "\x1b[2K"))

(define/contract (scroll-up [d 1])
  (->* ()
       (exact-positive-integer?)
       void?)
  (printf "\x1b[~aS" d))

(define/contract (scroll-down [d 1])
  (->* ()
       (exact-positive-integer?)
       void?)
  (printf "\x1b[~aT" d))

(define/contract (show-cursor)
  (-> void?)
  (display "\x1b[?25h"))

(define/contract (hide-cursor)
  (-> void?)
  (display "\x1b[?25l"))

(define/contract (reset-attributes)
  (-> void?)
  (display "\x1b[0m"))

(define/contract (set-intensity intensity)
  (-> (or/c 'bold 'faint 'normal)
      void?)
  (printf
   "\x1b[~am"
   (case intensity
     [(bold)   1]
     [(faint)  2]
     [(normal) 22])))

(define/contract (set-underline underline)
  (-> (or/c 'single 'double 'none)
      void?)
  (printf
   "\x1b[~am"
   (case underline
     [(single) 4]
     [(double) 21]
     [(none)   24])))

(define/contract (set-blink blink)
  (-> (or/c 'slow 'fast 'none)
      void?)
  (printf
   "\x1b[~am"
   (case blink
     [(slow) 5]
     [(fast) 6]
     [(none) 25])))

(define/contract (enable-strikethrough)
  (-> void?)
  (display "\x1b[9m"))

(define/contract (disable-strikethrough)
  (-> void?)
  (display "\x1b[29m"))

(define/contract (set-frame frame)
  (-> (or/c 'framed 'encircled 'none)
      void?)
  (printf
   "\x1b[~am"
   (case frame
     [(framed)    51]
     [(encircled) 52]
     [(none)      54])))

(define/contract (enable-overline)
  (-> void?)
  (display "\x1b[53m"))

(define/contract (disable-overline)
  (-> void?)
  (display "\x1b[55m"))

(define/contract (reset-foreground-color)
  (-> void?)
  (display "\x1b[39m"))

(define/contract (reset-background-color)
  (-> void?)
  (display "\x1b[49m"))

(define/contract 4-bit-color?
  (-> any/c
      boolean?)
  (or/c 'black
        'dark-red
        'dark-green
        'dark-yellow
        'dark-blue
        'dark-magenta
        'dark-cyan
        'gray
        'dark-gray
        'red
        'green
        'yellow
        'blue
        'magenta
        'cyan
        'white))

(define/contract (set-foreground-color/4-bit color)
  (-> 4-bit-color?
      void?)
  (printf
   "\x1b[~am"
   (case color
     [(black)        30]
     [(dark-red)     31]
     [(dark-green)   32]
     [(dark-yellow)  33]
     [(dark-blue)    34]
     [(dark-magenta) 35]
     [(dark-cyan)    36]
     [(gray)         37]
     [(dark-gray)    90]
     [(red)          91]
     [(green)        92]
     [(yellow)       93]
     [(blue)         94]
     [(magenta)      95]
     [(cyan)         96]
     [(white)        97])))

(define (set-background-color/4-bit color)
  (-> 4-bit-color?
      void?)
  (printf
   "\x1b[~am"
   (case color
     [(black)        40]
     [(dark-red)     41]
     [(dark-green)   42]
     [(dark-yellow)  43]
     [(dark-blue)    44]
     [(dark-magenta) 45]
     [(dark-cyan)    46]
     [(gray)         47]
     [(dark-gray)    100]
     [(red)          101]
     [(green)        102]
     [(yellow)       103]
     [(blue)         104]
     [(magenta)      105]
     [(cyan)         106]
     [(white)        107])))

(define/contract (set-foreground-color/8-bit color)
  (-> byte?
      void?)
  (printf "\x1b[38;5;~am" color))

(define/contract (set-background-color/8-bit color)
  (-> byte?
      void?)
  (printf "\x1b[48;5;~am" color))

(define/contract (set-foreground-color/24-bit r g b)
  (-> byte?
      byte?
      byte?
      void?)
  (printf "\x1b[38;2;~a;~a;~am" r g b))

(define/contract (set-background-color/24-bit r g b)
  (-> byte?
      byte?
      byte?
      void?)
  (printf "\x1b[48;2;~a;~a;~am" r g b))

(define/contract (set-foreground-color color)
  (-> (or/c 4-bit-color? byte? (vector/c byte? byte? byte?))
      void?)
  (cond
    [(4-bit-color? color) (set-foreground-color/4-bit color)]
    [(byte? color) (set-foreground-color/8-bit color)]
    [else (apply set-foreground-color/24-bit (vector->list color))]))

(define/contract (set-background-color color)
  (-> (or/c 4-bit-color? byte? (vector/c byte? byte? byte?))
      void?)
  (cond
    [(4-bit-color? color) (set-background-color/4-bit color)]
    [(byte? color) (set-background-color/8-bit color)]
    [else (apply set-background-color/24-bit (vector->list color))]))
