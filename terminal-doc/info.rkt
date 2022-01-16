#lang info

(define collection 'multi)

(define deps '("base"))

(define build-deps '("racket-doc"
                     "scribble-lib"
                     "terminal-lib"))

(define update-implies '("terminal-lib"))

(define pkg-desc "A Racket library for manipulating the terminal.")

(define pkg-authors '(lbuttitta))

