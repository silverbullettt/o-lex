#lang racket

(require "table.rkt" "nfa.rkt")

(define (test nfa str)
  (printf "\"~a\" -> ~a~%" str ((nfa 'recog) str)))

(define t
  (make-trans '(0 #\a (1))
              '(0 *ε* (2))
              '(1 #\b (0 1))))

(define NFA (make-nfa
             '(0 1 2)
             '(#\a #\b)
             t
             0
             '(1 2)))

(test NFA "a")
(test NFA "ab")
(test NFA "abbbb")
(test NFA "abbaabb")
(test NFA "")

(define N1 (make-plain-nfa "wocao"))
(define N2 (make-plain-nfa "fuck"))
(define N3 (make-plain-nfa "shit"))
(define N4 (nfa-union N1 N2 N3))
((N4 'recog) "wocao")
((N4 'recog) "fuck")
((N4 'recog) "shit")
(table->list (N4 'T))
(N4 'init)
(N4 'F)
((N4 'recog) "other")

(define N5 (make-plain-nfa "ab"))
(define N6 (nfa-star-closure N5))
(define N7 (nfa-positive-closure N5))