#lang racket

(require "table.rkt" "nfa.rkt")

(define (test nfa str)
  (printf "\"~a\" -> ~a~%" str (nfa str)))

(define t
  (make-trans '(0 #\a (1))
              '(0 #\ε (2))
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