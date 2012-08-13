#lang racket

(require "table.rkt" "nfa.rkt" "regex-to-nfa.rkt")

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
(define N4 (nfa-concate N1 N2 N3))
((N4 'recog) "wocaofuckshit")
((N4 'recog) "other")

(define N5 (make-plain-nfa "ab"))
(define N6 (nfa-star-closure N5))
(define N7 (nfa-positive-closure N5))


(define a (make-plain-nfa "a"))
(define b (nfa-star-closure (make-plain-nfa "1")))
(define c (make-plain-nfa "z"))


(regex-parser "cnxkfhasdkhkfd")
(regex-parser "cnxkfh|asdkhkfd")
(regex-parser "cnx~~k~~fhasdkhkfd")
(regex-parser "cnx~~k~~fhasd~ckhkfd")
(regex-parser "abc[ef]xyz")
(regex-parser "123(ab)+xyz")
(regex-parser "123(ab)*xyz")

(define n0 (regex->nfa "abc|def|xyz"))
(define n1 (regex->nfa "abc[de]xyz"))
(define n2 (regex->nfa "abc[d|~[e]xyz"))
(define n3 (regex->nfa "a(1)+z"))
(define n4 (regex->nfa "a(1)*z"))
(table->list (n3 'T))
(table->list (n4 'T))

