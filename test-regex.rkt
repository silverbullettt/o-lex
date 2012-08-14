#lang racket

(require "regex-to-nfa.rkt" "table.rkt" "nfa-to-dfa.rkt")

(define n (nfa->dfa (regex->nfa "[+|-](~d)+[.(~d)+][e[+|-](~d)+[.(~d)+]]")))
(define (t s) ((n 'recog) s))


(t "123e456")
(t "123")
(t "0.123")
(t "+123e-456")
(t "123e456")
(t "123.7e456")
(t "+67.12e+89.13")

(newline)
(t "123ee12")
(t "123e")
(t ".45")