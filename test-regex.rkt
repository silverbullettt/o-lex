#lang racket

(require "regex-to-nfa.rkt")

(define n (regex-to-nfa "[+|-](~d)+[.(~d)+][e[+|-](~d)+[.(~d)+]]"))
(define (t s) ((n 'recog) s))

(t "123e456")
(t "123")
(t "+123e-456")
(t "123e456")
(t "123.7e456")
(t "+67.12e+89.13")
