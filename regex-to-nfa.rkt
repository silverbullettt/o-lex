#lang racket

(require "nfa.rkt" "utility.rkt")
(provide regex-to-nfa)

(define (char-range low high)
  (define (identity x) x)
  (define (next char) (integer->char (+ (char->integer char) 1)))
  (range->list identity char>? next low high))

(define digit (char-range #\0 #\9))
(define downcase-char (char-range #\a #\z))
(define upcase-char (char-range #\A #\Z))
(define all-char (append downcase-char upcase-char))

(define (regexp-to-nfa) '())