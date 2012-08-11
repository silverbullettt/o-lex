#lang racket

(require "nfa.rkt" "utility.rkt")
(provide regex-to-nfa)

(define (char-range low high)
  (map integer->char (range (char->integer low)
                            (+ (char->integer high) 1))))

(define digit (char-range #\0 #\9))
(define downcase-char (char-range #\a #\z))
(define upcase-char (char-range #\A #\Z))
(define all-char (append downcase-char upcase-char))

; TODO list:
; 0. design
; 1. parse regex
; 2. meta-character(\d,\c,(,),[,],|,*,+)
; 3. or(|)
; 4. duplication(()*,()+)
; 5. optional([])

; 6. longest matching
; 7. token type
(define (regex-to-nfa) '())
