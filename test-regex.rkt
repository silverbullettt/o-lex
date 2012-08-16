#lang racket

(require "nfa.rkt" "regex-to-nfa.rkt" "nfa-to-dfa.rkt" "lex-parser.rkt")

(define n (regex->nfa "(+|-)?(~d)+(.(~d)+)?(e(+|-)?(~d)+(.(~d)+)?)?"))
(define (t s) ((n 'recog) s))


(t "123e456")
(t "123")
(t "0.123")
(t "+123e-456")
(t "123e456")
(t "123.7e456")
(t "67.12e+89.13")

(newline)
(t "123ee12")
(t "123e")
(t ".45")

;(define cmt (regex-matcher "/*([^*]|(*)+[^*/])*(*)+/"))
;(cmt "void /*main**int**/ main(int argc,..*/.)")

(define num (regex-matcher "(+|-)?(~d)+(.(~d)+)?(e(+|-)?(~d)+(.(~d)+)?)?"))
(num "sdkfhiwuh13242352sdjkvhxcj")
(num "sdkfhiwuh13242.sdjkvhxcj")
(num "++13242352.15431")
(newline)

(define lp (make-lex-parser '(("(_|~c)(_|~c|~d)*" id)
                              ("(+|-)?(~d)+(.(~d)+)?(e(+|-)?(~d)+(.(~d)+)?)?" num)
                              ("+" plus)
                              ("=" equal))))

(lp "x = 123 + y")
(lp "x = 123 + 456")
(lp "x = 123+456")