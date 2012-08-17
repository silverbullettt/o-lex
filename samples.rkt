#lang racket

(require "nfa.rkt" "regex-parser.rkt" "nfa-to-dfa.rkt" "lex-parser.rkt")

(define n (regex->nfa "(~+|-)?~d+(.~d+)?(e(~+|-)?~d+(.~d+)?)?"))
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

(define cmt (regex-matcher "/~*([^*]|~*+[^*/])*~*+/"))
(cmt "void /*main**int**/ main(int argc,..*/.)/*wocao*/")

(define num (regex-matcher "(~+|-)?~d+(.~d+)?(e(~+|-)?~d+(.~d+)?)?"))
(num "sdkfhiwuh13242352sdjkvhxcj")
(num "sdkfhiwuh13242.sdjkvhxcj")
(num "++13242352.15431")
(newline)


; latter rule has higher priority- -
(define lp (make-lex-parser '(("write" write)
                              ("(_|~c)(_|~c|~d)*" id)
                              ("(~+|-)?~d+(.~d+)?(e(~+|-)?~d+(.~d+)?)?" num)
                              ("~+" plus)
                              ("=" assign)
                              ("~(" lbrac)
                              (")" rbrac)
                              (";" semi)
                              ("input" input)
                              ("output" output))))

(lp "x = 123 + y")
(lp "x = 123.89 + 456")
(lp "x = 123+456")
(lp "inpu")
(lp "input")
(lp "input1")
(lp "put()")
(lp "write(x)")
(lp "x = 123; output(x); x = input()")
