#lang racket

(require "nfa.rkt" "regex-parser.rkt" "nfa-to-dfa.rkt" "lex-parser.rkt")

(define t (make-regex-recognizer "(~+|-)?~d+(.~d+)?(e(~+|-)?~d+(.~d+)?)?"))

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

(define cmt (make-regex-matcher "/~*([^*]|~*+[^*/])*~*+/"))
(cmt "void /*main**int**/ main(int argc,..*/.)/*wocao*/")

(define num (make-regex-matcher "(~+|-)?~d+(.~d+)?(e(~+|-)?~d+(.~d+)?)?"))
(num "sdkfhiwuh13242352sdjkvhxcj")
(num "sdkfhiwuh13242.sdjkvhxcj")
(num "++13242352.15431")
(newline)

(define cmt-filter 'comment)

; latter rule has higher priority- -
(define lp (make-lex-parser '(("//[^\n]*" comment)
                              ("(_|~c)(_|~c|~d)*" id)
                              ("~d+(.~d+)?(e(~+|-)?~d+(.~d+)?)?" num)
                              ("\"[^\"]*\"" string)
                              ("~+" plus)
                              ("=" assign)
                              ("~(" lbrac)
                              (")" rbrac)
                              (";" semi)
                              ("input" input)
                              ("output" output))
                            'comment
                            'string))

(lp "x = 123 + y;
output(x); \"a string\"// this is a comment
input(); s = \"hello,world.\"
\"another string\"")
(lp "x = 123.89 + 456")
(lp "x = 1 + 2;output(x);")
(lp "inpu")
(lp "input")
(lp "input1")
