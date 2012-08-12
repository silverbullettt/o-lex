#lang racket

(require "nfa.rkt" "utility.rkt")
(provide regex-to-nfa)

(define (char-range low high)
  (map integer->char (range (char->integer low)
                            (+ (char->integer high) 1))))

(define digit (char-range #\0 #\9))
(define downcase (char-range #\a #\z))
(define upcase (char-range #\A #\Z))
(define alphabet (append downcase upcase))
(define special '(#\| #\\ #\[ #\] #\( #\) #\+ #\*))
(define escape #\~)

(define (special-char? c)
    (if (member c special) #t #f))

; TODO list:
; 0. design
; 1. parse regex
; 2. meta-character(|,~d,~c,[,],(,),*,+)
; 3. or(|)
; 4. wildcard(~d,~c)
; 5. optional([])
; 6. duplication(()*,()+)

; 7. longest matching
; 8. token type

(define (regex-parser regex)
  (let ([regex-len (string-length regex)])
    (define (parse-iter bs cs i s)
      ; bs -> string before special character
      ; cs -> string after first special character
      ; i -> current character index
      ; s -> state
      (if (= i regex-len)
          (if (equal? s 'in-plain)
              (list 'plain "" "" bs)
              (error 'regex-parser "illegal regular expression -- \"~a\"" regex))
          (let ([c (string-ref regex i)])
            (match c
              [#\| (list 'or (substring regex 0 i) (substring regex (+ i 1)) "")]
              [#\~ (match s 
                     ['in-plain (parse-iter bs "" (+ i 1) 'in-escape)]
                     ['in-escape 
                      (printf "~a~%" bs)
                      (parse-iter (string-append bs "~") "" (+ i 1) 'in-plain)])]
              [_ (parse-iter (string-append bs (string c)) "" (+ i 1) s)]))))
    (parse-iter "" "" 0 'in-plain)))

(define (regex-to-nfa regex) #f)
(regex-parser "cnxkfhasdkhkfd")
(regex-parser "cnxkfh|asdkhkfd")
(regex-parser "cnx~~k~~fhasdkhkfd")