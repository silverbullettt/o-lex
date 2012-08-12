#lang racket

(require "nfa.rkt" "utility.rkt")
(provide regex-to-nfa)

(define (char-range low high)
  (map integer->char (range (char->integer low)
                            (+ (char->integer high) 1))))

(define *digit* (char-range #\0 #\9))
(define *downcase* (char-range #\a #\z))
(define *upcase* (char-range #\A #\Z))
(define *alphabet* (append *downcase* *upcase*))
(define *special* '(#\| #\\ #\[ #\] #\( #\) #\+ #\*))

(define *wildcard-char* '(#\d #\c))
(define *wildcard-map* (make-hash '((#\d . dight) (#\c . char))))
(define (wildcard? c)
  (if (member c *wildcard-char*) #t #f))

(define *dup-char* '(#\* #\+))
(define *dup-map* (make-hash '((#\* . dup0) (#\+ . dup1))))

(define *escape* #\~)
(define (escape? c) (char=? c *escape*))

; TODO list:
; 0. design
; 1. parse regex
; 2. meta-character(|,~d,~c,[,],(,),*,+)
; 3. or(|) OK
; 4. wildcard(~d,~c) OK
; 5. optional([]) OK
; 6. duplication(()*,()+) OK

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
              [#\| (match s ; split
                     ['in-plain
                      (list 'or (substring regex 0 i) (substring regex (+ i 1)) "")]
                     ['in-escape
                      (parse-iter (string-append bs "|") "" (+ i 1) 'in-plain)])]
              [#\[ (match s ; left bracket of optional
                     ['in-escape
                      (parse-iter (string-append bs "[") "" (+ i 1) 'in-plain)]
                     ['in-plain
                      (let ([last (string-find-last regex #\])])
                        (list 'opt
                              bs
                              (substring regex (+ i 1) last)
                              (substring regex (+ last 1))))])]
              [#\( (match s ; left bracket of duplication
                     ['in-escape
                      (parse-iter (string-append bs "(") "" (+ i 1) 'in-plain)]
                     ['in-plain
                      (let* ([last (string-find-last regex #\))]
                             [dup-char (string-ref regex (+ last 1))])
                        (list (hash-ref *dup-map* dup-char)
                              bs
                              (substring regex (+ i 1) last)
                              (substring regex (+ last 2))))])]
              [(? escape?) (match s ; escape
                             ['in-plain
                              (parse-iter bs "" (+ i 1) 'in-escape)]
                             ['in-escape
                              (parse-iter (string-append bs "~") "" (+ i 1) 'in-plain)])]
              [(? wildcard?) (match s ; wildcard
                                 ['in-plain
                                  (parse-iter (string-append bs (string c)) "" (+ i 1) s)]
                                 ['in-escape
                                  (list (hash-ref *wildcard-map* c)
                                        bs
                                        (substring regex (+ i 1))
                                        "")])]
              [_ 'in-plain
                    (parse-iter (string-append bs (string c)) "" (+ i 1) s)]))))
    (parse-iter "" "" 0 'in-plain)))

(define (regex-to-nfa regex) #f)


(regex-parser "cnxkfhasdkhkfd")
(regex-parser "cnxkfh|asdkhkfd")
(regex-parser "cnx~~k~~fhasdkhkfd")
(regex-parser "cnx~~k~~fhasd~ckhkfd")
(regex-parser "abc[ef]xyz")
(regex-parser "123(ab)+xyz")
(regex-parser "123(ab)*xyz")