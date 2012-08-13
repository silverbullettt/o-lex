#lang racket

; TODO list:
; 0. design
; 1. parse regex
; 2. meta-character(|,~d,~c,[,],(,),*,+)
; 3. or(|) OK
; 4. wildcard(~d,~c) OK
; 5. optional([]) OK
; 6. duplication(()*,()+) OK
; ------------- all above ok! ----------------
; 7. longest matching
; 8. token type

(require "nfa.rkt" "table.rkt" "utility.rkt")
(provide regex-parser regex->nfa)

(define (char-range low high)
  (map integer->char (range (char->integer low)
                            (+ (char->integer high) 1))))

(define *digit* (char-range #\0 #\9))
(define *downcase* (char-range #\a #\z))
(define *upcase* (char-range #\A #\Z))
(define *alphabet* (append *downcase* *upcase*))

(define *wildcard-char* '(#\d #\c))
(define *wildcard-map* (make-hash '((#\d . digit) (#\c . char))))
(define (wildcard? c)
  (if (member c *wildcard-char*) #t #f))

(define *dup-char* '(#\* #\+))
(define *dup-map* (make-hash '((#\* . dup0) (#\+ . dup1))))

(define *escape* #\~)
(define (escape? c) (char=? c *escape*))


(define (make-wildcard-nfa char-list)
  (make-nfa '(0 1)
            char-list
            (apply make-trans
                   (map (lambda (d) (list 0 d '(1))) char-list))
            0
            '(1)))
(define (make-digit-nfa) (make-wildcard-nfa *digit*))
(define (make-char-nfa) (make-wildcard-nfa *alphabet*))

(define (find-match-bracket str i lbrac rbrac)
  (let ([str-len (string-length str)])
    (define (find-iter i deep esc)
      (if (= i str-len)
          -1
          (let ([c (string-ref str i)])
            (cond [(escape? c)
                   (if esc 
                       (find-iter (+ i 1) deep #f)
                       (find-iter (+ i 1) deep #t))]
                  [(char=? c rbrac)
                   (if esc
                       (find-iter (+ i 1) deep #f)
                       (if (= deep 1) i (find-iter (+ i 1) (- deep 1) #f)))]
                  [(char=? c lbrac)
                   (if esc
                       (find-iter (+ i 1) deep #f)
                       (find-iter (+ i 1) (+ deep 1) #f))]
                  [else
                   (find-iter (+ i 1) deep #f)]))))
    (find-iter i 0 #f)))


(define (regex-parser regex)
  (let ([regex-len (string-length regex)])
    (define (parse-iter bs i s)
      ; bs -> string before special character
      ; i -> current character index
      ; s -> state
      (if (= i regex-len)
          (if (equal? s 'in-plain)
              (list 'plain "" bs "")
              (error 'regex-parser "illegal regular expression -- \"~a\"" regex))
          (let ([c (string-ref regex i)])
            (match c
              [#\| (match s ; split
                     ['in-plain
                      (list 'or (substring regex 0 i) "" (substring regex (+ i 1)))]
                     ['in-escape
                      (parse-iter (string-append bs "|") (+ i 1) 'in-plain)])]
              [#\[ (match s ; left bracket of optional
                     ['in-escape
                      (parse-iter (string-append bs "[") (+ i 1) 'in-plain)]
                     ['in-plain
                      (let ([mb (find-match-bracket regex i #\[ #\])])
                        (list 'opt
                              bs
                              (substring regex (+ i 1) mb)
                              (substring regex (+ mb 1))))])]
              [#\( (match s ; left bracket of duplication
                     ['in-escape
                      (parse-iter (string-append bs "(") (+ i 1) 'in-plain)]
                     ['in-plain
                      (let* ([mb (find-match-bracket regex i #\( #\))]
                             [dup-char (string-ref regex (+ mb 1))])
                        (list (hash-ref *dup-map* dup-char)
                              bs
                              (substring regex (+ i 1) mb)
                              (substring regex (+ mb 2))))])]
              [(? escape?) (match s ; escape
                             ['in-plain
                              (parse-iter bs (+ i 1) 'in-escape)]
                             ['in-escape
                              (parse-iter (string-append bs "~") (+ i 1) 'in-plain)])]
              [(? wildcard?) (match s ; wildcard
                                 ['in-plain
                                  (parse-iter (string-append bs (string c)) (+ i 1) s)]
                                 ['in-escape
                                  (list (hash-ref *wildcard-map* c)
                                        bs
                                        ""
                                        (substring regex (+ i 1)))])]
              [_ 'in-plain
                    (parse-iter (string-append bs (string c)) (+ i 1) s)]))))
    (parse-iter "" 0 'in-plain)))

(define (regex->nfa regex)
  (match (regex-parser regex)
      [(list type before content after)
       (match type
         ['plain (make-plain-nfa content)]
         ['or
          (nfa-union (make-plain-nfa before)
                     (regex->nfa after))]
         [else
          (let ([before-nfa (make-plain-nfa before)]
                [after-nfa (regex->nfa after)]
                [content-nfa (regex->nfa content)])
            (nfa-concate before-nfa
                         (match type
                           ['opt (nfa-union content-nfa (make-ε-nfa))]
                           ['dup0 (nfa-star-closure content-nfa)]
                           ['dup1 (nfa-positive-closure content-nfa)]
                           ['char (make-char-nfa)]
                           ['digit (make-digit-nfa)])
                         after-nfa))])]))
