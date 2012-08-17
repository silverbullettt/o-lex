#lang racket

; TODO list:
; 0. design
; 1. parse regex
; 2. meta-character(|,~d,~c,(,),*,+,?)
; 3. or(|) BUG: when '|' is after a special char, something wrong
; 4. wildcard(~d,~c) OK
; 6. duplication(()*,()+,()?) OK
; ------------- all above ok! ----------------
; 7. longest matching
; 8. token type

(require "nfa.rkt" "nfa-to-dfa.rkt" "table.rkt" "utility.rkt")
(provide regex-parser regex->nfa regex-matcher)

(define (char-range low high)
  (map integer->char (range (char->integer low)
                            (+ (char->integer high) 1))))

(define *digit* (char-range #\0 #\9))
(define *downcase* (char-range #\a #\z))
(define *upcase* (char-range #\A #\Z))
(define *alphabet* (append *downcase* *upcase*))
(define *symbol* (string->list "`~!@#$%^&*()_-+=[]{}|\\,./?;:'\" \t\n"))

(define *wildcard-char* '(#\d #\c))
(define *wildcard-map* (make-hash '((#\d . digit) (#\c . char))))
(define (wildcard? c) (if (member c *wildcard-char*) #t #f))

(define *dup-char* '(#\* #\+ #\?))
(define *dup-map* (make-hash '((#\* . zero-or-more)
                               (#\+ . one-or-more)
                               (#\? . zero-or-one))))
(define (dup? c) (if (member c *dup-char*) #t #f))

(define *neg* #\^)
(define (neg? c) (char=? c *neg*))

(define *escape* #\~)
(define (escape? c) (char=? c *escape*))

(define *or* #\|)
(define (or? c) (char=? c *or*))

(define *spec-brac* (string->list "(["))
(define (spec-brac? c) (if (member c *spec-brac*) #t #f))
(define *brac-map* (make-hash '((#\( . #\))
                                (#\[ . #\]))))
(define (get-rbrac lbrac) (hash-ref *brac-map* lbrac))

(define (make-wildcard-nfa char-list)
  (make-nfa '(0 1)
            char-list
            (apply make-trans
                   (map (lambda (d) (list 0 d '(1))) char-list))
            0
            '(1)))
(define (make-digit-nfa) (make-wildcard-nfa *digit*))
(define (make-char-nfa) (make-wildcard-nfa *alphabet*))
(define (make-any-nfa) (make-wildcard-nfa 
                        (append *digit* *alphabet* *symbol*)))

(define (make-negchar-nfa char-list)
  ; make a nfa match any char but which in char-list
  (let* ([nfa (make-any-nfa)]
         [init (nfa 'init)]
         [T (nfa 'T)])
    (begin
      (for-each (lambda (c)
                  ((T 'insert!) init c '()))
                char-list)
      (make-nfa (nfa 'S)
                (remove* char-list (nfa 'alphabet))
                T
                init
                (nfa 'F)))))
     
(define (find-match-bracket str i lbrac)
  (let ([str-len (string-length str)]
        [rbrac (get-rbrac lbrac)])
    (define (find-iter i deep esc)
      (if (= i str-len)
          -1
          (let ([c (string-ref str i)])
            (cond [(escape? c)
                   (find-iter (+ i 1) deep (not esc))]
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

(define (or-regex regex)
  ; check if the regex is a or-regular expression
  (let ([regex-len (string-length regex)])
    (define (iter i esc)
      (if (= i regex-len)
          #f
          (let ([c (string-ref regex i)])
            (cond
              [(escape? c) (iter (+ i 1) (not esc))]
              [esc (iter (+ i 1) #f)]
              [(spec-brac? c) (iter (+ (find-match-bracket regex i c) 1) esc)]
              [(or? c) i]
              [else (iter (+ i 1) esc)]))))
    (iter 0 #f)))


(define (regex-parser regex)
  (let ([regex-len (string-length regex)]
        [or-reg (or-regex regex)]) ; or-reg -> is this regex a or-regexp?
    (define (parse-iter bs i s)
      ; bs -> string before special character
      ; i -> current character index
      ; s -> state
      (if (= i regex-len)
          (if (equal? s 'in-plain)
              (list 'plain "" bs "")
              (error 'regex-parser "illegal regular expression -- \"~a\"" regex))
          (let ([c (string-ref regex i)])
            (define (look-forward-dup)
              ; just invoked in escape, and c must be meta character
              (let ([next-c
                     (if (= i (- regex-len 1)) #f (string-ref regex (+ i 1)))])
                (if (dup? next-c)
                    (list (hash-ref *dup-map* next-c)
                          bs
                          (string *escape* c)
                          (substring regex (+ i 2)))
                    (parse-iter (string-append bs (string c)) (+ i 1) 'in-plain))))
            (match c
              [(? escape?) (match s ; escape
                             ['in-plain (parse-iter bs (+ i 1) 'in-escape)]
                             ['in-escape (look-forward-dup)])]
              [(? dup?) (match s ; duplcation
                          ['in-escape (look-forward-dup)]
                           ; (parse-iter (string-append bs (string c)) (+ i 1) 'in-plain)]
                          [else
                           (if (string-empty? bs)
                               (error "Fuck!")
                           (let ([bs-len (string-length bs)])
                             (list (hash-ref *dup-map* c)
                                   (substring bs 0 (- bs-len 1))
                                   (substring bs (- bs-len 1) bs-len)
                                   (substring regex (+ i 1)))))])]
              [(? wildcard?) (match s ; wildcard
                                 ['in-plain (look-forward-dup)]
                                  ;(parse-iter (string-append bs (string c)) (+ i 1) s)]
                                 ['in-escape
                                  (if (and (not (= i (- (string-length regex) 1)))
                                           (dup? (string-ref regex (+ i 1))))
                                      (list (hash-ref *dup-map* (string-ref regex (+ i 1)))
                                            bs
                                            (substring regex (- i 1) (+ i 1))
                                            (substring regex (+ i 2)))
                                      (list (hash-ref *wildcard-map* c)
                                            bs
                                            ""
                                            (substring regex (+ i 1))))])]
              [#\[ (match s ; character set
                     ['in-escape (look-forward-dup)]
                     ['in-plain
                      (let ([mb (find-match-bracket regex i #\[)])
                        (if (and (not (= mb (- (string-length regex) 1)))
                                 (dup? (string-ref regex (+ mb 1)))) ; dup-char
                            (list (hash-ref *dup-map* (string-ref regex (+ mb 1)))
                                  bs
                                  (substring regex i (+ mb 1))
                                  (substring regex (+ mb 2)))
                            (let* ([type (if (neg? (string-ref regex (+ i 1)))
                                             'neg-set
                                             'set)]
                                   [content (if (eq? type 'neg-set)
                                                (substring regex (+ i 2) mb)
                                                (substring regex (+ i 1) mb))]
                                   [after (substring regex (+ mb 1))])
                              (list type bs content after))))])]
              [#\( (match s ; left bracket of duplication
                     ['in-escape (look-forward-dup)]
                     ['in-plain
                      (let* ([mb (find-match-bracket regex i #\()]
                             [after (if (or (= mb (- (string-length regex) 1))
                                            (not (dup? (string-ref regex (+ mb 1)))))
                                        (substring regex (+ mb 1))
                                        (substring regex (+ mb 2)))]
                             [dup-char (if (= mb (- (string-length regex) 1))
                                           #f
                                           (string-ref regex (+ mb 1)))])
                        (list (hash-ref *dup-map* dup-char 'regexp)
                              bs
                              (substring regex (+ i 1) mb)
                              after))])]
              [else 'in-plain
                    (parse-iter (string-append bs (string c)) (+ i 1) s)]))))
    (if or-reg ; first, check whether the regex is or-regex
        (list 'or (substring regex 0 or-reg) "" (substring regex (+ or-reg 1)))
        (parse-iter "" 0 'in-plain))))

(define (regex->nfa regex)
  (match (regex-parser regex)
      [(list type before content after)
       (match type
         ['plain (make-plain-nfa content)]
         ['or
          (nfa-union (regex->nfa before)
                     (regex->nfa after))]
         [else
          (let ([before-nfa (make-plain-nfa before)]
                [after-nfa (regex->nfa after)])
            (nfa-concate before-nfa
                         (match type
                           ['set (make-wildcard-nfa (string->list content))]
                           ['neg-set (make-negchar-nfa (string->list content))]
                           [else
                            (let ([content-nfa (regex->nfa content)])
                              (match type
                                ['regexp content-nfa]
                                ['zero-or-more (nfa-star-closure content-nfa)]
                                ['one-or-more (nfa-positive-closure content-nfa)]
                                ['zero-or-one (nfa-union content-nfa (make-ε-nfa))]
                                ['char (make-char-nfa)]
                                ['digit (make-digit-nfa)]))])
                         after-nfa))])]))

(define (regex-matcher arg)
  (let* ([dfa (if (string? arg)
                  (nfa->dfa (regex->nfa arg))
                  (nfa->dfa arg))]
         [T (dfa 'T)]
         [init (dfa 'init)])
    (define (accept? s)
      (if (member s (dfa 'F)) #t #f))
    (define (match-str str)
      (let ([result #f])
        (define (reset-result-and-iter start)
          (let ([mat result])
            (begin (set! result #f)
                   (cons (cons mat start)
                         (match-iter init
                                     (+ start (string-length mat))
                                     (+ start (string-length mat)))))))
        (define (match-iter stat index start)
          (if (= index (string-length str))
              (if (accept? stat) (list (cons (substring str start index) start)) '())
              (let ([next ((T 'lookup) stat (string-ref str index))])
                (begin
                  (if (accept? stat) (set! result (substring str start index)) '())
                  (if (not next)
                      (cond [result (reset-result-and-iter start)]
                            [(eq? init index) (match-iter init (+ index 1) start)]
                            [else (match-iter init (+ start 1) (+ start 1))])
                      (if (eq? init stat)
                          (match-iter next (+ index 1) index)
                          (match-iter next (+ index 1) start)))))))
        (match-iter init 0 0)))
    match-str))
