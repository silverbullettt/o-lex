#lang racket

(require "regex-parser.rkt"
         "nfa.rkt" "nfa-to-dfa.rkt"
         "table.rkt" "utility.rkt")
(provide make-lex-parser)

(define (nfa-union! N1 N2)
  ; union N2 -> N1, this function will modify N1
  (let* ([new-init (+ (max (apply max (N1 'S))
                           (apply max (N2 'S)))
                      1)]
         [new-S (cons new-init (append (N1 'S) (N2 'S)))]
         [new-alphabet (union-append (N1 'alphabet) (N2 'alphabet))]
         [t (N1 'T)]
         [new-F (append (N1 'F) (N2 'F))])
    (begin
      (table-union! t (N2 'T))
      ; add ε-moves
      ((t 'insert!) new-init *ε* (list (N1 'init) (N2 'init)))
      (make-nfa new-S new-alphabet t new-init new-F))))

(define (make-lex-parser regex-list)
  (define (iter-union regex-list result token-map)
    ; regex-list -> list of regex and token type pair
    ; result -> result nfa
    ; stat-map -> map from accept state to token type
    (if (null? regex-list)
        (list result token-map)
        (let ([nfa (regex->nfa (caar regex-list))]
              [type (cdar regex-list)])
          (if result
              (let ([new-nfa (solve-state-collide result nfa)])
                (iter-union (cdr regex-list)
                            (nfa-union! result new-nfa)
                            (cons (cons (new-nfa 'F) type) token-map)))
              (iter-union (cdr regex-list)
                          nfa
                          (cons (cons (nfa 'F) type) token-map))))))
  (define (convert->dfa nfa token-map)
    (match (nfa->dfa nfa 'get-state-map)
      [(list dfa state-map)
       (let ([dfa-token-map (make-hash)])
         (begin
           (for-each
            (lambda (kv)
              (hash-set! dfa-token-map (car kv)
                         (second (assf (lambda (s) (list-intersect? (cdr kv) s)) token-map))))
            (map reverse-pair
                 (filter (lambda (kv) (member (cdr kv) (dfa 'F))) (hash->list state-map))))
           (list dfa dfa-token-map)))]))
  
  (match (apply convert->dfa (iter-union regex-list #f '()))
    [(list dfa token-map)
     (define (parse str)
       (define init (dfa 'init))
       (define (next-state s c) (((dfa 'T) 'lookup) s c))
       (define (accept? s) (if (member s (dfa 'F)) #t #f))
       (define (token-type s) (hash-ref token-map s))
       (define (parse-iter stat index tok)
         (if (= index (string-length str))
             (list (cons (token-type stat) tok))
             (let* ([c (string-ref str index)]
                    [next (next-state stat c)])
               (if (not next)
                   (cond [(accept? stat)
                          (cons (cons (token-type stat) tok)
                                (parse-iter init index ""))]
                         [(and (eq? stat init) (char-whitespace? c))
                          (parse-iter init (+ index 1) "")]
                         [else
                          (error 'lex-parser "Unknown token: \"~a\" on '~a'~%" tok c)])
                   (parse-iter next
                               (+ index 1)
                               (string-append tok (string c)))))))
       (parse-iter init 0 ""))
     parse]))