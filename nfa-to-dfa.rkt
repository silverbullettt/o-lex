#lang racket

(require "table.rkt" "nfa.rkt" "utility.rkt")
(require data/queue)
(provide make-dfa nfa->dfa)

(define (make-dfa Q Σ Δ q0 F)
  ; Q -> set of states
  ; Σ -> set of input symbols
  ; Δ -> a translation relation Q × Σ → Q
  ; q0 -> an initial state
  ; F -> set of acceptable states
  (define (next-state q sym)
      (if ((Δ 'lookup) q sym)
          ((Δ 'lookup) q sym)
          #f)) ; use #f to present unknown state
    (define (accept? s)
      (if (member s F) #t #f))
  
  (define (recognize str)
    (define (recognize-iter curr-state i)
      (cond [(not curr-state) #f]
            [(= (string-length str) i) (accept? curr-state)]
            [else (recognize-iter
                   (next-state curr-state
                               (string-ref str i))
                   (add1 i))]))
    (if (recognize-iter q0 0)
        'accept
        'reject))
  
  (define (dispatch m)
    (case m
      ['recog recognize]
      ['S Q]
      ['alphabet Σ]
      ['T Δ]
      ['init q0]
      ['F F]
      [else (error 'DFA-dispatch "Unknown message ~a~%" m)]))
  dispatch)

(define (nfa->dfa . args)
  (define (arrange state-set)
    (sort (remove-duplicates state-set) <))
  (define (arrange-table t)
    (begin
      (for-each (lambda (kv) ((t 'insert!) (first kv)
                                           (second kv)
                                           (arrange (third kv))))
                (table->list t))
      t))
  (let* ([nfa (car args)]
         [Q (nfa 'S)]
         [Σ (nfa 'alphabet)]
         [Δ (arrange-table (nfa 'T))]
         [q0 (nfa 'init)]
         [F (nfa 'F)]
         [dfa-init #f])
    
    (define (next-states q sym)
      (if ((Δ 'lookup) q sym)
          ((Δ 'lookup) q sym)
          '()))
    (define (accept? states)
      (if (null? (filter (lambda (s) (member s F)) states))
          #f
          #t))
    (define (ε-span states)
      ; deal with ε-moves by span the set of states with which it can reach through 'ε'
      (arrange (apply append
                      (map (lambda (s) (cons s (ε-span (next-states s *ε*)))) ; ε-span should be recursive
                               states))))
    (define (list-next-states states sym)
      (ε-span
       (apply append (map (lambda (q) (next-states q sym)) states))))
    
    (define (convert-table)
      (let ([q (make-queue)]
            [dfa-t (make-table 2)])
        (define (convert-iter)
          (if (queue-empty? q)
              dfa-t
              (let ([state-set (dequeue! q)])
                (begin
                  (for-each (lambda (sym)
                              (if ((dfa-t 'lookup) state-set sym)
                                  '()
                                  (let ([nexts (list-next-states state-set sym)])
                                    (if (null? nexts)
                                        '()
                                        (enqueue! q
                                                  ((dfa-t 'insert!) state-set
                                                                    sym
                                                                    nexts))))))
                            Σ)
                  (convert-iter)))))
        (begin (set! dfa-init (arrange (ε-span (list q0))))
               (enqueue! q dfa-init)
               (convert-iter))))
    
    (define (state-list->int dfa-t)
      (let ([new-t (make-table 2)]
            [state-map (make-hash)]
            [counter 0])
        (define (inc)
          (begin (set! counter (add1 counter)) counter))
        (define (convert set)
          (if (hash-has-key? state-map set)
              (hash-ref state-map set)
              (hash-ref! state-map set (inc))))
        (begin (for-each (lambda (kv)
                           ((new-t 'insert!) (convert (first kv))
                                             (second kv)
                                             (convert (third kv))))
                         (table->list dfa-t))
               (cons new-t state-map))))
    
    (let* ([dfa-t (car (state-list->int (convert-table)))]
           [state-map (cdr (state-list->int (convert-table)))]
           [dfa  (make-dfa (hash-values state-map)
                           Σ
                           dfa-t
                           (hash-ref state-map dfa-init)
                           (map (lambda (k) (hash-ref state-map k))
                                (filter accept? (hash-keys state-map))))])
      (if (= (length args) 1) dfa (list dfa state-map)))))
