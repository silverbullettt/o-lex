#lang racket

(require "table.rkt" "utility.rkt")
(provide make-trans make-nfa make-empty-nfa make-plain-nfa
         star-closure nfa-union nfa-concate)

(define (make-trans . tuples)
  (let ((table (make-table 2)))
    (begin
      (map (lambda (tuple)
             ((table 'insert!)
              (first tuple) (second tuple) (third tuple)))
           tuples)
      table)))

(define (make-nfa Q Σ Δ q0 F)
  ; Q -> set of states
  ; Σ -> set of input symbols
  ; Δ -> a translation relation Q × Σ → P(Q)
  ; q0 -> an initial state
  ; F -> set of acceptable states
  ; This NFA will work well when input symbols without 'ε'
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
    ; (printf "   span states: ~a~%" states)
    (remove-duplicates
     (flatten
      (map (lambda (s) (cons s (next-states s #\ε))) states))))
  (define (recognize-iter curr-states str)
    ; (printf "states: ~a, str: ~a~%" curr-states str)
    (cond ((null? curr-states) #f)
          ((= (string-length str) 0) (accept? curr-states))
          (else
           (let ((first-sym (string-ref str 0))
                 (rest-sym (substring str 1)))
             (for/or ([s curr-states])
               ; (printf " for/or s: ~a, curr-states: ~a~%" s curr-states)
               (recognize-iter (ε-span (next-states s first-sym)) rest-sym))))))
  (define (recognize str)
    (if (recognize-iter (ε-span (list q0)) str)
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
      [else (error 'NFA-dispatch "Unknown message ~a~%" m)]))
  dispatch)

(define (make-empty-nfa)
  ; make a NFA that can recognize empty string
  (make-nfa 0 '() (make-trans) 0 (list )))

(define (make-plain-nfa word)
  ; make a NFA which can recognize the word
  ; the states of result are presented by 0~length
  (if (= (string-length word) 0)
      (make-empty-nfa)
      (let ((Q (range (+ (string-length word) 1)))
            (alphabet (remove-duplicates (string->list word)))
            (t (make-table 2))
            (init 0)
            (F (list (string-length word))))
        (begin
          (for-each
           (lambda (s) ((t 'insert!) s (string-ref word s) (list (+ s 1))))
           (range (string-length word)))
          (make-nfa Q alphabet t init F)))))

(define (star-closure nfa)
  (let ((t (nfa 'T))
        (init (nfa 'init))
        (F (nfa 'F)))
    (begin
      ((t 'insert!) init #\ε (car F))
      (for-each (lambda (s) ; acceptable state
                  (let ((new-next (if ((t 'lookup) s #\ε)
                                      (cons init ((t 'lookup) s #\ε))
                                      (list init))))
                    ((t 'insert!) s #\ε new-next)))
                F)
      (make-nfa (nfa 'Q) (nfa 'alphabet) t init F))))

(define (solve-state-collide A B)
  ; compose a new state machine, which isomorphism with B
  ; but all states in new machine will not interact with A
  ; Note: all states in NFA are integers
  (let* ((new-Q (range (+ (apply max (A 'S)) 1)
                       (+ (+ (apply max (A 'S)) 1) (length (B 'S)))))
         (state-map (make-hash (map cons (B 'S) new-Q)))
         (t (make-trans)))
    (define (convert old-state)
      (hash-ref state-map old-state #f))
    
    (begin
      (for-each (lambda (kv)
                  (let ((old-curr (caar kv))
                        (old-next (cdr kv)) ; next is a set of states!
                        (sym (cadar kv)))
                    ((t 'insert!) (convert old-curr) sym (map convert old-next))))
                (table->list (B 'T)))
      (make-nfa new-Q
                (B 'alphabet)
                t
                (hash-ref state-map (B 'init))
                (map convert (B 'F))))))

(define (nfa-union N1 N2)
  ; union two NFAs
  ; add a new init state, and set ε-moves to the NFAs initial states
  (let* ((new-N2 (solve-state-collide N1 N2)) ; 
         (new-init (+ (max (apply max (N1 'S))
                           (apply max (new-N2 'S)))
                      1))
         (new-S (cons new-init (append (N1 'S) (new-N2 'S))))
         (new-alphabet (union-append (N1 'alphabet) (new-N2 'alphabet)))
         (new-t (new-N2 'T))
         (new-F (append (N1 'F) (new-N2 'F))))
    (begin
      (for-each (lambda (kv)
                  (let ((curr (caar kv))
                        (next (cdr kv)) ; next is a set of states!
                        (sym (cadar kv)))
                    ((new-t 'insert!) curr sym next)))
                (table->list (N1 'T)))
      ; add ε-moves
      ((new-t 'insert!) new-init #\ε (list (N1 'init) (new-N2 'init)))
      (make-nfa new-S new-alphabet new-t new-init new-F))))

(define (nfa-concate N1 N2)
  ; concate two NFAs
  (let* ((new-N2 (solve-state-collide N1 N2))
         (new-S (append (N1 'S) (new-N2 'S)))
         (new-t (new-N2 'T))
         (new-alphabet (union-append (N1 'alphabet) (new-N2 'alphabet)))
         (new-init (N1 'init))
         (new-F (new-N2 'F)))
    (begin
      (for-each (lambda (kv)
                  (let ((curr (caar kv))
                        (next (cdr kv)) ; next is a set of states!
                        (sym (cadar kv)))
                    ((new-t 'insert!) curr sym next)))
                (table->list (N1 'T)))
      ; add ε-moves from final states of N1 to initial state of N2
      (for-each (lambda (fs) ; final states of N1
                  ((new-t 'insert!) fs #\ε (cons (new-N2 'init)
                                                 (if (((N1 'T) 'lookup) fs #\ε)
                                                     (((N1 'T) 'lookup) fs #\ε)
                                                     '()))))
                (N1 'F))
      (make-nfa new-S new-alphabet new-t new-init new-F))))
