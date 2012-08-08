#lang racket

(require "table.rkt")

(provide make-nfa)

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
    (if (null? (filter (λ (s) (member s F)) states))
        #f
        #t))
  (define (ε-span states) 
    ; deal with ε-transfer by span the set of states with which it can reach through 'ε'
    (remove-duplicates
     (flatten
      (map (λ (s) (cons s (next-states s #\ε))) states))))
  (define (recognize-iter curr-states str)
    ; (printf "states: ~a, str: ~a~%" curr-states str)
    (cond ((null? curr-states) #f)
          ((= (string-length str) 0) (accept? curr-states))
          (else
           (let ((first-sym (string-ref str 0))
                 (rest-sym (substring str 1)))
             (for/or ([s curr-states])
               (recognize-iter (ε-span (next-states s first-sym)) rest-sym))))))
  (define (recognize str)
    (if (recognize-iter (ε-span (list q0)) str)
        'accept
        'reject))
  recognize)
