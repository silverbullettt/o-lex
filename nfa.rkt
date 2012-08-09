#lang racket

(require "table.rkt")
(require "utility.rkt")
(provide make-trans make-nfa make-empty-nfa make-plain-nfa
         star-closure)

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
      ['s Q]
      ['alphabet Σ]
      ['T Δ]
      ['init q0]
      ['F F]))
  dispatch)

(define (make-empty-nfa . q0)
  ; make a NFA that can recognize empty string
  (let ((q (if (null? q0) 0 (car q0))))
    (make-nfa q '() (make-trans) q (list q))))

(define (make-plain-nfa word)
  ; make a NFA which can recognize the word
  ; the states of result are presented by 0~length
  (if (= (string-length word) 0)
      (make-empty-nfa)
      (let ((Q (range (string-length word)))
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
