#lang racket

(provide range accumulate union-append)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

(define (range . params)
  (define (check-params start end step)
    (cond ((and (positive? step) (< end start))
           (error "range: step > 0 && start > end"))
          ((and (negative? step) (> end start))
           (error "range: step < 0 && start < end"))
          ((zero? step)
           (error "range: step cannot be zero!"))
          (else 'ok)))
  (define (range-iter start end step lst)
    (if (or (and (> step 0) (>= start end))
            (and (< step 0) (<= start end)))
        (reverse lst)
        (range-iter (+ start step) end step (cons start lst))))
  (define (check-and-iter start end step)
    (if (eq? (check-params start end step) 'ok)
        (range-iter start end step '())
        #f))
  
  (case (length params)
    ['1 (check-and-iter 0 (first params) 1)]
    ['2 (check-and-iter (first params) (second params) 1)]
    ['3 (check-and-iter (first params) (second params) (third params))]
    [else (error "range: params must be (range end) or (range start end [step])")]))

(define (union-append . lists)
  (remove-duplicates (accumulate append '() lists)))