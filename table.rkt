#lang racket

(provide make-table table->list list->table)

(define (make-table . n)
  ; a table with arbitrary dimentions
  (let ((dim (if (null? n) 1 (car n)))
        (table (make-hash)))
    
    (define (check-params-and-do params only-keys? proc proc-name)
      ; check whether the number of params is right
      (let ((params-num (if only-keys? dim (+ dim 1)))
            (err-msg (if only-keys?
                         "illegal number of keys, given: ~a, excepted: ~a~%"
                         "illegal number of keys and value, given: ~a, excepted: ~a~%")))
        (if (not (= (length params) params-num))
            (error proc-name err-msg (length params) params-num)
            proc)))
    
    (define (lookup-iter key-list table)
      (let ((value (hash-ref table (car key-list) #f)))
        (cond ((= (length key-list) 1) value)
              (value (lookup-iter (cdr key-list) value))
              (else #f))))
    (define (lookup . keys)
      ((check-params-and-do keys #t lookup-iter 'lookup) keys table))
    
    (define (insert-iter! key-list value table)
      (if (= (length key-list) 1)
          (begin (hash-ref! table (car key-list) value)
                 'ok)
          (let ((key (car key-list)))
            (if (hash-has-key? table key)
                (insert-iter! (cdr key-list) value (hash-ref table key))
                (begin
                  (hash-set! table key (make-hash))
                  (insert-iter! (cdr key-list) value (hash-ref table key)))))))
    (define (insert! . keys-and-value)
      ((check-params-and-do keys-and-value #f insert-iter! 'insert!)
       (take keys-and-value dim) ; key-list
       (last keys-and-value) ; value
       table))
    (define (insert-list! keys-and-value)
      (apply insert! keys-and-value))
    
    (define (dispatch m)
      (case m
        ['lookup lookup]
        ['insert! insert!]
        ['insert-list! insert-list!]
        ['table table]
        ['dim dim]
        [else (error 'dispatch "Unknown message ~a~%" m)]))
    dispatch))

(define (table->list t)
  ; convert a n-dim table to a key-value list
  ; each key-value pair is presented by ((k1 k2 ...) v)
  ; so list looks like (((k1 k2 ...) v) ((k1 k2 ...) v) ((k1 k2 ...) v))
  (let ((l '()) (d (t 'dim)))
    (define (convert-iter key other-keys dim table)
      (if (= dim 1)
          (set! l (cons (cons (reverse (cons key other-keys))
                              table)
                        l))
          (hash-map table
                    (lambda (k v)
                      (convert-iter k (cons key other-keys) (- dim 1) v)))))
    (begin (hash-map (t 'table)
                     (lambda (k v)
                       (convert-iter k '() d v)))
           l)))

(define (list->table l . dim)
  (let* ((d (if (null? dim) 1 (car dim)))
         (t (make-table d)))
    (begin
      (for-each (lambda (kv-list) ((t 'insert-list!) kv-list)) l)
      t)))
