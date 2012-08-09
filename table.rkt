#lang racket

(provide make-table table->list)

(define (make-table . n)
  ; a table with arbitrary dimentions
  (let ((dim (if (null? n) 1 (car n)))
        (table (make-hash)))
    
    (define (lookup-iter key-list table)
      (let ((value (hash-ref table (car key-list) #f)))
        (cond ((= (length key-list) 1) value)
              (value (lookup-iter (cdr key-list) value))
              (else #f))))
    (define (lookup . keys)
      (if (not (= (length keys) dim))
          (error 'lookup 
                 "illegal number of keys, given: ~a, excepted: ~a~%"
                 (length keys)
                 dim)
          (lookup-iter keys table)))
    
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
      (if (not (= (length keys-and-value) (+ dim 1)))
          (error 'insert! 
                 "illegal number of keys and value, given: ~a, excepted: ~a~%"
                 (length keys-and-value)
                 (+ dim 1))
          (insert-iter! (take keys-and-value dim) ; key-list
                        (last keys-and-value) ; value
                        table)))
    
    (define (list-all) #f)
      ; return a list contains all members of table
      ; each key-value pair is presented by (k1 k2 ... kn v)
      
      
    (define (dispatch m)
      (case m
        ['lookup lookup]
        ['insert! insert!]
        ['table table]
        ['dim dim]
        ['list (list-all)]
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
