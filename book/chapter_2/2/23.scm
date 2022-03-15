(define (for-each proc l)
  (if (null? l)
      #t
      (and (proc (car l))
           (for-each proc (cdr l)))))

;; good point raised in schemewiki community is that, if proc returns false this will fail

(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
               (for-each proc (cdr items)))))

;; but since we can pass a bunch of instructions to else, that should work
