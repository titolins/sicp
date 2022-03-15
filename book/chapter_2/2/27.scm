(define (deep-reverse l)
  (define (iter l r)
    (cond ((null? l) r)
          ((pair? (car l)) (iter (cdr l) (cons (iter (car l) '()) r)))
          (else (iter (cdr l) (cons (car l) r)))))
  (iter l '()))

;; we can isolate the cond to the 2nd iter parameter
(define (deep-reverse l)
  (define (iter l r)
    (if (null? l)
        r
        (iter (cdr l)
              (cons (if (pair? (car l))
                        (iter (car l) '())
                        (car l))
                    r))))
  (iter l '()))
