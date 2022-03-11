(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l))
              (list (car l)))))

(define (reverse l)
  (define (iter l r)
    (if (null? l)
        r
        (iter (cdr l) (cons (car l) r))))
  (iter l '()))
