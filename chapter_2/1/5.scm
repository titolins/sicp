(define (count-0-remainder-divisions n d)
  (define (iter counter n)
    (if (= (remainder n d) 0)
        (iter (+ counter 1) (/ n d))
        counter))
  (iter 0 n))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (count-0-remainder-divisions z 2))

(define (cdr z)
  (count-0-remainder-divisions z 3))
