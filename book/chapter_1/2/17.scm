;; 1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (even? n)
  (= (remainder n 2) 0))
(define (double a)
  (+ a a))
(define (halve a)
  (/ a 2))
(define (fast-* a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))
