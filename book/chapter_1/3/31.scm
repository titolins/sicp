;; 1.31
;; a
(define (inc x) (+ x 1))
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fact x)
  (product identity 1 inc x))

;; b
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (fact x)
  (product-iter identity 1 inc x))
