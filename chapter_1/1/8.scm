;; 1.8
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x) 
  (= (improve guess x) guess))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve-cube guess x)
                 x)))

(define (cbrt x)
  (cbrt-iter 1.1 x))
