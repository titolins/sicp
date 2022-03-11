;; 1.37
;;; recursive
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

;;; iterative
(define (cont-frac-iter n d k)
  (define (iter i res)
    (if (= i 1)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

;;(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)
;;Value: .6179775280898876
;;(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 12)
;;Value: .6180555555555556 <-- .6180 ; k = 12
