;; 1.13
(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

;;(define o (/ (+ 1 (sqrt 5)) 2))
;;(define t (/ (- 1 (sqrt 5)) 2))

;;> (fib 0)
;; 0
;;> (/ (- (expt o 0) (expt t 0)) (sqrt 5))
;;0
;; =======

;;> (fib 1)
;; 1
;;> (/ (- (expt o 1) (expt t 1)) (sqrt 5))
;;1.
;; =======

;;> (fib 7)
;;13
;;> (/ (- (expt o 7) (expt t 7)) (sqrt 5))
;;13.000000000000002
;; =======

;;> (fib 8)
;;21
;;> (/ (- (expt o 8) (expt t 8)) (sqrt 5))
;;21.000000000000004
;; =======

;;(fib 9)
;;34
;;> (/ (- (expt o 9) (expt t 9)) (sqrt 5))
;;34.00000000000001
;; =======

;;> (fib 10)
;;55
;;> (/ (- (expt o 10) (expt t 10)) (sqrt 5))
;;55.000000000000014
;; =======
