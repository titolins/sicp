;(load "berkeley.scm")

;; 2
;;(define (every f ps)
;;  (define (iter l res)
;;    (if (null? l)
;;      res
;;      (iter (bl l) (cons (f (last l)) res))))
;;  (iter ps '()))
;
;; testing
;;(every square '(1 2 3 4))
;;(keep (lambda (wd)(member? 'e wd)) '(purple syzygy))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(((lambda (x) (x x))
  (lambda (fact-gen)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((fact-gen fact-gen) (- n 1)))))))
 5)
