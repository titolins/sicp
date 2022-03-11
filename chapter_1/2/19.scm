;; NOT DONE

;;; observation

;; Tpq of (a,b)
;;; a <- bq + aq + ap
;;; b <- bp + aq

;;; with:
;;; p = 0
;;; q = 1
;;; Tpq(0, 1) n = 0
;;; =========
;;; a <- 1*1 + 0*1 + 0*0
;;; a = 1
;;; b <- 1*0 + 0*1
;;; b = 0 ?

;;; Tpq(1, 0) n = 1
;;; =========
;;; a <- 0*1 + 1*1 + 1*0
;;; a = 1
;;; b <- 0*0 + 1*1
;;; b = 1

;;; Tpq(1, 1) n = 2
;;; a <- 1*1 + 1*1 + 1*0
;;; a = 2
;;; b <- 1*0 + 1*1
;;; b = 1

;;; Tpq(2, 1) n = 3
;;; a <- 1*1 + 2*1 + 2*0
;;; a = 3
;;; b <- 1*0 + 2*1
;;; b = 2

;;; Tpq(3, 2) n = 4
;;; a <- 2*1 + 3*1 + 3*0
;;; a = 5
;;; b <- 2*0 + 3*1
;;; b = 3

;;; Tpq(5, 3) n = 5
;;; a <- 3*1 + 5*1 + 5*0
;;; a = 8
;;; b <- 3*0 + 5*1
;;; b = 5

;;; Tpq(8, 5) n = 6
;;; a <- 5 + 8 + 8*0
;;; a = 13
;;; b <- 5*0 + 8
;;; b = 8

;;; Tpq(13, 8) n = 7
;;; a = 21
;;; b = 13


;;; Tpq(21, 13) n = 8
;;; a = 55
;;; b = 34

;;; Tpq(55, 34) n = 9
;;; a = 89
;;; b = 55

;;; ==========================

;; n = 2
;;; with:
;;; p' = 1
;;; q' = 1
;;; Tp'q'(1, 1)
;;; a <- 1*1 + 1*1 + 1*1
;;; a = 3
;;; b <- 1*1 + 1*1
;;; b = 2

;; n = 4
;;; Tp'q'(3, 2)
;;; a <- 2 + 3 + 3
;;; a = 8
;;; b <- 2 + 3
;;; b = 5

;; I don't think (5, 3) is done since it's n = 3 (not even) - not sure though
;;; Tp'q'(5, 3)
;;; q' = 2
;;; p' = 1
;;; a <- 6 + 10 + 5
;;; a = 21
;;; b <- 3 + 10
;;; b = 13

;;; Tpq(8, 5)
;;; a <- 21
;;; b <- 13
;;; 21 <- 13q' + 8p'
;;; 13 <- 5p' + 8q'
;; q' = 1
;; p' = 1

;;; Tpq(13, 8)
;;; a <- 55
;;; b <- 34
;;; 55 <- 21q' + 13p'
;;; 34 <- 8p' + 13q'
;; q' = 2
;; p' = 1

;;; Tpq(21, 13)
;;; a <- 89
;;; b <- 55
;;; 89 <- 34q' + 21p'
;;; 55 <- 13p' + 21q'
;; q' = 2
;; p' = 1

;;; Tpq(55, 34)
;;; a <- 144
;;; b <- 89
;;; 144 <- 89q' + 55p'
;;; 89 <- 34p' + 55q'
;; q' = 1
;; p' = 1

;; so first, p' = q', then q' increases (but does it increment or doubles?)
;; ============

;; solution
(define (even? n)
  (= (remainder n 2) 0))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (inc x) (+ x 1))
(define (double x) (+ x x))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   q
                   (??) ;; compute p'
                   (??) ;; compute q'
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
