;; 1.9
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))

; (+ 3 4)
; (inc (+ 2 4))
; (inc (inc (+ 1 4)))
; (inc (inc (inc 4)))
; (inc (inc 5))
; (inc 6)
; 7
; linear recursive since the inc call depends on the recursive call
;;================================================================

;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))
; (+ 3 4)
; (+ 2 5)
; (+ 1 6)
; (+ 0 7)
; 7
; iterative since we're keeping the state of each step updated
;; ================================================================

;; ackermann

;(define (A x y)
;  (cond ((= y 0) 0)
;        ((= x 0) (* 2 y))
;        ((= y 1) 2)
;        (else (A (- x 1)
;                 (A x (- y 1))))))

;;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

;;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 0 (A 1 (A 1 1)))))
;; (A 1 (A 1 (A 0 (A 1 2))))
;; (A 1 (A 1 (A 0 (A 0 (A 1 (A 1 1))))))
;; (A 1 (A 1 (A 0 (A 0 (A 1 2)))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 1 (A 1 1)))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 1 2))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 1))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 1)))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 1))))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 1)))))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 1))))))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 (A 1 1)))))))))))))
;; how does it end?
;; it actually does end, not sure what's wrong there. skipping it since it's not really
;; in the course exercises and I feel that I've practiced substitution enough for now

;;; (A 1 2)
;; (A 0 (A 1 (A 1 1)))
;; ================================================================
;; code above was commented since it was breaking the REPL for some reason..

;; fib
;;; tree recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;;; iterative
(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))


;; counting change
;;; iter

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (cc amount 5))

;; exponentiation
;; linear recursive
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; linear iteration
(define (expt b n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1)
              (* b product))))
  (iter n 1))

;; log steps
(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt b n)
  (d n b)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
