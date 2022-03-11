#lang racket
;; 1.
;;; book exercises

;;; 2.
(define (inc i) (+ i 1))
(define (sum-of-factors n)
  (define (factor? i)
    (= (remainder n i) 0))
  (define (iter i a)
    (cond ((= i n) a)
          ((factor? i) (iter (inc i) (+ a i)))
          (else (iter (inc i) a))))
  (iter 1 0))

(define (next-perf n)
  (if (= (sum-of-factors n) n)
      n
      (next-perf (+ n 1))))


;; while this solution is correct, it does it's job in O(n)
;; we don't need to go until n to figure out the sum of it's factors
;; e.g., one solution would be going until n/2
;; class solution is even better though

;;; 3
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
(define (bad-cc amount kinds-of-coins)
  (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
        ((= amount 0) 1)
        (else (+ (bad-cc amount
                         (- kinds-of-coins 1))
                 (bad-cc (- amount
                            (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
;; not sure exactly
;; at first it seems obvious, but trying to reproduce it doesn't work
;; if at some point both kinds-of-coins and amount are 0 then the result would get
;; sketched. but we only decrease kinds-of-coins when amount is kept the same (and vice-versa)
;; so both values won't ever reach 0 at the same time
;; you can force this by calling (cc 0 0) and (bad-cc 0 0). we can see that the former
;; returns 1 for this case, while the latter returns 0

;;; 4
;; 37 = (+ b n (* counter product))
;; b and n don't change
;; (expt 3 3)
;; c_x1 = 3
;; p_x1 = 1
;; x1 = 3 + 3 + 3*1
;; c_x2 = 2
;; p_x2 = 3
;; x2 = 3 + 3 + 2*3
;;; from https://github.com/zackads/sicp/blob/main/week_3/homework.md
;;; (not sure where he got the answer from)
;;; The product of PRODUCT times [B to the COUNTER power] is always equal to [B to the N power].
;;; found the answers:
;;; https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week3

;;; skipping experts
