(load "intervals.scm")

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

;; or, using make-center-width
(define (make-center-percent c p)
  (make-center-width c (* c p)))

;; without width
(define (percent i)
  (/ (- (upper-bound i) (center i)) (center i)))

;; with width
(define (percent i)
  (/ (width i) (center i)))

;; the above has 2 possible improvements

;; 1. percent is considered as a decimal
;; if, instead, we'd like to receive a
;; percentage in from 0.0 to 100.0

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

;; this shouldn't affect our solution (since the constructor will do min max on the parameters)
;; but making sure we have an abs width value might be worth it
(define (make-center-percent c p)
  (make-center-width c (abs (* c (/ p 100)))))

;; this will return exact numbers if we pass ps' as integers (without decimals)
;; exact->inexact can be used to solve that. We won't do this here though


;; 2. if center is 0, the percent solution above will try a division by 0, which will fail
;; we can just define that percentage for intervals with center 0 = 0
(define (percent i)
  (if (= (center i) 0)
      0
      (* (/ (- (upper-bound i) (center i)) (center i)) 100)))

;; curiosity:
;; defining percentage without center
;; (/ (- (upper-bound i) (lower-bound i)) (+ (upper-bound i) (lower-bound i)))
