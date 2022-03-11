#lang racket

;; 1.
(define (substitute items old-word new-word)
  (if (null? items)
      '()
      (let ((item (car items)))
        (cons
         (cond ((list? item) (substitute item old-word new-word))
               ((eq? item old-word) new-word)
               (else item))
         (substitute (cdr items) old-word new-word)))))

;;;; results
;;> (substitute '((lead guitar) (bass guitar) (rhythm guitar) drums) 'guitar 'axe)
;;'((lead axe) (bass axe) (rhythm axe) drums)

;; 2.
(define (contains? items item)
  (define (iter items i)
    (cond ((null? items) -1)
          ((eq? (car items) item) i)
          (else (iter (cdr items) (+ i 1)))))
  (iter items 0))

(define (nth-item items n)
  (define (iter items i)
    (cond ((null? items) '())
          ((= n i) (car items))
          (else (iter (cdr items) (+ i 1)))))
  (iter items 0))

(define (substitute2 items old-words new-words)
  (if (null? items)
      '()
      (let ((item (car items)))
        (cons
         (cond ((list? item) (substitute2 item old-words new-words))
               ((>= (contains? old-words item) 0) (nth-item new-words (contains? old-words item)))
               (else item))
         (substitute2 (cdr items) old-words new-words)))))

;;;; results
;; > (substitute2 '((4 calling birds) (3 french hens) (2 turtle doves)) '(1 2 3 4) '(one two three four))
;; '((four calling birds) (three french hens) (two turtle doves))


;;; extra
;; two ways of doing it
;;;; 1. kind of cheat but it works -> using eval will return the procedure right away
;;;; (we should validate the input ofc)
;;;; 2. also, validate the input and iterate letter by letter
;;;; if 'a, we should car, if 'd we should cdr
(require berkeley)

(define (cxr-function input)
  ;; only skips c's -> should validate properly by making sure it's only the starting char
  ;; otherwise should fail
  (cond
    ((eq? (first input) 'c) (cxr-function (bf input)))
    ((eq? (first input) 'r) (lambda (x) x))
    ((eq? (first input) 'a) (lambda (x) ((cxr-function (bf input)) (car x))))
    ((eq? (first input) 'd) (lambda (x) ((cxr-function (bf input)) (cdr x))))
    (else (error "invalid input" (first input)))))

;;;; results
;; -> ((cxr-function 'cadar) '((4 5) 1 2 3))
;; 5
;; > ((eval 'cadar) '((4 5) 1 2 3))
;; 5
;; -> (cadar '((4 5) 1 2 3))
;; 5
