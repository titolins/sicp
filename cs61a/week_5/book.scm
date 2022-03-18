;; regular definitions
(define (square x) (* x x))
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; sequence operations

;; map is map

;; guile also has filter, but it's nice to have an example implementation anyways
(define (even? x)
  (= (modulo x 2) 0))
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;; accumulate (useful for the calc implementation also - we're using racket + berkeley library there only for that)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; scheme@(guile-user) [2]> (accumulate + 0 (list 1 2 3 4 5))
;; $9 = 15
;; scheme@(guile-user) [2]> (accumulate * 1 (list 1 2 3 4 5))
;; $10 = 120
;; scheme@(guile-user) [2]> (accumulate cons '() (list 1 2 3 4 5))
;; $11 = (1 2 3 4 5)

;; enumerate interval
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;; scheme@(guile-user) [2]> (enumerate-interval 2 7)
;; $12 = (2 3 4 5 6 7)

;; enumerate tree
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;; signal-flow style
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
