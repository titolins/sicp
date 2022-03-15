(define (old-mul x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (opposite-pair? a b)
  (if (positive? a)
      (negative? b)
      (positive? b)))

(define (positive-pair? a b)
  (if (opposite-pair? a b)
      #f
      (positive? a)))

(define (negative-pair? a b)
  (if (opposite-pair? a b)
      #f
      (negative? a)))

(define (new-mul x y)
  (let ((x0 (lower-bound x))
        (x1 (upper-bound x))
        (y0 (lower-bound y))
        (y1 (upper-bound y)))
    (cond
     ((negative-pair? x0 x1)
      (cond
       ((negative-pair? y0 y1) (make-interval (* x0 y0) (* x1 y1)))
       ((opposite-pair? y0 y1) (make-interval (* x0 y1) (* x0 y0)))
       (else (make-interval (* x0 y1) (* x1 y0)))))
     ((opposite-pair? x0 x1)
      (cond
       ((negative-pair? y0 y1) (make-interval (* x1 y0) (* x0 y0)))
       ((opposite-pair? y0 y1) (make-interval
                                ((lambda (a b) (if (< a b) a b)) (* x0 y1) (* x1 y0))
                                ((lambda (a b) (if (> a b) a b)) (* x0 y0) (* x1 y1))))
       (else (make-interval (* x0 y1) (* x1 y1)))))
     (else
      (cond
       ((negative-pair? y0 y1) (make-interval (* x1 y0) (* x0 y1)))
       ((opposite-pair? y0 y1) (make-interval (* x1 y1) (* x1 y0)))
       (else (make-interval (* x0 y0) (* x1 y1))))))))

;; testing values
(define i1 (make-interval -15 -5))
(define i2 (make-interval -5 5))
(define i3 (make-interval 5 15))
