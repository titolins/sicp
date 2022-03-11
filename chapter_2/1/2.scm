; 2.2
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (avg-point p-fn p1 p2)
  (/ (+ (p-fn p1) (p-fn p2)) 2))

(define (avg-x-point p1 p2)
  (avg-point x-point p1 p2))

(define (avg-y-point p1 p2)
  (avg-point y-point p1 p2))

(define (midpoint-segment s)
  (let ((p1 (start-segment s)) (p2 (end-segment s)))
    (make-point (avg-x-point p1 p2) (avg-y-point p1 p2))))
