; 2.3

;; helpers
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (axis-length point s)
  (- (point (end-segment s)) (point (start-segment s))))
(define (x-length s)
  (axis-length x-point s))
(define (y-length s)
  (axis-length y-point s))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;; ex
;; there are some ways of defining a rectangle
;; first, we will use an approach using 2 segments
(define make-rect cons)
(define rect-b car)
(define rect-h cdr)

(define (rect-length r)
  (x-length (rect-b r)))

(define (rect-height r)
  (y-length (rect-h r)))

(define (perimeter r)
  (+ (* 2 (rect-length r)) (* 2 (rect-height r))))

(define (area r)
  (* (rect-length r) (rect-height r)))

;; the second approach involves having a single segment
;; representing one of the diagonals of the rectangle
(define make-rect make-segment)

(define (rect-length r)
  (x-length r))

(define (rect-height r)
  (y-length r))
