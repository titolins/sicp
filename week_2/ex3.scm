(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  (+ x 1))


; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; 1.43
(define (repeated f n)
  (define (repeat n res)
    (if (= n 0) res
        (repeat (- n 1) (f res))))
  (lambda (x) (repeat n x)))

; 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess (improve guess))
        guess
        (iter (improve guess))))
  (lambda (x) (iter x)))

(define tolerance 0.00001)
(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (sqrt x)
  ((iterative-improve close-enough? (lambda (y) (average y (/ x y))))) x)
