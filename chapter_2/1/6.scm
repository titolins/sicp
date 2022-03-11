(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((n f) x))))    ; add-1 body
;; (lambda (f) (lambda (x) (f ((zero f) x)))) ; replace n with zero
;; (lambda (f) (lambda (x) (f x)))            ; ((zero f) x) is just x

;; church numerals can be represented by successive applications of the
;; successor function to the 0 value
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
