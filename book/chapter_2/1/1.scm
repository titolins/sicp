; 2.1
(define (pos n)
  (if (< n 0)
      (- n)
      n))

(define (invert-if p n)
  (if (p n)
      (- n)
      n))

(define (pos n)
  (invert-if (lambda (x) (< x 0)) n))

(define (neg n)
  (invert-if (lambda (x) (> x 0)) n))

(define (make-rat n d)
  (if (or (and (> n 0) (< d 0)) (and (< n 0) (> d 0)))
      (cons (neg n) (pos d))
      (cons (pos n) (pos d))))
