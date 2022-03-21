;; using nested maps with accumulate

;; required
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (inc x) (+ x 1))

(define (prime? x)
  (define (iter a)
    (cond ((= a x) #t)
          ((= 0 (remainder x a)) #f)
          (else (iter (inc a)))))
  (if (< x 2) #f (iter 2)))

;; find all ordered pairs of distinctive positive integers i and j
;; such that i+j is prime

;; generating the sequence can be done with previously defined
;; enumerate-interval

;; (accumulate append
;;             '()
;;             (map (lambda (i)
;;                    (map (lambda (j) (list i j))
;;                         (enumerate-interval 1 (- i j))))
;;                  (enumerate-interval 1 n)))

;; flatmap to simplify this
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; filtering
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; generating triple from pair
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; combines into
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
