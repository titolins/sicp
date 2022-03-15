;; 1.11

;; recursive
(define (f-recur n)
  (if (< n 3) n
      (+ (f-recur (- n 1))
         (f-recur (- n 2))
         (f-recur (- n 3)))))

;; iterative
(define (f-iter n)
  (define (iter a b c count)
    (if (= count n) c
        (iter (+ a b c) a b (+ count 1))))
  (iter 2 1 0 0))
