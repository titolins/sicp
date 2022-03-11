;; 1.38
;; main part taken from http://community.schemewiki.org/?sicp-ex-1.38
;; euler-d is clear (succession of three digits where first and last are always one)
;; but why +2? ahhh, read the book:
;; "... which included a continued fraction expansion for e - 2..."
;; since the expansion will calculate e - 2, we need to sum 2 to get the approximation of e

(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 1)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

(define (euler-d i)
  (if (= (remainder i 3) 2)
      (/ (+ i 1) 1.5)
      1))

(define (e-euler k)
  (+ 2.0 (cont-frac (lambda (i) 1)
                    euler-d
                    k)))
