(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;scheme@(guile-user) [27]> (cc 100 uk-coins)
;$191 = 104561
;scheme@(guile-user) [27]> (cc 100 us-coins)
;$192 = 292
;scheme@(guile-user) [27]> (cc 100 (reverse us-coins))
;$193 = 292
;scheme@(guile-user) [27]> (cc 100 (reverse uk-coins))
;$194 = 104561
;scheme@(guile-user) [27]> (cc 100 (list 25 50 10 1 5))
;$195 = 292

;; changing the coin-values order does not affect the result
;; this happens because cc will call itself recursively with both:
;; i - same amount and except first coin-values
;; ii - amount - first-coin-amount and same coin-values
;; so it will cover all possible combinations no matter the order
