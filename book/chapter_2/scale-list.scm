;; without map

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;; using map

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
