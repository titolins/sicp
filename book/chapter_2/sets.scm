;; helpers
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))

;; sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;; ordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

;; on average, we can say we need n/2 steps to check for membership with this.
;; That's still O(n) growth, but an improvement nonetheless

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; For intersection though, the speedup is a lot more considerable. The idea is that,
;; since both sets are ordered we can (considering x1 and x2 as the first elements of each set):
;; - if x1 = x2
;;   - add it to the result and we can calculate the set for the cdr of both set1 and set2
;; - if x1 < x2
;;   - we know x1 is not in s2 (since s2 is also ordered), so we can just skip that
;; - if x2 < x1
;;   - we know x2 is not in x1 (since s1 is also ordered), so we can just skip that
;; By doing this, we're effectively reducing the problem to calculating the intersection of smaller sets.
;; So, the number of steps required becomes the sum of the sizes of the sets (instead of the product),
;; making it O(n) (linear) growth, instead of quadratic.
