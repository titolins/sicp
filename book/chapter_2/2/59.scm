;;;; helpers
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;;;; solution
;; avoiding the worst-case scenario if set2 is nil
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

;; not avoiding the worst-case if set2 is nil
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))
