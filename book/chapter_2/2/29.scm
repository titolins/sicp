(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a. left-branch and right-branch, branch-length and branch-structure
(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

;; b. total-weight

(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else
         (+ (total-weight (branch-structure (left-branch m)))
            (total-weight (branch-structure (right-branch m)))))))

;; c. balanced?
(define (torque b)
  (* (branch-length b) (total-weight (branch-structure b))))

(define (balanced? m)
  (if (not (pair? m))
      #t
      (and (= (torque (left-branch m)) (torque (right-branch m)))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))))

;; tests
(define m1 (make-mobile
            (make-branch 14 (make-mobile (make-branch 4 3) (make-branch 4 3)))
            (make-branch 7 (make-mobile (make-branch 2 6) (make-branch 2 6)))))

(define m2 (make-mobile
            (make-branch 14 (make-mobile (make-branch 1 2) (make-branch 2 3)))
            (make-branch 7 (make-mobile (make-branch 3 2) (make-branch 4 5)))))

;; scheme@(guile-user) [16]> (total-weight m1)
;; $37 = 18

;; d. using cons instead of list
;; as long as we respect the data abstraction, we should require changes to the selectors only
;; in this case actually, only changes to the selectors that involve cdr are required (because of cons' nature)
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch m)
  (cdr m))

(define (branch-structure b)
  (cdr b))

