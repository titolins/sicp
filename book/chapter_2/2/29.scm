
;; ============= using list ============= ;;

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; ============= using cons ============= ;;

(define (make-mobile left right)
  (cons left right)) ; <--

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile)) ; <--

(define (make-branch length structure)
  (cons length structure)) ; <--

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch)) ; <---

;; besides the constructors, only cdr selectors will change (since car works the same for both lists and pairs)

;; ============= total weight ============= ;;

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define test-total-weight (build-tester total-weight))
(define total-weight-cases (list (make-case m1 18) (make-case m2 12)))
(test-total-weight total-weight-cases)

;; ============= balanced? ============= ;;

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (if (not (pair? mobile))
      #t
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

(define test-balanced? (build-tester balanced?))
(define balanced?-cases (list (make-case m1 #t) (make-case m2 #f)))
(test-balanced? balanced?-cases)

;; ============= tests ============= ;;

;; values
; 1.
; weight 18
; balanced
(define m1 (make-mobile
            (make-branch 14 (make-mobile (make-branch 4 3) (make-branch 4 3)))
            (make-branch 7 (make-mobile (make-branch 2 6) (make-branch 2 6)))))

; 2.
; weight 12
; unbalanced
(define m2 (make-mobile
            (make-branch 14 (make-mobile (make-branch 1 2) (make-branch 2 3)))
            (make-branch 7 (make-mobile (make-branch 3 2) (make-branch 4 5)))))

;; helpers
(define (make-case mobile exp-res)
  (cons mobile exp-res))

(define (build-tester fn)
  (define (run-tests cases)
    (define (case-input c)
      (car c))
    (define (case-exp c)
      (cdr c))
    (define (display-line l)
      (newline)
      (display l)
      (newline))
    (define (display-case c i)
      (display-line "==============")
      (display-line "test case")
      (display "i = ")
      (display i)
      (newline)
      (display "mobile = ")
      (display (case-input c))
      (newline)
      (display "exp = ")
      (display (case-exp c))
      (newline))
    (define (success)
      (display-line "TEST PASSED")
      (display-line "=============="))
    (define (fail res)
      (display-line "TEST FAILED")
      (display "got = ")
      (display res)
      (newline)
      (display-line "=============="))
    (define (finished)
      (display-line "==============")
      (display-line "finished tests")
      (display-line "=============="))
    (define (run-case c i)
      (display-case c i)
      (let ((res (fn (case-input c))))
        (if (eq? res (case-exp c))
            (success)
            (fail res))))
    (define (iter cases i)
      (if (null? cases)
          #t
          (and (run-case (car cases) i)
              (iter (cdr cases) (+ i 1)))))
    (iter cases 0))
  run-tests)
