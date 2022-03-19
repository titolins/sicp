(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; (fold-right / 1 (list 1 2 3))
;; (/ 1 (/ 2 (/ 3 1)))
;; (/ 1 (/ 2 3))
;; (/ 3 2)
(fold-right / 1 (list 1 2 3))

;; (fold-left / 1 (list 1 2 3))
;; (/ (/ (/ 1 1) 2) 3)
;; (/ (/ 1 2) 3)
;; (/ 1 6)
(fold-left / 1 (list 1 2 3))

;; (fold-right list nil (list 1 2 3))
;; (list 1 (list 2 (list 3 nil)))
;; (list 1 (list 2 (3 '())))
;; (list 1 (2 (3 '())))
;; (1 (2 (3 ())))
(fold-right list '() (list 1 2 3))

;; (fold-left list nil (list 1 2 3))
;; (list (list (list '() 1) 2) 3)
;; (list (list ('() 1) 2) 3)
;; (list (('() 1) 2) 3)
;; (((() 1) 2) 3)
(fold-left list '() (list 1 2 3))

;; fold-left and fold-right will work the same if the order of the sequence items
;; doesn't affect the final result (which is to say the operation is commutative)
