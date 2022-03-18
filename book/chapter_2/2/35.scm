;; (define (count-leaves t)
;;   (accumulate (??) (??) (map (??) (??))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;; enumerate and count
(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y)) 0 (enumerate-tree t)))

(define (count-leaves t)
   (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;; probably what they expect:
;; we can just return ones and do sum
(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub-tree)
                         (cond ((null? sub-tree) 0)
                               ((not (pair? sub-tree)) 1)
                               (else (count-leaves sub-tree))))
                       t)))

