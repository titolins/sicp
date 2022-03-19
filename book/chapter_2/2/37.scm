(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;;(define (matrix-*-vector m v)
;;  (map (??) m))

;; for some insight on what exactly should be done for multiplying matrices by vectors:
;; https://mathinsight.org/matrix_vector_multiplication
;; basically, "One takes the dot product of x with each of the rows of A"
(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

;;(define (transpose mat)
;;  (accumulate-n (??) (??) mat))

(define (transpose mat)
  (accumulate-n cons '() mat))

;;(define (matrix-*-matrix m n)
;;  (let ((cols (transpose n)))
;;    (map (??) m)))

;; the thing to pay attention here is that you can only get the product of matrices with x rows with matrices with x columns
;; so if you try to multiple the matrice m below by itself it will error (you have to do m by (transpose m))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (map (lambda (c) (dot-product r c)) cols)) m)))

;; which translates to:
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
