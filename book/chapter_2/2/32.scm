; (define (subsets s)
;   (if (null? s)
;       (list '())
;       (let ((rest (subsets (cdr s))))
;         (append rest (map (??) rest))))) <-- complete

;; using append list
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (i) (append (list (car s)) i)) rest)))))

;; using cons
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (i) (cons (car s) i)) rest)))))

(subsets (list 1 2 3))
; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; the base case is an empty list ('())
;; rest will be equal to the recursive call
;; append is used -> so the last call will be the first element

;; when we reach the last element of the list (number 3), the following will happen
;; rest = (())
;; (car s) = 3
;; -> (append (()) (map (lambda (i) (cons 3 i)) (())))
;; ---> (map (lambda (i) (cons 3 i)) (()))
;; ---> ((3))
;; -> (append (()) ((3)))
;; -> (() (3))
;; so we have a list of lists which sole element is an empty list (in the last recursive call)
;; and we will call map on this list and append the first element of s (which at this point is just the last element of the set) to all elements in there
;; then we will append this to the recursive call result.
;; so in the previous recursive call (once s = (3) returns), we will have:
;; rest = (() (3))
;; (car s) = 2
;; -> (append (() (3)) (map (lambda (i) (cons 2 i)) (() (3))))
;; ---> (map (lambda (i) (cons 2 i)) (() (3)))
;; ---> ((2) (2 3))
;; -> (append (() (3)) ((2) (2 3)))
;; -> (() (3) (2) (2 3))
;; and lastly:
;; rest = (() (3) (2) (2 3))
;; (car s) = 1
;; -> (append (() (3) (2) (2 3)) (map (lambda (i) (cons 1 i)) (() (3) (2) (2 3))))
;; ---> (map (lambda (i) (cons 1 i)) (() (3) (2) (2 3)))
;; ---> ((1) (1 3) (1 2) (1 2 3))
;; -> (append (() (3) (2) (2 3)) ((1) (1 3) (1 2) (1 2 3)))
;; -> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; so we start from a list with an empty list
;; we grab the last element of the set and we map/append it to each element of the list and join both the list and the map result
;; do this recursively, until we reach the first element of the list and we will have all possible sub-sets

