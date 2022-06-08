;; helpers
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))

;;;; solution
;; checking for an element in a set is the same - at the first possible match, we simply return #t.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; adding elements to sets changes, we can just append it to the list (regardless of the element being present or not)
(define (adjoin-set x set)
  (cons x set))

;; intersection can also stay the same, it will just append elements that exist in both sets to a same set
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; union could also stay the same - since we're using adjoin-set (which is now constant time), it also improves efficiency
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

;; we could also just use append
(define (union-set set1 set2)
  (append set1 set2))

;; The efficiency in terms of input size will change for adjoin-set, which will become constant time - O(1),
;; and union, which becomes linear - O(n). Other than that, it stays the same:
;; O(n^2) - for intersection
;; O(n) - for element

;; what will change is that the input size will grow larger, since we're now keeping duplicates. So, in general,
;; operations that add elements to the set will see an improvement (since we are not checking for duplicates),
;; but for others (e.g. membership check and intersection) we should expect worse efficiency (since the set
;; sizes will grow larger).
;; This representation will perform better if sets are to be constructed from known non-duplicate elements.
;; It can also be useful if we need to count occurrences of different elements (although there are probably better ways to handle that)
