(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

;; this will reverse because it will start from the last element of the list (nil in the book)
;; and do (cons (car things) answer) for each call, while removing the first element
;; to exemplify that:
;;       things   |  answer
;; 1.  (1 2 3 4)  | ()
;; 2.  (2 3 4)    | (1)
;; 3.  (3 4)      | (4 1)
;; 4.  (4)        | (9 4 1)
;; 5.  ()         | (16 9 4 1)
;; so, it always gets the first value of things and appends to the front of answer (cons)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; this won't work because of 2 main (related) issues:
;; i - this will append nil to the beginning of the list and won't terminate the list properly with nil
;; ii - cons expects the second argument to be a list value, otherwise it will
;;      form a pair but not a list

;; an example working solution
;; taken from http://community.schemewiki.org/?sicp-ex-2.22
(define (square-list items)
  (define (iter l pick)
    (define r (square (car l)))
    (if (null? (cdr l))
        (pick (list r))
        (iter (cdr l) (lambda (x) (pick (cons r x))))))
  (iter items (lambda (x) x)))
