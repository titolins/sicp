; helper lib
; allows handling symbols as strings/lists (using first/last)
; #\a -> a
;(define (char->symbol chr)
;  (string->symbol (list->string (list chr))))
;
;; abc -> (a b c)
;(define (symbol->list sym)
;  (map char->symbol 
;       (string->list (symbol->string sym))))


; 1 book (exercises.scm)
; 2
(define (squares ns)
  (define (square x)
    (* x x))
  (if (null? ns)
      '()
      (cons (square (first ns)) (squares (cdr ns)))))

; 3
(define (switch sentence)
  (define (switch-me s)
    (if (null? s)
        '()
        (cond ((or
                 (equal? (first s) 'i)
                 (equal? (first s) 'me))
               (cons 'you (switch-me (cdr s))))
              ((equal? (first s) 'you)
               (cons 'me (switch-me (cdr s))))
              (else (cons (first s) (switch-me (cdr s)))))))
  (if (equal? (first sentence) 'you)
      (cons 'i (switch-me (cdr sentence)))
      (switch-me sentence)))

; 4
(define (ordered? xs)
  (if (null? (cdr xs))
      #t
      (if (> (first xs) (first (cdr xs)))
          #f
          (ordered? (cdr xs)))))

; 5
; not having the original sdk is a bit annoying
; (makes us have to work around such things as symbols having to be treated as lists..)
(define (symbol->list-of-symbols sym)
  (map (lambda (s) (string->symbol (string s))) 
       (string->list (symbol->string sym))))
(define (ends-e sentence)
  (define (word-ends-e w)
    (if (equal? (last (symbol->list-of-symbols w)) 'e)
        #t
        #f))
  (if (null? sentence)
      '()
      (if (word-ends-e (first sentence))
          (cons (first sentence) (ends-e (cdr sentence)))
          (ends-e (cdr sentence)))))

; 6
; if you use
(and (= 1 1) (/ 2 0))
; you see you get a zero division error
; however, if you do the same changing the equality values in the first predicate
; (to something false). e.g.:
(and (= 1 0) (/ 2 0))
; you don't get and error
; which means and is a special form (since if the first predicate is false, it doesn't evaluate
; the rest)

; the same for or..
(or (= 1 0) (/ 2 0))
; doesn't raise any errors, whilst..
(or (= 1 1) (/ 2 0))
; does! that happens because the second predicate just gets evaluated if the first one is true
; like this, there are no unecessary computations
; however, if they were ordinary form it would probably help with bugs..
