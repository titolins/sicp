#lang racket

(require berkeley)

;; 1. What will scheme print

(every - (keep number? '(the 1 after 909)))

;; (keep number? '(the 1 after 909))
;; -> '(1 909)
;; (every - '(1 909))
;; -> '(-1 -909)

((lambda (a b) ((if (< b a) + *) b a)) 4 6)

;; (lambda (a b) ((if (< b a) + *) b a))
;; -> (lambda (4 6) ((if (< 6 4) + *) 6 4))
;; -> (lambda (4 6) (* 6 4))
;; -> 24

(word (first '(cat)) (butlast 'dog))

;; (word (first '(cat)) (butlast 'dog))
;; -> (first '(cat))
;; ==> '(cat)
;; ==> (cons 'cat '())
;; -> 'cat
;; (butlast 'dog)
;; -> 'do
;; (word 'cat 'do)
;; 'catdo
