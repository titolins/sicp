#lang racket

(require berkeley)

;; adding this here since it's really interesting
;; it's basically the start of a scheme repl
;; it's a calculator, but accepts scheme-like expressions so it's implementation is as simple as it gets

;; calc is the repl
(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (newline) ;; had to add this for
  (calc))

;; calc-eval will turn the syntax into the semantics
;; -> it konws the form/syntax (the expression)
;; -> it returns the value/semantics
;; there's a mutual recursion going on here (calc-cal -> calc-apply) -> to be explained next week
;; that's a consequence of deep lists as data
(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
        (else (error "Calc: bad expression:" exp))))

;; apply just konw semantics (except for parenthesis and quotation marks)
;; it receives valid scheme data and performs calculations on it
(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
        ((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
                           ((= (length args) 1) (- (car args)))
                           (else (- (car args) (accumulate + 0 (cdr args)))))) ;; make clear what's the expected behaviour of the - operator
        ((eq? fn '*) (accumulate * 1 args))
        ((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
                           ((= (length args) 1) (/ (car args)))
                           (else (/ (car args) (accumulate * 1 (cdr args))))))
        (else (error "Calc: bad operator:" fn))))
