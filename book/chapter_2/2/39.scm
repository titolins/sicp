(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; (define (reverse sequence)
;;   (fold-right (lambda (x y) (??)) '() sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

;; (define (reverse sequence)
;;   (fold-left (lambda (x y) (??)) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

;; or, in this case we can simply use cons
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


;; this is also a cool solution
;; taken from: http://community.schemewiki.org/?sicp-ex-2.39
(define (push value sequence)
  (fold-right cons (list value) sequence))

(define (reverse sequence)
  (fold-right push '() sequence))
