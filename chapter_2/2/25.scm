;; 1. (1 3 (5 7) 9)
(define x (list 1 3 (list 5 7) 9))

;; (cdr x)
;; (3 (5 7) 9)
;; (cdr)
;; ((5 7) 9)
;; (car)
;; (5 7)
;; (cdr)
;; (7)
;; (car)
;; 7

(car (cdr (car (cdr (cdr x)))))

;; 2. ((7))
(define x (list (list 7)))

;; (car x)
;; (7)
;; (car)
;; 7

(car (car x))

;; 3. (1 (2 (3 (4 (5 (6 7))))))
(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;; (cdr x)
;; ((2 (3 (4 (5 (6 7))))))
;; (car x)
;; (2 (3 (4 (5 (6 7)))))
;; (cdr)
;; ((3 (4 (5 (6 7)))))
;; (car)
;; (3 (4 (5 (6 7))))
;; (cdr)
;; ((4 (5 (6 7))))
;; (car)
;; (4 (5 (6 7)))
;; (cdr)
;; ((5 (6 7)))
;; (car)
;; (5 (6 7))
;; (cdr)
;; ((6 7))
;; (car)
;; (6 7)
;; (cdr)
;; (7)
;; (car)
;; 7

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))
