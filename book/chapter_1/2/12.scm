;; 1.12

;;   |0       1
;;   |1      1 1
;;   |2     1 2 1
;;   |3    1 3 3 1
;;   |4   1 4 6 4 1
;;   |5  1 510 105 1
;; y V -------------> x

(define (pascal y x)
  (cond ((or (= y 0) (= x 0) (= x y)) 1)
        (else (+ (pascal (- y 1) x)
                 (pascal (- y 1) (- x 1))))))

;; (pascal 3 1)
;; (+ (pascal ))
