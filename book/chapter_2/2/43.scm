

;; from 42.
;; (flatmap
;;  (lambda (rest-of-queens)
;;    (map (lambda (new-row)
;;           (adjoin-position new-row k rest-of-queens))
;;         (enumerate-interval 1 board-size)))
;;  (queen-cols (- k 1)))

;; from 43.
;; (flatmap
;;  (map (lambda (new-row)
;;         (lambda (rest-of-queens)
;;           (adjoin-position new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; on 42, the code would first generate the interval for the board size,
;; get the positions for k-1 and then extend it to include the new queen
;; each queen-cols call will do board-size calls on adjoin-position
;; and call itself recursively with board-size-1 (until it reaches 0)

;; on 43, with the slower way we're doing the whole thing over again for each possibility
;; it's basically enumerating the interval and for each of those calling queen-cols k-1
;; each queen-cols call will do board-size calls to queen-cols k-1, so that's the issue

;; didn't get into any time calculations though
