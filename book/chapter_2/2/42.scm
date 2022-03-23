;; helpers
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; implement the representation for sets of board positions
;; adjoin-position -> adjoins a new row-column position to a set of positions
;; empty-board -> represents an empty set of positions
;; safe? -> determines for a set of positions whether the queen in the ktth column is safe with respect to the others
;; we need only check whether the new queen is safe - the others are already guaranteed

;; positions will be a set of (row, col) items
;; rest-of-queens is basically queen-cols of k-1 (which is the representation of all the possible ways to place k-1 queens in the board)
;; k is the current col

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (let ((pos (list (list new-row k))))
    (if (null? rest-of-queens)
        pos
        (append pos rest-of-queens))))

;; lol, I suspected I was overdoing it but this is just way much simpler:
;; taken from: http://community.schemewiki.org/?sicp-ex-2.42
(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

;; just for clarity
(define pos-row car)
(define pos-col cadr)

(define (in-queen-range? pos1 pos2)
  (define (diag? pos1 pos2)
    (= (abs (- (pos-row pos1) (pos-row pos2))) (abs (- (pos-col pos1) (pos-col pos2)))))
  (or (= (pos-row pos1) (pos-row pos2))
      (= (pos-col pos1) (pos-col pos2))
      (diag? pos1 pos2)))

;; NOTE: we're not actually using k since adjoin-position will always append
;; the new queen to the start of the list (so we just pick the first one)
(define (safe? k positions)
  (define (iter k-pos remaining-pos)
    (cond ((null? remaining-pos) #t)
          ((in-queen-range? k-pos (car remaining-pos)) #f)
          (else (iter k-pos (cdr remaining-pos)))))
  (iter (car positions) (cdr positions)))

;; there are some cool solutions using accumulate in the scheme wiki page,
;; so I thought I'd give it a go at something in that sense
;; (changed it a bit for the exercise - the version over there does an acc
;; on integers and check for equality on 0)
;; the above version should still be a little bit more performant since it
;; doesn't need to go over the entire list
(define (safe? k positions)
  (not (accumulate (lambda (x y) (or (in-queen-range? (car positions) x) y)) #f (cdr positions))))
