#lang racket

(require berkeley)

; assignment
; https://people.eecs.berkeley.edu/~bh/61a-pages/Volume1/Project1/nodate-21.pdf
; code taken from:
; https://people.eecs.berkeley.edu/~bh/61a-pages/Lib/twenty-one.scm
; make-deck needed to be fixed (replace -1+'s)

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
          ((< (best-total dealer-hand-so-far) 17)
           (play-dealer customer-hand
                        (se dealer-hand-so-far (first rest-of-deck))
                        (bf rest-of-deck)))
          ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
          ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
          (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
          ((strategy customer-hand-so-far dealer-up-card)
           (play-customer (se customer-hand-so-far (first rest-of-deck))
                          dealer-up-card
                          (bf rest-of-deck)))
          (else
           (play-dealer customer-hand-so-far
                        (se dealer-up-card (first rest-of-deck))
                        (bf rest-of-deck)))))
  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
                   (first (bf (bf deck)))
                   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (map (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)))

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
          (se (first in) (shuffle (se (bf in) out) (- size 1)))
          (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
        deck
        (move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

;;; rules:
;; cards are words
; e.g.: 10s -> 10 of spades
;; cards:
; a -> ace
; j -> jack
; q -> queen
; k -> king
; 2 - 10 -> regulars
;; points
; j/q/k -> 10
; a -> 1 or 11 (player can choose)
; 2 - 10 -> 2 - 10
;; suits:
; c -> clubs
; d -> diamons
; h -> hearts
; s -> spades

; 1. The program in the library is incomplete. It lacks a procedure best-total that takes a
; hand (a sentence of card words) as argument, and returns the total number of points in the
; hand. It’s called best-total because if a hand contains aces, it may have several different
; totals. The procedure should return the largest possible total that’s less than or equal to
; 21, if possible.

; Write best-total
(define (best-total-for n)
  (define (sum-specs v-fn partial specs)
    (if (= specs 0)
        partial
        (sum-specs v-fn (+ partial (v-fn partial specs)) (- specs 1))))
  (define (sum-as partial as)
    (sum-specs (lambda (partial as)
                 (if (> (+ partial 11 (- as 1)) n)
                     1
                     11))
               partial
               as))
  (define (rank-value r)
    "rank-value returns the value for numeric or picture ranks excluding aces"
    (cond ((integer?  r) r)
          ((or (eq? r 'j) (eq? r 'q) (eq? r 'k)) 10)
          (else 0)))
  (define (partial hand as total)
    "partial calculates the sub-total (excluding aces) and return that and number of as"
    (if (empty? hand)
        (se total as)
        (let ((rank (bl (first hand))))
          (if (eq? rank 'a)
              (partial (bf hand) (+ as 1) total)
              (partial (bf hand) as (+ total (rank-value rank)))))))
  (define (total partial-res)
    "total calls sum-as with the partial result to return the best-total value"
    (sum-as (first partial-res) (last partial-res)))
  (lambda (hand)
    "returns the best possible total for the hand (where hand value <= n)"
    (total (partial hand 0 0))))

(define best-total (best-total-for 21))

;; results:
;-> (best-total '(ad 8s))
;19
;-> (best-total '(ad 8s 5h))
;14
;-> (best-total '(ad as 9h))
;21
;-> (best-total '(ad as 9h 10h))
;21
;-> (best-total '(ad as 9h 10h 5s))
;26

; 2. define a strategy procedure stop-at-17 that's identical to the dealer's, i.e., takes a card if and only if the total so far is less than 17.
(define (stop-at n)
  "returns a strategy that will only draw another card if the best-total value (of n - 1) for hand is less than n"
  (lambda (hand dealer-card)
    (if (< ((best-total-for (- n 1)) hand) n)
        #t
        #f)))

(define stop-at-17 (stop-at 17))

; 3. write a procedure play-n such that
; (play-n strategy n)
; plays n games with a given strategy and returns the number of games that
; the cusomer won minus the number that s/he lost.
(define (play-n strategy n)
  "play-n will play n games with given strategy and return wins - losses"
  (define (iter counter score)
    (if (= counter 0)
        score
        (iter (- counter 1) (+ score (twenty-one strategy)))))
  (iter n 0))

; 4. define a strategy named dealer-sensitive that "hits" (takes a card) if (and only if):
; - the dealer has an ace, 7, 8, 9, 10, OR picture card showing AND the customer has less than 17; OR
; - the dealer has a 2, 3, 4, 5 OR 6 showing AND the customer has less than 12.
; (The idea is that in the second case, the dealer is much more likely to "bust" (go over 21), since
; there are more 10-pointers than anything else)
(define (dealer-sensitive hand dealer-card)
  (define (check-rank predicate? card)
    "check-rank is a helper for running a predicate on a cards' rank"
    (predicate? (bl card)))
  (define (high? card)
    "high? returns #t if the given card is considered high in value (7 to 10, picture cards or aces)"
    (check-rank (lambda (rank) (or (eq? rank 'a)
                                   (eq? rank 7)
                                   (eq? rank 8)
                                   (eq? rank 9)
                                   (eq? rank 10)
                                   (eq? rank 'j)
                                   (eq? rank 'q)
                                   (eq? rank 'k)))
                card))
  (define (low? card)
    "low? returns #t if the given card is considered low in value (2 to 6)"
    (check-rank (lambda (rank) (or (eq? rank 2)
                                   (eq? rank 3)
                                   (eq? rank 4)
                                   (eq? rank 5)
                                   (eq? rank 6)))
                card))
  (cond ((and (high? dealer-card) ((stop-at 17) hand dealer-card)) #t)
        ((and (low? dealer-card) ((stop-at 12) hand dealer-card)) #t)
        (else #f)))

; 5. generalize part 2 above by defining a function stop-at
; that's done on 2 already

; 6. On Valentine's day, your local casino has a special deal: if you win a round of 21 with a
; heart in your hand, they pay double. ... Write a valentine strategy that stops at 17 unless
; you have a hear in your hand, in which case it stops at 19
(define (contains cards transform v)
  "contains checks if there's a value v = transform(c) for c in cards"
  (cond ((empty? cards) #f)
        ((eq? (transform (first cards)) v) #t)
        (else (contains (bf cards) transform v))))
(define (contains-heart? hand)
  "contains-heart? returns true if there are any cards of the heart suit in hand - false otherwise"
  (contains hand last 'h))
(define (valentine hand dealer-card)
  "valentine is a strategy that will stop drawing cards at 19 points, if there's a heart in the current hand
otherwise, it will stop drawing cards at 17 points"
  (if (contains-heart? hand)
      ((stop-at 19) hand 'a)
      ((stop-at 17) hand 'a)))

; 7. generalive part 6 above by defining a function suit-strategy that takes three arguments:
; a suit (h, s, d, or c), a strategy to be used if your hand doesn't include that suit, and a
; strategy to be used if your hand does include that suit. ... Show how you could use this function and
; the stop-at function from part 5 above to redefine the valentine strateg of part 6.
(define (contains-suit? hand suit)
  "contains-suit? returns true if hand contains any card from given suit"
  (contains hand last suit))
(define (suit-strategy suit has-strategy not-strategy)
  (lambda (hand dealer-card)
    (if (contains-suit? hand suit)
        (has-strategy hand dealer-card)
        (not-strategy hand dealer-card))))

(define valentine (suit-strategy 'h (stop-at 19) (stop-at 17)))

; 8. Define a function majority that takes three strategies as arguments and produces a
; strategy as a result, such that the result strategy always decides whether or not to “hit”
; by consulting the three argument strategies, and going with the majority. That is, the
; result strategy should return #t if and only if at least two of the three argument strategies
; do. Using the three strategies from parts 2, 4, and 6 as argument strategies, play a few
; games using the “majority strategy” formed from these three
(define (majority s1 s2 s3)
  (lambda (hand dealer-card)
    (let ((res1 (s1 hand dealer-card))
          (res2 (s2 hand dealer-card))
          (res3 (s3 hand dealer-card)))
      (cond ((and res1 res2) #t)
            ((and res1 res3) #t)
            ((and res2 res3) #t)
            (else #f)))))

; 9. Some people just can’t resist taking one more card. Write a procedure reckless that
; takes a strategy as its argument and returns another strategy. This new strategy should
; take one more card than the original would. (In other words, the new strategy should
; stand if the old strategy would stand on the butlast of the customer’s hand.)
(define (reckless s)
  (lambda (hand dealer-card)
    (s (bl hand) dealer-card)))
