
;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs)
;;              '()
;;              (cons (accumulate op init (??))
;;                    (acumulate-n op init (??))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) '() seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) '() seqs)))))

;; this can be simplified. If we consider that map can be defined in terms of accumulate:
;; (define (map p sequence)
;;   (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
;; we can simply replace the inner accumulate with map calls

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
