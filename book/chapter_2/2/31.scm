(define (square x) (* x x))

(define (tree-map proc)
  (define (recur tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (recur sub-tree)
               (proc sub-tree)))
         tree))
  recur)

(define square-tree (tree-map square))


;; I understood from question that the request was to do the above, but schemewiki shows the below
;; I don't fully understand the declaration syntax used in the book:
;; (define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (tree-map proc sub-tree)
              (proc sub-tree)))
        tree))

