;; Programming Languages B, Week 02 - Extra Problems

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(require "hw5.rkt")

;; Racket structs:
(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

(define (tree-height btree)
  (if (btree-leaf? btree)
      0
      (let ([left (btree-node-left btree)]
            [right (btree-node-right btree)])
        (max (+ 1 (tree-height left))
             (+ 1 (tree-height right))))))

(define (sum-tree btree)
  (if (btree-leaf? btree)
      0
      (+ (sum-tree (btree-node-left btree))
         (sum-tree (btree-node-right btree))
         (btree-node-value btree))))

(define (prune-at-v btree v)
  (if (btree-leaf? btree)
      (btree-leaf)
      (let ([value (btree-node-value btree)])
        (if (equal? value v)
          (btree-leaf)
          (btree-node value
                      (prune-at-v (btree-node-left btree) v)
                      (prune-at-v (btree-node-right btree) v))))))

(define (well-formed-tree? x)
  (or (btree-leaf? x)
      (btree-node? x)))

;; fold left side first
(define (fold-tree f acc btree)
  (if (btree-leaf? btree)
      acc
      (fold-tree f
                 (fold-tree f
                            (f acc
                               (btree-node-value btree))   
                            (btree-node-left btree))
                 (btree-node-right btree))))

(define (fold-tree-curr f acc)
  (lambda (btree)
    (if (btree-leaf? btree)
        acc
        ((fold-tree-curr f
                        ((fold-tree-curr f
                                        (f (btree-node-value btree) acc))
                         (btree-node-left btree)))
         (btree-node-right btree)))))

;; Dynamic typing:
(define (crazy-sum lst)
  (define (helper lst binf)
    (if (null? lst)
        (if (or (equal? + binf)
                (equal? - binf))
            0
            1)
        (let ([head (car lst)]
              [tail (cdr lst)])
          (if (procedure? head)
              (head (car tail) (helper (cdr tail) head))
              (binf head (helper tail binf))))))
  (helper lst +))

(define (either-fold f acc foldable)
  (cond [(list? foldable) (foldl f acc foldable)]
        [(or (btree-leaf? foldable)
             (btree-node? foldable))
         (fold-tree f acc foldable)]
        [#t (error "either-fold argument must be a foldable")]))

(define (flatten lst)
  (if (null? lst)
      null
      (if (list? (car lst))
          (append (flatten (car lst))
                  (flatten (cdr lst)))
          (cons (car lst)
                (flatten (cdr lst))))))

;; Using lambda-calculus ideas to remove features from MUPL programs:
(define (remove-lets mupl-exp)
  (cond [(or (int? mupl-exp)
             (aunit? mupl-exp)
             (var? mupl-exp))
         mupl-exp]

        [(fst? mupl-exp) (remove-lets (fst-e mupl-exp))]
        [(snd? mupl-exp) (remove-lets (snd-e mupl-exp))]
        [(isaunit? mupl-exp) (remove-lets (isaunit-e mupl-exp))]
        
         [(closure? mupl-exp)
          (closure (closure-env mupl-exp)
                   (remove-lets (closure-fun mupl-exp)))]

         [(fun? mupl-exp)
          (fun (fun-nameopt mupl-exp)
               (fun-formal mupl-exp)
               (remove-lets (fun-body mupl-exp)))]

         [(apair? mupl-exp)
          (apair (remove-lets (apair-e1 mupl-exp))
                 (remove-lets (apair-e2 mupl-exp)))]

         [(add? mupl-exp)
          (add (remove-lets (add-e1 mupl-exp))
               (remove-lets (add-e2 mupl-exp)))]

         [(ifgreater? mupl-exp)
          (ifgreater (remove-lets (ifgreater-e1 mupl-exp))
                     (remove-lets (ifgreater-e2 mupl-exp))
                     (remove-lets (ifgreater-e3 mupl-exp))
                     (remove-lets (ifgreater-e4 mupl-exp)))]

         [(call? mupl-exp)
          (call (remove-lets (call-funexp mupl-exp))
                (remove-lets (call-actual mupl-exp)))]

         [(mlet? mupl-exp)
          (call (fun #f
                     (mlet-var mupl-exp)
                     (remove-lets (mlet-body mupl-exp)))
                (remove-lets (mlet-e mupl-exp)))]

         [#t (error (format "bad MUPL expression: ~v" mupl-exp))]))


;; More MUPL functions:
(define mupl-all
  (fun "all" "mlst"
       (ifaunit (var "mlst")
                (int 1)
                (ifeq (fst (var "mlst"))
                      (int 0)
                      (int 0)
                      (call (var "all")
                            (snd (var "mlst")))))))

(define mupl-append
  (fun "append" "mlst1"
       (fun "loop" "mlst2"
            (ifaunit (var "mlst2")
                     (var "mlst1")
                     (apair (fst (var "mlst2"))
                            (call (var "loop")
                                  (snd (var "mlst2"))))))))

(define mupl-zip
  (fun "zip" "mlst-pair"
       (mlet* (list (cons "mlst1" (fst (var "mlst-pair")))
                    (cons "mlst2" (snd (var "mlst-pair"))))
              (ifaunit (var "mlst1")
                       (aunit)
                       (ifaunit (var "mlst2")
                                (aunit)
                                (apair (apair (fst (var "mlst1"))
                                              (fst (var "mlst2")))
                                       (call (var "zip")
                                             (apair (snd (var "mlst1"))
                                                    (snd (var "mlst2"))))))))))

(define mupl-curry
  (fun "curry" "f"
       (fun #f "x"
            (fun #f "y"
                 (call (var "f")
                       (apair (var "x") (var "y")))))))

(define mupl-uncurry
  (fun "uncurry" "f"
       (fun #f "param-pair"
            (call (call (var "f")
                        (fst (var "param-pair")))
                  (snd (var "param-pair"))))))

;; More MUPL macros:
(define (ifgreater3 e1 e2 e3 e4 e5)
  (mlet* (list (cons "_x" e1) (cons "_y" e2) (cons "_z" e3))
         (ifgreater (var "_x")
                    (var "_y")
                    (ifgreater (var "_y") (var "_z") e4 e5)
                    e5)))

(define (call-curried mexp mexp-lst)
  (if (null? (cdr mexp-lst))
      (call mexp (car mexp-lst))
      (call-curried (call mexp (car mexp-lst))
                    (cdr mexp-lst))))
