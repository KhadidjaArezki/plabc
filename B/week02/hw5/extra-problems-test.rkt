;; Programming Languages B, Week 02 - Extra Problems

#lang racket
(require "extra-problems.rkt")
(require "hw5.rkt")
(require rackunit)

(define leaf (btree-leaf))
(define short-tree (btree-node 16 leaf leaf))
(define tall-tree (btree-node 4 short-tree leaf))
(define taller-tree (btree-node 2 tall-tree short-tree))

(define tests
  (test-suite
   "Sample tests for PLB Week 02 Extra Problems"

   ;; sum-tree tests
   (check-equal? (sum-tree leaf) 0 "sum-tree test of sum 0")
   (check-equal? (sum-tree short-tree) 16 "sum-tree test of sum 16")
   (check-equal? (sum-tree tall-tree) 20 "sum-tree test of sum 20")
   (check-equal? (sum-tree taller-tree) 38 "sum-tree test of sum 3")

   ;; tree-height tests
   (check-equal? (tree-height leaf) 0 "tree-height test of length 0")
   (check-equal? (tree-height short-tree) 1 "tree-height test of length  1")
   (check-equal? (tree-height tall-tree) 2 "tree-height test of length 2")
   (check-equal? (tree-height taller-tree) 3 "tree-height test of length 3")

   ;; prune-at-v tests
   (check-equal? (prune-at-v leaf 2) (btree-leaf) "prune-at-v test of leaf")
   (check-equal? (prune-at-v short-tree 2) (btree-node 16 (btree-leaf) (btree-leaf)) "prune-at-v test of short-tree and value 2")
   (check-equal? (prune-at-v short-tree 16) (btree-leaf) "prune-at-v test of short-tree and value 16")

   ;; well-formed-tree tests
   (check-equal? (well-formed-tree? leaf) #t "well-formed-tree test of leaf")
   (check-equal? (well-formed-tree? 0) #f "well-formed-tree test of non binary tree")
   (check-equal? (well-formed-tree? short-tree) #t "well-formed-tree test of node")

   ;; fold-tree tests
   (check-equal? (fold-tree (lambda (x y) (+ x y 1)) 7 
                            (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) 18 "fold-tree test of acc 18")

   ;; fold-tree-curr tests
   (check-equal? ((fold-tree-curr (lambda (x y) (+ x y 1)) 7) 
                            (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) 18 "fold-tree-curr test of acc 18")

   ;; crazy-sum tests
   (check-equal? (crazy-sum (list 10 * 6 / 5 - 3)) 20 "crazy-sum test of 20")

   ;; either-fold tests
   (check-equal? (either-fold (lambda (x y) (+ x y 1)) 0 (list 1 2 3)) 9 "either-fold test for a list")
   (check-equal? (either-fold (lambda (x y) (+ x y 1)) 7
                            (btree-node 4 (btree-node 5 (btree-leaf) (btree-leaf)) (btree-leaf))) 18 "either-fold test for a binary tree")

   ;; flatten tests
   (check-equal? (flatten (list 1 2 (list (list 3 4) 5 (list (list 6) 7 8)) 9 (list 10))) (list 1 2 3 4 5 6 7 8 9 10))

   ;; mupl-all tests
   (check-equal? (eval-exp (call mupl-all (apair (int 1) (aunit)))) (int 1) "mupl-all test for true")
   (check-equal? (eval-exp (call mupl-all (apair (int 1) (apair (int 0) (aunit))))) (int 0) "mupl-all test for false")

   ;; mupl-append tests
   (check-equal? (eval-exp (call (call mupl-append (apair (int 3) (aunit)))
                                 (apair (int 1) (apair (int 2) (aunit)))))
                 (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))) "mupl-append test for mupl list of 3 elements")

   ;; mupl-zip tests
   (check-equal? (eval-exp (call mupl-zip (apair (apair (int 1) (aunit))
                                                 (apair (int 2) (apair (int 3) (aunit))))))
                 (apair (apair (int 1) (int 2)) (aunit)) "mupl-zip test of two lists of different lengthes")

   ;; mupl-curry tests
   (check-equal? (eval-exp (call (call (call mupl-curry mupl-zip)
                                       (apair (int 1) (aunit)))
                                 (apair (int 2) (apair (int 3) (aunit)))))
                 (apair (apair (int 1) (int 2)) (aunit)) "mupl-curry test with mupl-zip")

   ;; mupl-uncurry tests
   (check-equal? (eval-exp (call (call mupl-uncurry mupl-append)
                                 (apair (apair (int 3) (aunit)) (apair (int 1) (apair (int 2) (aunit))))))
                 (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))) "mupl-uncurry test with mupl-append")

   ;; ifgreater3 tests
   (check-equal? (eval-exp (ifgreater3 (int 3) (int 2) (int 1) (int 4) (int 5))) (int 4) "ifgreater3 test for true")
   (check-equal? (eval-exp (ifgreater3 (int 3) (int 2) (int 3) (int 4) (int 5))) (int 5) "ifgreater3 test for false")

   ;; call-curried tests
   (check-equal? (eval-exp (call-curried mupl-append (list (apair (int 3) (aunit))
                                                            (apair (int 1) (apair (int 2) (aunit))))))
                           (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))) "call-curried test with mupl-append")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
