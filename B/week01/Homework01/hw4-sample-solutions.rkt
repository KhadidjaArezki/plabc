#lang racket

(provide (all-defined-out))

;; Homework 04 sample solutions

;; Problem 01 - sequence

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 02 - string-append-map

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 03 - list-nth-mod

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: n must be non-negative")]
        [(null? xs) (error "list-nth-mod: list must be non-empty")]
        [#t (let* ([len (length xs)]
                   [posn (remainder n len)])
              (car (list-tail xs posn)))]))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 04 -stream-for-n-steps

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)])
            (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 05 - funny-number-stream

(define funny-number-stream 
  (letrec ([f (lambda (n) (cons (if (= (remainder n 5) 0) (- n) n)
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 06 - dan-then-dog

(define (dan-then-dog)
  (cons "dan.jpg"
        (lambda () (cons "dog.jpg" dan-then-dog))))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 07 - stream-add-zero

(define (stream-add-zero s)
  (lambda ()
    (let ([next (s)])
      (cons (cons 0 (car next)) (stream-add-zero (cdr next))))))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 08 - cycle-lists

(define (cycle-lists xs ys)
  (letrec ([loop (lambda (n)
                   (cons (cons (list-nth-mod xs n)
                               (list-nth-mod ys n))
                         (lambda () (loop (+ n 1)))))])
    (lambda () (loop 0))))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 09 - vector-assoc

(define (vector-assoc v vec)
  (letrec ([loop (lambda (i)
                   (if (= i (vector-length vec))
                       #f
                       (let ([x (vector-ref vec i)])
                         (if (and (cons? x) (equal? (car x) v))
                             x
                             (loop (+ i 1))))))])
    (loop 0)))

;;;;;;;;;;;;;;;;;;;;;;

;; Problem 10 -

(define (cached-assoc lst n)
  (let ([cache (make-vector n #f)]
        [next-to-replace 0])
    (lambda (v)
      (or (vector-assoc v cache)
          (let ([ans (assoc v lst)])
            (and ans
                 (begin (vector-set! cache next-to-replace ans)
                        (set! next-to-replace 
                              (if (= (+ next-to-replace 1) n)
                                  0
                                  (+ next-to-replace 1)))
                        ans)))))))


;;;;;;;;;;;;;;;;;;;;;;

;; Challenge problem 

(define-syntax while-less
  (syntax-rules (do)
    ((while-less x do y)
      (let ([z x])
        (letrec ([loop (lambda ()
                         (let ([w y])
                           (if (or (not (number? w)) (>= w z))
                               #t
                               (loop))))])
          (loop))))))
