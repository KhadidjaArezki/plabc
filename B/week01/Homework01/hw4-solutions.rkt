
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
;; A stream is defined as a thunk that
;; when called returns a pair (v . #procedure)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define test
  (letrec ([f (lambda (b) (cons b (lambda () (f (not b)))))])
    (lambda () (f #t))))
;; put your code below

;; produces a list of numbers from low to high (including
;; low and possibly high) separated by stride and in sorted order
(define (sequence low high stride)
  (if (<= stride 0) (error "sequence: stride must be a positive number")
      (if (> low high)
          '()
          (cons low (sequence (+ low stride) high stride)))))

;; produce a string list with suffix
;; appended to each string in the list
(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix))
       xs))

;;return xs[i], i =  n mod (length xs)
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs
                          (remainder n (length xs)))))))

(define (twice-each s)
  (lambda ()
    (let ([pr (s)])
      (cons (car pr)
            (lambda ()
              (cons (car pr)
                    (twice-each (cdr pr))))))))

;; produce a list holding the first
;; n values produced by s in order
(define (stream-for-n-steps s n)
  (if (= n 0)
      '()
      (let ([p (s)])
        (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))))

;(stream-for-n-steps (twice-each nats) 10)

;; produce a stream of natural numbers except
;; numbers divisble by 5 are negated
(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([th (lambda () (f (+ x 1)))])
                  (if (= (remainder x 5) 0)
                      (cons (- x) th)
                      (cons x th))))])
    (lambda () (f 1))))


;; produce a stream where  elements alternate
;; between the strings "dan.jpg" and "dog.jpg"
(define dan-then-dog
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (next x)))))]
           [next (lambda (s)
                   (cond
                     [(equal? s "dan.jpg") "dog.jpg"]
                     [(equal? s "dog.jpg") "dan.jpg"]
                     [#t s]))])
    (lambda () (f "dan.jpg"))))


;; for each value v produced by the stream
;; produces a pair (0, v)
(define (stream-add-zero s)
  (lambda ()
    (let ([p (s)])
      (cons (cons 0 (car p)) (stream-add-zero (cdr p))))))


;; cycles through elements of xs and ys forever
;; produces a pair of the next two elements of xs and ys
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
            (cons (cons (list-nth-mod xs n)
                        (list-nth-mod ys n))
                  (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))


;; return the first pair with a
;; car field equal to v, else #f
(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda (i)
                (cond [(equal? l 0) #f]
                      [(>= i l) #f]
                      [#t (let ([x (vector-ref vec i)])
                            (if (pair? x)
                                (if (equal? (car x) v)
                                    x
                                    (f (+ i 1)))
                                (f (+ i 1))))]))])
    (f 0)))


;; create an n-element cache of recent results
;; cache = (vector (cons v1 r1) (cons v2 r2) ...) 
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [pos 0])
    (lambda (v)
      (let ([cached-result (vector-assoc v cache)])
        (if cached-result
            (cdr cached-result)
            (let ([result (assoc v xs)])
              (if (not result)
                  #f
                  (let ([new-pos (if (>= pos (- n 1)) 0 (+ pos 1))])
                    (begin
                      (vector-set! cache pos (cons v result))
                      (set! pos new-pos))
                      result))))))))


;; e1 and e2 produce numbers
;; evaluates e1 once => v1, and keeps
;; evaluating e2 => v2 until v2 >= v1, return #t
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
              [f (lambda ()
                  (if (< (begin e2) v1)
                      (f)
                      #t))])
       (f))]))
