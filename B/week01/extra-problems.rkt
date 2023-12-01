#lang racket
(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define (stream-for-n-steps s n)
  (if (= n 0)
      '()
      (let ([p (s)])
        (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))))

;; palindromic: produces a list of numbers such that:
;; the first element should be the sum of the first and the last elements of ns
;; the second one should be the sum of the second and second last elements 
(define (palindromic ns)
   (if (null? ns)
        null
        (letrec ([len (length ns)]
                 [loop (lambda (i)
                         (if (= i len)
                             null
                             (cons
                              (+ (list-ref ns i) (list-ref ns (- len (+ i 1))))
                              (loop (+ i 1)))))])
          (loop 0))))

;; fibonacci: stream that produces 0, 1, and then each successive
;; element is the sum of two immediately preceding elements
(define fibonacci
  (letrec ([prev 0]
           [curr 1]
           [f (lambda ()
                (let ([temp (+ prev curr)])
                  (begin 
                    (set! prev curr)
                    (set! curr temp)
                    (cons curr (lambda () (f))))))])
    (lambda () (f))))

;; stream-until: and applies f to the values produced
;; by s in succession until f evaluates to #f
(define (stream-until f s)
  (let* ([p (s)]
         [res (f (car p))])
    (if (not res)
        (car p)
        (stream-until f (cdr p)))))


;; stream-map: produce a stream whose values are the
;; result of applying f to the values produced by s
(define (stream-map f s)
  (letrec ([g (lambda (p)
               (cons (f (car p)) (lambda () (g ((cdr p))))))])
    (lambda () (g (s)))))

;; stream-zip: return a stream by zipping together each value
;; produced by s1 with a value produced by s2
;; (v1 v2 . #procedure)...
(define (stream-zip s1 s2)
  (letrec ([f (lambda (p1 p2)
                (cons (cons (car p1) (car p2)) (lambda () (f ((cdr p1)) ((cdr p2))))))])
    (lambda () (f (s1) (s2)))))

;; interleave: returns a stream by taking one
;; element from each stream in ss in sequence
;; for each stream in ss, call, get car and cdr
;; return a list of cars and a thunk with list of cdrs as arg
;; (cons (cons (car p1) (cons (car p2) (cons (car p3) ...))) (lambda () (f (ps.forEach(p => (cdr p))))))
(define (interleave ss)
  (letrec ([f (lambda (streams)
                (letrec ([ths null]
                        [g (lambda (stream-list)
                          (if (null? stream-list)
                              null
                              (let ([p1 ((car stream-list))])
                                (begin (set! ths (append ths  (list (cdr p1))))
                                       (cons (car p1) (g (cdr stream-list)))))))])
                  (cons (g streams) (lambda () (f ths)))))])
    (lambda () (f ss))))


;; pack: returns a stream that produces a
;; list of the next n elements produced by s
(define (pack n s)
  (letrec ([f (lambda (th)
                (letrec ([g (lambda (m p)
                              (if (= m 1)
                                  (begin
                                    (set! th (cdr p))
                                    (cons (car p) null))
                                  (cons (car p) (g (- m 1) ((cdr p))))))])
                  (cons (g n (th)) (lambda () (f th)))))])
    (lambda () (f s))))

;; sqrt-stream: use Newton's Method for approximating the square root of a number.
;; Start with n as an initial guess in the stream, and produces successive
;; guesses applying fn(x) = 1/2(x + n/x) to the current guess
(define (sqrt-stream n)
  (letrec ([f (lambda (guess)
                (let ([next-guess (* 0.5 (+ guess (/ n guess)))])
                  (cons guess (lambda () (f next-guess)))))])
    (lambda () (f n))))

;; approx-sqrt: use sqrt-stream to return a number x
;; such that x*x is within e of n, i.e., x*x = n+-e
(define (approx-sqrt n e)
  (letrec ([f (lambda (p)
                (let* ([x (car p)])
                  (if (or (= n (+ (* x x) e)) (= n (- (* x x) e)))
                      x
                      (f ((cdr p))))))])
    (f ((sqrt-stream n)))))


(define-syntax perform
  (syntax-rules (if unless)
    [(perform e1 if e2)
     (let ([v e2])
       (if v e1 v))]
    [(preform e1 unless e2)
       (if (not e2) e1 #f)]))

;; (perform (begin (print "e1") 1) if (begin (print "e2") 2))
;; (perform (begin (print "e1") 1) if (begin (print "e2") #f))
;; (perform (begin (print "e1") 1) unless (begin (print "e2") #f))
;; (perform (begin (print "e1") 1) unless (begin (print "e2") 2))
;; (approx-sqrt 16.0 0.01)
;; (stream-for-n-steps (sqrt-stream 16)10)
;; (stream-for-n-steps (pack 5 nats) 3)
;; (define evens (stream-map (lambda (n) (* n 2)) nats))
;; (stream-for-n-steps (interleave (list evens nats ones)) 10)
;; (stream-for-n-steps (stream-map (lambda (n) (* n 2)) nats) 10) 
;; (stream-until (lambda (n) (< n 10)) nats)
;; (cons 0(cons 1(stream-for-n-steps fibonacci 14))) ;; (list 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
;; (cons 0(cons 1(stream-for-n-steps fibonacci 10)))
;; (palindromic (list 1 2 4 8)) ;; (list 9 6 6 9)
;; (stream-for-n-steps nats 10)