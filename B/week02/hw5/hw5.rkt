;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

(define (mupllist->racketlist mlst)
  (if (aunit? mlst)
      null
      (cons (apair-e1 mlst) (mupllist->racketlist (apair-e2 mlst)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (begin (print env) (error "unbound variable during evaluation" str))]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(int? e) e]

        [(closure? e) e]

        [(aunit? e) e]

        [(var? e) 
         (eval-under-env (envlookup env (var-string e)) env)]

        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        ;; CHANGE add more cases here
        [(fun? e)
         (closure env e)]

        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL number comparison applied to non-number")))]

        [(call? e)
         (let* ([v1 (eval-under-env (call-funexp e) env)]
                [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([f (closure-fun v1)]
                      [fun-name (fun-nameopt f)]
                      [fun-param (fun-formal f)]
                      [fun-body (fun-body f)]
                      [fun-env (closure-env v1)]
                      [ext-env
                      (if fun-name
                          (cons (cons fun-name v1)
                                (cons (cons fun-param v2) fun-env))
                          (cons (cons fun-param v2) fun-env))])
                 (eval-under-env fun-body ext-env))
               (error "MUPL function call applied to a non function expression")))]

        [(mlet? e)
         (let* ([v1 (mlet-e e)]
               [v2 (mlet-body e)]
               [ext-env (cons (cons (mlet-var e) v1) env)])
           (eval-under-env v2 ext-env))]

        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
               (eval-under-env (apair-e2 e) env))]

        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p)
               (eval-under-env (apair-e1 p) env)
               (error "MUPL fst applied to non-pair")))]

        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p)
               (eval-under-env (apair-e2 p) env)
               (error "MUPL snd applied to non-pair")))]

        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst) e2
      (mlet
       (car (car lstlst))
       (cdr (car lstlst))
       (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x")
                    (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun "mupl-map"
       "param-fun"
       (fun "loop"
            "mlst"
            (ifaunit (var "mlst")
             (aunit)
             (apair (call (var "param-fun")
                          (fst (var "mlst")))
                    (call (var "loop") (snd (var "mlst"))))))))

(define mupl-mapAddN  
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "x" (add (var "i") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  ; helper is a procedure that parses fun-body and stores free variables in a set and returns it.
  ; It keeps track of funcion's bound vars in a second set.
  ;    A free var is one that is not defined anywhere in the function body:
  ;    It's not a reference to the function or its argument
  ;    It's not a local var bound in a  mlet exp inside the fun-body
  
  (define (helper body boundvars freevars)
    (cond [(var? body)
           (let ([v (var-string body)])
             (if (set-member? boundvars v)
               freevars
               (set-add freevars v)))]

          [(apair? body)
           (set-union (helper (apair-e1 body) boundvars freevars)
                      (helper (apair-e2 body) boundvars freevars))]

          [(mlet? body)
           (let ([new-boundvars (set-add boundvars (mlet-var body))])
             (set-union (helper (mlet-e body) new-boundvars freevars)
                      (helper (mlet-body body) new-boundvars freevars)))]

          [(add? body)
           (set-union (helper (add-e1 body) boundvars freevars)
                      (helper (add-e2 body) boundvars freevars))]

          [(ifgreater? body)
           (set-union (helper (ifgreater-e1 body) boundvars freevars)
                      (helper (ifgreater-e2 body) boundvars freevars)
                      (helper (ifgreater-e3 body) boundvars freevars)
                      (helper (ifgreater-e4 body) boundvars freevars))]

          [(fst? body) (helper (fst-e body) boundvars freevars)]

          [(snd? body) (helper (snd-e body) boundvars freevars)]

          [(call? body)
           (set-union (helper (call-funexp body) boundvars freevars)
                      (helper (call-actual body) boundvars freevars))]

          [(fun? body)
           (let ([new-boundvars (set-union boundvars
                                           (set (fun-nameopt body) (fun-formal body)))])
             (helper (fun-body body) new-boundvars freevars))]

          [(closure? body)
           (helper (closure-fun body) boundvars freevars)]
          
          [#t freevars]))
          
  (if (not (fun? e))
      e
      (let* ([fun-name (fun-nameopt e)]
             [fun-param (fun-formal e)]
             [fun-body (fun-body e)]
             [freevars (helper fun-body (set fun-name fun-param) (set))])
        (fun-challenge fun-name fun-param fun-body freevars))))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(int? e) e]

        [(closure? e) e]

        [(aunit? e) e]

        [(var? e) 
         (eval-under-env-c (compute-free-vars (envlookup env (var-string e))) env)]

        [(add? e) 
         (let ([v1 (eval-under-env-c (compute-free-vars (add-e1 e)) env)]
               [v2 (eval-under-env-c (compute-free-vars (add-e2 e)) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        [(fun-challenge? e)
         ; loop over freevars set and call envlookup on all strings in it. Add (str, e) pair to new env
         (let ([fun-env (set-map (fun-challenge-freevars e)
                                 (lambda (s) (cons s (envlookup env s))))]) 
           (closure fun-env e))]

        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (compute-free-vars (ifgreater-e1 e)) env)]
               [v2 (eval-under-env-c (compute-free-vars (ifgreater-e2 e)) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (compute-free-vars (ifgreater-e3 e)) env)
                   (eval-under-env-c (compute-free-vars (ifgreater-e4 e)) env))
               (error "MUPL number comparison applied to non-number")))]

       
        [(call? e)
         (let ([v1 (eval-under-env-c (compute-free-vars (call-funexp e)) env)]
               [v2 (eval-under-env-c (compute-free-vars (call-actual e)) env)])
           (if (closure? v1)
               (let* ([f (closure-fun v1)]
                      [fun-name (fun-challenge-nameopt f)]
                      [fun-param (fun-challenge-formal f)]
                      [fun-body (fun-challenge-body f)]
                      [fun-env (closure-env v1)]
                      [ext-env ; includes function free and bound vars
                      (if fun-name
                          (cons (cons fun-name v1)
                                (cons (cons fun-param v2) fun-env))
                          (cons (cons fun-param v2) fun-env))])
                 (eval-under-env-c (compute-free-vars fun-body) ext-env))
               (error "MUPL function call applied to a non function expression")))]

        [(mlet? e)
         (let* ([v1 (mlet-e e)]
               [v2 (mlet-body e)]
               [ext-env (cons (cons (mlet-var e) v1) env)])
           (eval-under-env-c (compute-free-vars v2) ext-env))]

        [(apair? e)
         (apair (eval-under-env-c (compute-free-vars (apair-e1 e)) env)
               (eval-under-env-c (compute-free-vars (apair-e2 e)) env))]

        [(fst? e)
         (let ([p (eval-under-env-c (compute-free-vars (fst-e e)) env)])
           (if (apair? p)
               (eval-under-env-c (compute-free-vars (apair-e1 p)) env)
               (error "MUPL fst applied to non-pair")))]

        [(snd? e)
         (let ([p (eval-under-env-c (compute-free-vars (snd-e e)) env)])
           (if (apair? p)
               (eval-under-env-c (compute-free-vars (apair-e2 p)) env)
               (error "MUPL snd applied to non-pair")))]

        [(isaunit? e)
         (let ([v (eval-under-env-c (compute-free-vars (isaunit-e e)) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))