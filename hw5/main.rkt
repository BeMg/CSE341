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

(define (racketlist->mupllist rl)
  (cond [(null? rl) (aunit)]
        [#t (apair (car rl) (racketlist->mupllist (cdr rl)))]))
(define (mupllist->racketlist ml)
  (cond [(aunit? ml) null]
        [#t (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                              [v2 (eval-under-env (ifgreater-e2 e) env)]
                              [v3 (eval-under-env (ifgreater-e3 e) env)]
                              [v4 (eval-under-env (ifgreater-e4 e) env)])
                          (if (> (int-num v1) (int-num v2))
                              v3
                              v4))]
        [(fun? e) (closure env e)]
        [(call? e) (let* ([nouse (begin (println "In call") (println e) (println env))]
                          [clo (cond [(closure? (call-funexp e)) (call-funexp e)]
                                     [#t (eval-under-env (call-funexp e) env)])]
                          [func (closure-fun clo)]
                          [funcname (fun-nameopt func)]
                          [funcarg (fun-formal func)]
                          [funcbody (fun-body func)]
                          [env2 (append (list (cons funcarg (eval-under-env (call-actual e) env))
                                              (closure-env clo)
                                              env))]
                          [env3 (cond [funcname (append (list (cons funcname clo)) env2)]
                                      [#t env2])]
                          [nouse2 (begin (println "before eval") (println funcbody) (println env3))])
                     (eval-under-env funcbody env3))]
                                               
        [(mlet? e) (let ([env2 (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env)])
                     (eval-under-env (mlet-body e) env2))]
        [(apair? e) (let ([nouse (begin (println "In apair") (println e) (println env))]
                          [v1 (eval-under-env (apair-e1 e) env)]
                          [v2 (eval-under-env (apair-e2 e) env)])
                      (apair v1 v2))]
        [(fst? e) (let ([nouse (begin (println "In fst") (println e))]
                        [v (eval-under-env (apair-e1 (fst-e e)) env)])
                    v)]
        [(snd? e) (let ([v (eval-under-env (apair-e2 (snd-e e)) env)])
                    v)]
        [(aunit? e) e]
        [(isaunit? e) (if (aunit? (isaunit-e e))
                          (int 1)
                          (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (if (aunit? e1)
                               e2
                               e3))

(define (mlet* lstlst e2) (cond [(null? lstlst) e2]
                                [#t (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))]))

(define (ifeq e1 e2 e3 e4) (mlet* (list (cons "e1" e1) (cons "e2" e2))
                                  (ifgreater (var "e1") (var "e2")
                                             e4
                                             (ifgreater (var "e1") (var "e2")
                                                        e4
                                                        e3))))

;; Problem 4

(define mupl-map
  (fun #f "a1"
       (fun "f1" "lst"
            (ifgreater (isaunit (var "lst")) (int 0)
                       (aunit)
                       (apair (call (var "a1") (fst (var "lst")))
                              (call (var "f1") (snd (var "lst"))))))))
 
(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
