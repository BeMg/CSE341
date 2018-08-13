
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)
(define b (cons 1 (cons 2 (cons 3 (cons 4 null)))))
(define c (list (cons 1 2) (cons 2 3) (cons 4 5)))


;; put your code below

(define (sequence low high stride)
  (cond [(> low high) '()]
        [(= low high) (cons low null)]
        [(< low high) (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (let ([index (remainder n (length xs))])
  (cond [(= (length xs) 0) (error "list-nth-mod: empty list")]
        [(< n 0) (error "list-nth-mod: negative number")]
        [(= index 0) (car xs)]
        [#t (car (list-tail xs index))])))

(define (stream-for-n-steps s n)
  (let ([tmp_rst (s)])
    (cond [(= n 0) '()]
          [#t (cons (car tmp_rst) (stream-for-n-steps (cdr(s)) (- n 1)))])))

(define (funny-number-stream)
  (letrec ([f (lambda (x) (cond [(= 0 (remainder x 5)) (cons (- 0 x) (lambda () (f (+ x 1))))]
                             [#t (cons x (lambda () (f (+ x 1))))]))])
        (f 1)))

(define (dan-then-dog)
  (letrec ([for_dan (lambda () (cons "dan.jpg" (lambda () (for_dog))))]
           [for_dog (lambda () (cons "dog.jpg" (lambda () (for_dan))))])
    (for_dan)))

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))


(define (cycle-lists xs ys)
  (letrec ([foo (lambda (n) (cons (list-nth-mod xs n) (list-nth-mod ys n)))]
          [f (lambda (n) (cons (foo n) (lambda () (f (+ n 1)))))])
    (f 0)))


(define (vector-assoc v vec)
  (letrec ([foo (lambda (v vec n) (cond [(= n (vector-length vec)) #f]
                                        [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
                                        [#t (foo v vec (+ n 1))]))])
    (foo v vec 0)))


(define (cached-assoc xs n)
  (let* ([cache-table (make-vector n (cons #f #f))]
        [cache-index 0]
        [my-assoc (lambda (v)
                    (let ([tmp-cache (vector-assoc v cache-table)])
                          (if tmp-cache
                              tmp-cache
                              (let ([tmp-assoc (assoc v xs)])
                                (if tmp-assoc
                                (begin (vector-set! cache-table (remainder cache-index n) tmp-assoc) tmp-assoc)
                                #f)))))])
    my-assoc))

