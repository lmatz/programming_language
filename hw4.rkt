#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))
  

(define (list-nth-mod xs n)
  (cond [(< n 0)   (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list") ]
        [ #t (let ([remaind (remainder n (length xs))]) (car (list-tail xs remaind)))]))
  
(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (let ([x (s)])
          (cons (car x) (stream-for-n-steps (cdr x) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x)
             (if (= (remainder x 5) 0)
                 (cons (- 0 x) (lambda() (f (+ x 1))))
                 (cons x (lambda() (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda ()
                (cons "dan.jpg" (lambda () (g))))]
           [g (lambda ()
                (cons "dog.jpg" (lambda () (f))))])
    (lambda () (f))))

(define (stream-add-zero s)
  (let ([p (s)])
    (lambda () (cons (cons 0 (car p))
                     (stream-add-zero (cdr p))))))

(define (cycle-lists-naive xs ys)
  (letrec ([lenx (length xs)]
           [leny (length ys)]
           [p (lambda(count)
             (let ([xs1 (remainder count lenx)]
                   [ys1 (remainder count leny)])
               (cons (cons (car (list-tail xs xs1))
                           (car (list-tail ys ys1)))
                     (lambda () (p (+ count 1))))))])
    (lambda () (p 0))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda() (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(equal? n (vector-length vec)) #f]
                      [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v) (vector-ref vec n) (f (+ n 1)))]
                      [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [index-now 0])
    (lambda (v)
      (letrec ([if-cached (vector-assoc v cache)])
        (if if-cached
            (cdr if-cached)
            (letrec ([new-to-cache (assoc v xs)])
              (begin (set! index-now (if (= index-now n) 0 (+ index-now 1)))
                     (vector-set! cache index-now (cons v new-to-cache))
                     new-to-cache)))))))
