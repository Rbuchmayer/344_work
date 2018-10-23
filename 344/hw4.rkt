
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;returns a list of numbers counting from low to high by sequence
(define (sequence spacing low high)
  (cond [(> low high) null]
        [#t (cons low (sequence spacing (+ low spacing) high))]))

;returns the same list xs, but each element has suffix appended to the end
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;returns the element of xs with its position equal to the remainder of
;dividing n by the length of xs. Gives errors if n is negative or xs is null
(define (list-nth-mod xs n)
  (cond [(< n 0) error "list-nth-mod: negative number"]
        [(null? xs) error "list-nth-mod: empty list"]
        [#t (let ([i (remainder n (length xs))])
              (car(list-tail xs i)))]))

;returns a list of elements of stream s for k steps
(define (stream-for-k-steps s k)
  (cond [(= k 0) null]
        [#t (cons (car (s)) (stream-for-k-steps (cdr (s)) (- k 1)))]))

;helper function for funny-number-stream
(define (f x)    
  (let ([y (if (= 0 (remainder x 6)) (- 0 x) x)]) y
    (cons y (lambda () (f (+ x 1))))))
;a stream of the natural number, but all multiples of 6 are negated
(define funny-number-stream (lambda () (f 1)))

;helper function for dan-then-dog
(define (g flag)    
  (if flag (cons "dan.jpg" (lambda () (g (not flag))))
      (cons "dog.jpg" (lambda () (g (not flag))))))
;a stream that alternates between "dan" and "dog"
(define dan-then-dog (lambda () (g #t)))

;returns an identical stream except all elements are pairs with the car
;value as 1, and cdr value as the original value
(define (stream-add-one s)
  (letrec ([f (lambda () (cons (cons 1 (car (s))) (stream-add-one (cdr (s)))))])
    f))

;returns a stream where the elemets are pairs. The car of the paris cycles
;through xs, and the cdr of the pair cycles through ys
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))]) (lambda () (f 0))))

;behaves the same as Racket's assoc function, except handles vectors and
;skips non pairs
(define (vector-assoc v vec)
  (letrec ([f (lambda (p)
                (cond [(>= p (vector-length vec)) #f]
                      [(pair? (vector-ref vec p))
                       (cond 
                         [(equal? (car (vector-ref vec p)) v)  (vector-ref vec p)]
                         [#t (f (+ p 1))])]
                      [#t (f (+ p 1))]))])          
    (f 0)))

;behaves the same as Racket's assoc function, except utilizes a cache of
;size n for faster computing
(define (caching-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [pos 0]
         [add (lambda (v) (let ([ans (assoc v xs)])
                            (if ans (begin (vector-set! cache pos ans) (print "in cache ") (print cache)
                                           (if (= pos (- (vector-length cache) 1)) (set! pos 0)
                                               (set! pos (+ pos 1))) ans) #f )))]) 
    (lambda (v) (let ([ans2 (vector-assoc v cache)])
                  (if ans2 ans2 (add v))))))

;a macro that evaluates e2 until it is not greater than e1
(define-syntax while-greater
  (syntax-rules (while-greater do)
    [(while-greater e1 do e2)
     (letrec ([x e1]
              [f (lambda (y) (if (> y x) (f e2) #t ))]) (f e2))]))