#lang racket

(define espilon 0.00001)

(define (almost-equal? v1 v2) (< (abs (- v1 v2)) espilon))

(define (average x y) (/ (+ x y) 2))

(define (fixed-point f initial-guess)
  (define (apply x) 
    (let ([ result (f x)])
      (if (almost-equal? x result)
        result
        (apply result))))
  (apply initial-guess))

(define (fixed-point-2 f initial-guess)
  (define (apply x) 
    (let ([result (f x)])
      (if (almost-equal? x result)
        result
        (apply (average x result)))))
  (apply initial-guess))

(define (square x) (* x x))

(define (sqrt x)
  (fixed-point
   (λ (y) (average y (/ x y)))
   10.0))

(define dx 0.000001) 

(define (derivative f)
  (λ (x)
    (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-method g y)
  (- y (/ (g y) ((derivative g) y))))


(define (fixed-point-3 f transform initial-guess)
  (define (apply x) 
    (let ([result (f x)])
      (if (almost-equal? x result)
        result
        (apply (transform x result)))))
  (apply initial-guess))

(define (sqrt-4 x)
  (fixed-point-3
   (λ (y) (newton-method (λ (y) (- x (square y))) y))
   (λ (x y) y)
   1.0))



(define (deriv g)
  (λ (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (average-damp f)
  (λ (x)
    (average x (f x))))

(define (newton-transform g)
  (λ (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (nt-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))


(define (sqrt-3 x)
  (fixed-point-of-transform
   (λ (y) (/ x y))
   average-damp
   1.0))
   
(define (sqrt-nm x)
  (fixed-point-of-transform
   (λ (y) (- x (square y)))
   newton-transform
   1.0))
  
(define (sqrt-2 x)
  (newton-method
   (λ (y) (- x (square y))) 1.0))


(require racket/trace)
(trace average)

(trace fixed-point)
(fixed-point cos 1.0)
