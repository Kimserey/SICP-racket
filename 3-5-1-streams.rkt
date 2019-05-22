#lang racket

; Iterative calculation

(define (divides? a b) (= (remainder b a) 0)) 

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond [(> (* test-divisor test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor n (+ test-divisor 1))]))
  (find-divisor n 2))

; (define (prime? n)
;  (= n (smallest-divisor n)))

; (define (sum-primes a b)
;  (define (iter count acc)
;    (cond [(> count b) acc]
;          [(prime? count) (iter (+ count 1) (+ count acc))]
;          [else (iter (+ count 1) acc)]))
;  (iter a 0))

; Sequence operations calculation
; Even though composable, problematic due to the whole sequence
; needing to be computed prior the next operation start.

(define (filter predicate sequence)
  (cond [(null? sequence) '()]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (sum-primes-two a b)
  (accumulate + 0 (filter prime? (enumerate-interval a b))))

; Streams

(define the-empty-stream 'empty-stream)

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (define (force x) (x))
  (force (cdr stream)))

(define (memo-proc proc)
  (let ([already-run? false]
        [result false])
    (λ ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))

(define (stream-null? . streams)
  (if (null? streams)
      #t
      (and (eq? (car streams) the-empty-stream)
           (apply stream-null? (cdr streams)))))

; Generator will delay the evaluation of cdr
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons low (memo-proc (λ () (stream-enumerate-interval (+ 1 low) high))))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . args)
  (if (apply stream-null? args)
      the-empty-stream
      (cons
       (apply proc (map stream-car args))
       (λ () (apply stream-map proc (map stream-cdr args))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each displayln s))

(define (stream-filter pred stream)
  (cond [(stream-null? stream) the-empty-stream]
        [(pred (stream-car stream))
         (cons (stream-car stream)
               (memo-proc (λ () (stream-filter pred (stream-cdr stream)))))]
        [else (stream-filter pred (stream-cdr stream))]))

; Infinite stream

(define (integers-starting-from n)
  (cons n (λ () (integers-starting-from (+ n 1)))))

; (define integers (integers-starting-from 1))

; (define no-sevens
;  (stream-filter (λ (x) (not (divides? 7 x))) integers))

(define (fibgen a b)
  (cons a (λ () (fibgen b (+ a b)))))

; (define fibs (fibgen 0 1))

; Sieve of Eratosthenes
; Recursively curates the stream by removing divisibles
; of each primes.
; At each curation, the prime is cons'ed with the rest,
; resulting stream not longer contains divisable value of the prime returned.
;
; Stream is infinite, and the process is also infinite due to the sieve containing the sieve.
(define (sieve stream)
  (cons
   (stream-car stream)
   (λ ()
     (sieve
      (stream-filter
       (λ (x) (not (divides? (stream-car stream) x)))
       (stream-cdr stream))))))

; (define primes (sieve (integers-starting-from 2)))

(define (add-streams . args)
  (apply stream-map + args))

(define ones (cons 1 (λ () ones)))

(define integers
  (cons 1 (λ () (add-streams ones integers))))

(define fibs
  (cons 0 (λ () (cons 1 (λ () (add-streams (stream-cdr fibs) fibs))))))

(define (scale-stream stream factor)
  (stream-map (λ (x) (* x factor)) stream))

(define (partial-sums s)
  (let ([value (stream-car s)])
    (cons value (λ () (stream-map (λ (x) (+ x value)) (partial-sums (stream-cdr s)))))))

; Recursive definition of primes and prime?
; n is not a prime if there is a prime generated such as 'sqrt(n) < P < n'
(define primes
  (cons 2 (λ () (stream-filter prime? (integers-starting-from 3)))))

(define (prime? n)
  (define (iter ps)
    (cond [(> (* (stream-car ps) (stream-car ps)) n) true]
          [(divides? (stream-car ps) n) false]
          [else (iter (stream-cdr ps))]))
  (iter primes))

; Stream Paradigm

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (define guesses
    (cons 1.0 (λ () (stream-map (λ (guess) (sqrt-improve guess x)) guesses))))
  guesses)

; Stream-map with minus is used to alternate between positive and negative
(define (pi-summands n)
  (cons
   (/ 1.0 n)
   (λ () (stream-map - (pi-summands (+ n 2))))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

; Acceleration
(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
        [s2 (stream-ref s 2)])
    (cons (- s2 (/ (* (- s2 s1) (- s2 s1)) (+ s0 (* -2 s1) s2)))
          (λ () (euler-transform (stream-cdr s))))))

; Recursively transform the transform
(define (make-tableau transform s)
  (cons s (λ () (make-tableau transform (transform s)))))

; Select the first element of each sequence
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

; Infinite stream of pairs
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons (stream-car s1) (λ () (interleave s2 (stream-cdr s1))))))

(define (pairs s t)
  (cons
   (list (stream-car s) (stream-car t))
   (λ () (interleave
          (stream-map
           (λ (x) (list (stream-car s) x))
           (stream-cdr t))
          (pairs (stream-cdr s) (stream-cdr t))))))

(define int-pairs (pairs integers integers))
; Filter pairs which sum is prime
; (stream-ref (stream-filter (λ (pair) (prime? (+ (car pair) (cadr pair)))) int-pairs) 5) 