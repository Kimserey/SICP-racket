#lang racket

(require "2-4-3-data-directed-programming.rkt")

; Scheme number package

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  
  (put 'add '(scheme-number scheme-number)
       (λ (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (λ (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (λ (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (λ (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (λ (x) (tag x)))
  'install-scheme-number-package-done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; Rational package

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x))))
  (define (tag x) (attach-tag 'rational x))
  
  (put 'add '(rational rational)
       (λ (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (λ (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (λ (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (λ (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (λ (n d) (tag (make-rat n d))))
  
  'install-rational-package-done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; Complex package

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  (define (tag z) (attach-tag 'complex z))
  
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))

  (put 'add '(complex complex)
       (λ (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (λ (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (λ (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (λ (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (λ (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (λ (r a) (tag (make-from-mag-ang r a))))

  'install-complex-package-done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; Install all packages

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; Arithmetic Operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; Examples

(add (make-scheme-number 1) (make-scheme-number 1))
(add (make-rational 1 3) (make-rational 1 2))
(add (make-complex-from-real-imag 1 3) (make-complex-from-mag-ang 1 2))