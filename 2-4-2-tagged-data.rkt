#lang racket

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum:
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum:
              CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))


; We define constructors to create complex number. The datum
; constructed is tagged with the constructor used.
; The procedures to retrieve real/imaginary/magnitude/angle
; differs depending on the representation constructed.

; Rectangular form

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag
   'rectangular
   (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag
   'rectangular
   (cons (* r (cos a))
         (* r (sin a)))))

; Polar form

(define (real-part-polar z)
  (* (magnitude-polar z)
     (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z)
     (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag
   'polar
   (cons r a)))

; Conditional Selectors
; Each selector strips off the tag, and forward the
; contents to the underlying layer.
; At each layer, the datum is directed toward the appropriate operation,
; while being reduced to its core values used for computation. 

(define (real-part z)
  (cond [(rectangular? z) (real-part-rectangular (contents z))]
        [(polar? z) (real-part-polar (contents z))]
        [else (error "Unknown type: REAL-PART" z)]))

(define (imag-part z)
  (cond [(rectangular? z) (imag-part-rectangular (contents z))]
        [(polar? z) (imag-part-polar (contents z))]
        [else (error "Unknown type: IMAG-PART" z)]))

(define (magnitude z)
  (cond [(rectangular? z) (magnitude-rectangular (contents z))]
        [(polar? z) (magnitude-polar (contents z))]
        [else (error "Unknown type: MAGNITUDE" z)]))

(define (angle z)
  (cond [(rectangular? z) (angle-rectangular (contents z))]
        [(polar? z) (angle-polar (contents z))]
        [else (error "Unknown type: ANGLE" z)]))

; Make rectangular complex-number
; using rectangular constructor 
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

; Make polar complex-number
; using polar constructor
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; Complex-number arithmetic operations

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
   (* (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))