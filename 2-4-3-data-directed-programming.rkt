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


; Operations table

(define operations '())

(define (put op type-tags proc)
  (set! operations (cons (list op type-tags proc) operations)))

(define (get op type-tags)
  (define (find operations-table)
    (let ([operation (car operations-table)]
          [rest (cdr operations-table)])
      (cond
        [(and (equal? (car operation) op)
              (equal? (cadr operation) type-tags))
         (caddr operation)]
        [(null? rest) (error "Operation not found: GET" op type-tags)]
        [else (find rest)])))
  (find operations))

; Rectangular package
; The procedures are installed with '() list to cater for procedures accepting
; multiple arguments.
; Therefore we treat the current procedures as '(rectangle),
; a list of a single argument of type rectangular.
; The constructors need only a single 'rectangular as
; a constructor constructs a single value of a single type.

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  (define (tag z)
    (attach-tag 'rectangular z))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  'install-rectangular-package-done)

; Polar package
; Trigonometry is used to find real and imaginary parts.

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag x y)
     (cons (sqrt (+ (square x) (square y))) (atan y x)))

  (define (tag z)
    (attach-tag 'polar z))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  'install-polar-package-done)

; Apply generic procedure
; '. args' is used to specify the rest of
; the arguments as a list.

(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)])
    (let ([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (error "No method for these types:
                  APPLY-GENERIC"
                 (list op type-tags))))))

; Generic Selectors

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

; Constructors

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; Export procedures

(provide install-rectangular-package
         install-polar-package
         attach-tag
         put
         get
         operations
         real-part
         imag-part
         magnitude
         angle
         apply-generic)