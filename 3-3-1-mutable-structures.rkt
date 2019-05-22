#lang racket

; Mutable cons

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond [(eq? m 'car) x]
          [(eq? m 'cdr) y]
          [(eq? m 'set-car!) set-x!]
          [(eq? m 'set-cdr!) set-y!]
          [else (error "Undefined operation: CONS" m)]))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (caar z) ((z 'car) 'car))

(define (set-car! z v)
  ((z 'set-car!) v)
  z)
  
(define (set-cdr! z v)
  ((z 'set-cdr!) v)
  z)

; Queue with pointers
; Constant complexity

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      #f
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ([new-pair (cons item '())])
    (cond
      [(empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue]
      [else
       (set-cdr! (rear-ptr queue) new-pair)
       (set-rear-ptr! queue new-pair)
       queue])))

(define (delete-queue! queue)
  (cond
    [(empty-queue? queue) #f]
    [else
     (let ([front (front-ptr queue)])
       (set-front-ptr! queue (cdr front))
       (car front))]))

(define (print-queue queue)
  (define (print items)
    (if (null? items)
        'end
        (begin
          (displayln (car items))
          (print (cdr items))
          'end)))
  (print (front-ptr queue)))

; Table with assignment

(define (assoc key records)
  (cond
    [(null? records) #f]
    [(equal? key (caar records)) (car records)]
    [else (assoc key (cdr records))]))

(define (lookup table key)
  (let ([record (assoc key (cdr table))])
    (if record
        (cdr record)
        #f)))

(define (insert! table key value)
  (let ([record (assoc key (cdr table))])
    (if record
        (set-cdr! record value)
        (set-cdr!
         table
         (cons (cons key value)
               (cdr table))))))

(define (make-table)
  (cons '*table* '()))

(define t (make-table))