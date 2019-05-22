#lang racket

; Prelude

(define (append ls x)
  (if (null? ls)
      (list x)
      (cons (car ls) (append (cdr ls) x))))

; Stack implementation

(define (make-stack initial)
  (let ([stack (list initial)])

    (define (push x)
      (set! stack (cons x stack)))
    
    (define (pop)
      (if (pair? stack)
          (let ([x (car stack)])
            (set! stack (cdr stack))
            x)
          #f))
    
    (define (dispatch m)
      (cond [(eq? m 'push) push]
            [(eq? m 'pop) pop]
            [(eq? m 'content) stack]))
     dispatch))

(define (push x stack)
  ((stack 'push) x))
(define (pop stack)
  ((stack 'pop) stack))
(define (content-stack stack)
  (stack 'content))

; Queue implementation

(define (make-queue)
  (let ([queue '()])
    
    (define (enqueue x)
      (set! queue (append queue x)))

    (define (dequeue)
      (if (pair? queue)
          (let ([x (car queue)])
            (set! queue (cdr queue))
            x)
          #f))
    
    (define (dispatch m)
      (cond [(eq? m 'enqueue) enqueue]
            [(eq? m 'dequeue) dequeue]
            [(eq? m 'content) queue]))
     dispatch))

(define (enqueue! queue x)
  ((queue 'enqueue) x))
(define (dequeue! queue)
  ((queue 'dequeue)))
(define (content-queue queue)
  (queue 'content))

; Ordered Set implementation

(define (make-set)
  (let ([content '()])

    (define (adjoin x)
      (define (adjoin xs x)
        (if (null? xs)
            (list x)
            (cond
              [(= x (car xs)) xs]
              [(< x (car xs)) (cons x xs)]
              [else (cons (car xs) (adjoin (cdr xs) x))])))
      (set! content (adjoin content x)))

    (define (dispatch m)
      (cond [(eq? m 'adjoin) adjoin]
            [(eq? m 'content) content]))
    dispatch))

(define (adjoin! set x)
  ((set 'adjoin) x))
(define (content-set set)
  (set 'content))

; Map implementation

(define (make-map)
  (let ([content '()])

    (define (lookup key)
      (define (lookup key rest)
        (cond
          [(null? rest) #f]
          [(equal? key (caar rest)) (cdar rest)]
          [else (lookup key (cdr rest))]))
      (lookup key content))
    
    (define (add key value)
      (if (lookup key)
          #f
          (set! content (cons (cons key value) content))))

    (define (dispatch m)
      (cond [(eq? m 'add) add]
            [(eq? m 'lookup) lookup]
            [(eq? m 'content) content]))
    dispatch))

(define (add! m key value)
  ((m 'add) key value))
(define (lookup m key)
  ((m 'lookup) key))
(define (content-map m)
  (m 'content))

; Binary tree

(define (make-btree)
  (let ([content '()])

    (define (insert x)
      (define (insert-in tree)
        (cond
          [(null? tree) (list x 'none 'none)]
          [(< x (car tree))
           (if (eq? (cadr tree) 'none)
               (list (car tree) (list x 'none 'none) (caddr tree))
               (list (car tree) (insert-in (cadr tree)) (caddr tree)))]
          [else
           (if (eq? (caddr tree) 'none)
               (list (car tree) (cadr tree) (list x 'none 'none))
               (list (car tree) (cadr tree) (insert-in (caddr tree))))]))
      (set! content (insert-in content)))

    (define (contain x)
      (define (exists-in tree)
        (cond
          [(null? tree) #f]
          [(= x (car tree)) #t]
          [(< x (car tree))
           (if (eq? (cadr tree) 'none)
               #f
               (exists-in (cadr tree)))]
          [else
           (if (eq? (caddr tree) 'none)
               #f
               (exists-in (caddr tree)))]))
      (exists-in content))
   
    (define (dispatch m)
      (cond [(eq? m 'insert) insert]
            [(eq? m 'contain) contain]
            [(eq? m 'content) content]))
    dispatch))

(define (insert! t value)
  ((t 'insert) value))
(define (contain t value)
  ((t 'contain) value))
(define (content-btree t)
  (t 'content))
