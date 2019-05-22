#lang racket

; **
; Eval and Apply
; **

(define (eval exp env)
  (cond [(self-evaluating? exp)
         exp]
        [(variable? exp)
         (lookup-variable-value exp env)]
        [(quoted? exp)
         (text-of-quotation exp)]
        [(assignment? exp)
         (eval-assignment exp env)]
        [(definition? exp)
         (eval-definition exp env)]
        [(if? exp)
         (eval-if exp env)]
        [(lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env)]
        [(begin? exp)
         (eval-sequence
          (begin-actions exp)
          env)]
        [(cond? exp)
         (eval (cond->if exp) env)]
        [(application? exp)
         (apply-local (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env))]
        [else
         (error "Unknown expression type: EVAL" exp)]))

(define (apply-local procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type: APPLY" procedure)]))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; Conditional
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; Sequences
; Evaluation of compound sequence returns the last expression result
; from the list of expressions.
(define (eval-sequence exps env)
  (cond [(last-exp? exps)
         (eval (first-exp exps) env)]
        [else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env)]))

; Assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; **
; Expressions
; **

; Numbers and strings self evaluate
(define (self-evaluating? exp)
  (cond [(number? exp) true]
        [(string? exp) true]
        [else false]))

(define (variable? exp)
  (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; Quotations are list starting with 'quote
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

; Assignments start with 'set!
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

; Definitions start with 'define
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)
       (cddr exp))))

; Lambdas
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Conditionals
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Begin
(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (make-begin seq) (cons 'begin seq))

; Procedure application
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; Derived expression
(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ;no else clause
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; Predicates
(define (true? x)
  (not (eq? x false)))

; Procedures are composed of the parameters, the body, and the environment where they were defined.
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

; **
; Environment
; **
; Enclosing environment is the environment minus the current frame.
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment 'the-empty-environment)

; Frames in the environment are composed of a tuple of list of variables and list of values.
(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame) (mcar frame))

(define (frame-values frame) (mcdr frame))

; Adding a new binding to the frame is done by
; adding a new variable in the frame-variables
; and a new value in the frame-values.
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (frame-variables frame)))
  (set-mcdr! frame (cons val (frame-values frame))))

; Extending environment is create a new frame with initial vars/vals on top of a base environment.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))

; Lookup is executed by looking for a variable by recursively looking at each frame of the environment.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars) (env-loop (enclosing-environment env))]
            [(eq? var (car vars))(car vals)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; Setting a variable is done by recursively looking at each frame of the environment
; and setting the variable if found.
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars) (env-loop (enclosing-environment env))]
            [(eq? var (car vars)) (set-mcar! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; Define will only search within the first frame of
; the environment (current frame where the application is executed)
; for an existing variable or creating a new one if it does not exists.
(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars) (add-binding-to-frame! var val frame)]
            [(eq? var (car vars)) (set-mcar! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))

; **
; Run
; **

(define (setup-environment)
  (let ([initial-env (extend-environment
                      (primitive-procedure-names)
                      (primitive-procedure-objects)
                      the-empty-environment)])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (eval input the-global-environment)])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

;; Run the evaluator:
(driver-loop)

;;; M-Eval input:
; (define (append x y)
;  (if (null? x)
;       y
;      (cons (car x) (append (cdr x) y))))

;;; M-Eval value:
; ok

;;; M-Eval input:
; (append '(a b c) '(d e f))

;;; M-Eval value:
; (a b c d e f)