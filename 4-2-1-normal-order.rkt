#lang racket
; https://docs.racket-lang.org/compatibility/mlists.html#%28mod-path._racket%2Fmpair%29
; https://docs.racket-lang.org/reference/mpairs.html
(require compatibility/mlist)

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
         (apply-local
          (actual-value (operator exp) env)
          (operands exp) ; Instead of evaluating the operands, we pass them directly to apply-local
          env)] ; Apply-local needs the environment as the delayed operands will need to be evaluated when needed
        [else
         (error "Unknown expression type: EVAL" exp)]))

(define (apply-local procedure arguments env)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-args-values arguments env))] ; For primitive procedures, we evaluate directly as applicative-order
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; All arguments are delayed
           (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type: APPLY" procedure)]))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-args-values exps env)
  (if (no-operands? exps)
      '()
      (mcons (actual-value
             (first-operand exps)
             env)
            (list-of-args-values
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (mcons (delay-it
             (first-operand exps)
             env)
            (list-of-delayed-args
             (rest-operands exps)
             env))))

; Thunks
; Delayed arguments are not evaluated. Instead they are transformed into Thunks
; Thunks are forced with actual-value only at the output on the driver-loop

(define (delay-it exp env)
  (mlist 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (mcar (mcdr thunk)))
(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))

; Thunk memoization
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (mcar (mcdr evaluated-thunk)))

(define (force-it obj)
  (cond
    [(thunk? obj)
     (let ([result (actual-value (thunk-exp obj) (thunk-env obj))])
       (set-mcar! obj 'evaluated-thunk)
       (set-mcar! (mcdr obj) result)
       (set-mcdr! (mcdr obj) '())
       result)]
    [(evaluated-thunk? obj)
     (thunk-value obj)]
    [else obj]))

; Conditional
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
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
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))

; Quotations are list starting with 'quote
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (mcar (mcdr exp)))

; Assignments start with 'set!
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (mcar (mcdr exp)))

(define (assignment-value exp)
  (mcar (mcdr (mcdr exp))))

; Definitions start with 'define
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (mcar (mcdr exp)))
      (mcar (mcdr exp))
      (mcar (mcar (mcdr exp)))))

(define (definition-value exp)
  (if (symbol? (mcar (mcdr exp)))
      (mcar (mcdr (mcdr exp)))
      (make-lambda
       (mcdr (mcar (mcdr exp)))
       (mcdr (mcdr exp)))))

; Lambdas
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (mcar (mcdr exp)))

(define (lambda-body exp)
  (mcdr (mcdr exp)))

(define (make-lambda parameters body)
  (mcons 'lambda (mcons parameters body)))

; Conditionals
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (mcar (mcdr exp)))

(define (if-consequent exp) (mcar (mcdr (mcdr exp))))

(define (if-alternative exp)
  (if (not (null? (mcdr (mcdr (mcdr exp)))))
      (mcar (mcdr (mcdr (mcdr exp))))
      'false))

(define (make-if predicate consequent alternative)
  (mlist 'if predicate consequent alternative))

; Begin
(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (mcdr exp))

(define (last-exp? seq) (null? (mcdr seq)))

(define (first-exp seq) (mcar seq))

(define (rest-exps seq) (mcdr seq))

(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (make-begin seq) (cons 'begin seq))

; Procedure application
(define (application? exp) (mpair? exp))

(define (operator exp) (mcar exp))

(define (operands exp) (mcdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (mcar ops))

(define (rest-operands ops) (mcdr ops))

; Derived expression
(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp) (mcdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (mcar clause))

(define (cond-actions clause)
  (mcdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ;no else clause
      (let ([first (mcar clauses)]
            [rest (mcdr clauses)])
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
  (mlist 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (mcar (mcdr p)))

(define (procedure-body p) (mcar (mcdr (mcdr p))))

(define (procedure-environment p) (mcar (mcdr (mcdr (mcdr p)))))

; **
; Environment
; **
; Enclosing environment is the environment minus the current frame.
(define (enclosing-environment env) (mcdr env))

(define (first-frame env) (mcar env))

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
  (set-mcar! frame (mcons var (frame-variables frame)))
  (set-mcdr! frame (mcons val (frame-values frame))))

; Extending environment is create a new frame with initial vars/vals on top of a base environment.
(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (mlength vars) (mlength vals))
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
            [(eq? var (mcar vars))(mcar vals)]
            [else (scan (mcdr vars) (mcdr vals))]))
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
            [(eq? var (mcar vars)) (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
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
            [(eq? var (mcar vars)) (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
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
  (mcdr proc))

(define primitive-procedures
  (mlist
   (mcons 'car car)
   (mcons 'cdr cdr)
   (mcons 'cons cons)
   (mcons 'null? null?)
   (mcons '+ +)
   (mcons '- -)
   (mcons '/ /)
   (mcons '* *)
   (mcons '= =)))

(define (primitive-procedure-names)
  (mmap mcar primitive-procedures))

(define (primitive-procedure-objects)
  (mmap
   (lambda (proc)
     (mcons 'primitive (mcdr proc)))
   primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) (if (mpair? args) (mlist->list args) args)))
  
(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

(define (list->mlist/deep input)
  (mmap
   (lambda (value)
     (if (pair? value)
         (list->mlist/deep value)
         value))
   (list->mlist input)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (actual-value (list->mlist/deep input) the-global-environment)])
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
; (define (try a b) (if (= a 0) 1 b))

;;; M-Eval value:
; ok

;;; M-Eval input:
; (try 0 (/ 1 0))

;;; M-Eval value:
; (a b c d e f)