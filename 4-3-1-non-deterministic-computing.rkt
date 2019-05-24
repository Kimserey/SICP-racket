#lang racket
(require compatibility/mlist)

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond [(self-evaluating? exp)
         (analyze-self-evaluating exp)]
        [(variable? exp)
         (analyze-variable exp)]
        [(quoted? exp)
         (analyze-quoted exp)]
        [(assignment? exp)
         (analyze-assignment exp)]
        [(definition? exp)
         (analyze-definition exp)]
        [(if? exp)
         (analyze-if exp)]
        [(lambda? exp)
         (analyze-lambda exp)]
        [(begin? exp)
         (analyze-sequence
          (begin-actions exp))]
        [(cond? exp)
         (analyze (cond->if exp))]
        [(let? exp)
         (analyze-let exp)]
        [(amb? exp)
         (analyze-amb exp)]
        [(application? exp)
         (analyze-application exp)]
        [else
         (error "Unknown expression type: ANALYZE" exp)]))

(define (analyze-self-evaluating exp) (lambda (env succeed fail) (succeed exp fail)))

(define (analyze-quoted exp)
  (let ([qval (text-of-quotation exp)])
    (lambda (env succeed fail) (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ([old-value (lookup-variable-value var env)])
                 (set-variable-value! var val env)
                 (succeed 'ok (lambda () (set-variable-value! var old-value env) (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
   (lambda (env succeed fail)
    (vproc env
           (lambda (val fail2)
             (define-variable! var val env)
             (succeed 'ok fail2))
           fail))))

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
  (lambda (env succeed fail)
    (pproc env
           (lambda (pred-value fail2)
             (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
           fail))))

(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [body (lambda-body exp)])
    (lambda (env succeed fail)
      (succeed (make-procedure vars body env)
               fail))))

(define (analyze-let exp)
  (let ([vars (let-variables exp)]
        [aproc (mmap analyze (let-values exp))]
        [body (let-body exp)])
    (lambda (env succeed fail)
      (get-args
       aproc
       env
       (lambda (args fail2)
         (execute-application
          (make-procedure vars body env)
          args
          succeed
          fail2))
       fail))))

(define (analyze-sequence exp)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (mcar rest-procs))
              (mcdr rest-procs))))
  (let ([procs (mmap analyze exp)])
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (loop (mcar procs) (mcdr procs)))))

(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (mmap analyze (operands exp))])
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3) (execute-application proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args args env succeed fail)
  (if (null? args)
      (succeed '() fail)
      ((mcar args)
       env
       (lambda (arg fail2)
         (get-args
          (mcdr args)
          env
          (lambda (args fail3)
            (succeed (mcons arg args)
                     fail3))
          fail2))
       fail)))

(define (analyze-amb exp)
  (let ([cprocs (mmap analyze (amb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((mcar choices)
             env
             succeed
             (lambda ()
               (try-next (mcdr choices))))))
      (try-next cprocs))))

(define (execute-application proc args succeed fail)
  (cond [(primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail)]
        [(compound-procedure? proc)
         ((analyze-sequence (procedure-body proc))
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail)]
        [else (error "Unknown procedure type: EXECUTE-APPLICATION" proc)]))

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

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (mcar (mcdr exp)))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (mcar (mcdr exp)))

(define (assignment-value exp)
  (mcar (mcdr (mcdr exp))))

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

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (mcar (mcdr exp)))

(define (lambda-body exp)
  (mcdr (mcdr exp)))

(define (make-lambda parameters body)
  (mcons 'lambda (mcons parameters body)))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-variables exp)
  (mmap mcar (mcar (mcdr exp))))

(define (let-values exp)
  (mmap (lambda (val) (mcar (mcdr val))) (mcar (mcdr exp))))

(define (let-body exp)
  (mcdr (mcdr exp)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (mcar (mcdr exp)))

(define (if-consequent exp) (mcar (mcdr (mcdr exp))))

(define (if-alternative exp)
  (if (not (null? (mcdr (mcdr (mcdr exp)))))
      (mcar (mcdr (mcdr (mcdr exp))))
      'false))

(define (make-if predicate consequent alternative)
  (mlist 'if predicate consequent alternative))

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

(define (make-begin seq) (mcons 'begin seq))

(define (application? exp) (mpair? exp))

(define (operator exp) (mcar exp))

(define (operands exp) (mcdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (mcar ops))

(define (rest-operands ops) (mcdr ops))

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

(define (true? x)
  (not (eq? x false)))

(define (make-procedure parameters body env)
  (mlist 'procedure parameters body env))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (mcdr exp))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (mcar (mcdr p)))

(define (procedure-body p) (mcar (mcdr (mcdr p))))

(define (procedure-environment p) (mcar (mcdr (mcdr (mcdr p)))))

(define (enclosing-environment env) (mcdr env))

(define (first-frame env) (mcar env))

(define the-empty-environment 'the-empty-environment)

(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame) (mcar frame))

(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (frame-variables frame)))
  (set-mcdr! frame (mcons val (frame-values frame))))

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

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars) (add-binding-to-frame! var val frame)]
            [(eq? var (mcar vars)) (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))

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

(define (distinct? xs)
  (define (loop xs res)
    (cond [(null? xs) (and res true)]
          [(not (memq (car xs) (cdr xs))) (loop (cdr xs) true)]
          [else false]))
  (loop xs true))

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
   (mcons 'list list)
   (mcons 'cons cons)
   (mcons '> >)
   (mcons '< <)
   (mcons 'distinct? distinct?)
   (mcons 'abs abs)
   (mcons 'not not)
   (mcons '= =)
   (mcons 'memq memq)))

(define (primitive-procedure-names)
  (mmap mcar primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc)
          (mcons 'primitive (mcdr proc)))
        primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) (mlist->list/deep args)))

(define the-global-environment (setup-environment))

(define input-prompt ";;; Amb-Eval input:")

(define output-prompt ";;; Amb-Eval value:")

(define (mlist->list/deep input)
  (map
   (lambda (value)
     (if (mpair? value)
         (mlist->list/deep value)
         value))
   (mlist->list input)))

(define (list->mlist/deep input)
  (mmap
   (lambda (value)
     (if (pair? value)
         (list->mlist/deep value)
         value))
   (list->mlist input)))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ([input (read)])
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display
             ";;; Starting new problem")
            (ambeval
             (if (pair? input) (list->mlist/deep input) input)
             the-global-environment
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             (lambda ()
               (announce-output ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display
      ";;; There is no current problem")
     (driver-loop))))

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

(driver-loop)

;(define (pairs)
;  (define (require p)
;    (if (not p) (amb)))
;  (let ([a (amb 1 2 3)]
;        [b (amb 1 2 3)])
;    (require (= (+ a b) 4))
;    (list a b)))
;
;(pairs)