#lang racket

(require (only-in (file "a3.rkt") parse))

(define new-environment hash)
(define add-binding hash-set)
(define lookup-name hash-ref)

(define global-env (make-hash))

(define (eval source-code-str)
  (eval-program (parse source-code-str) (new-environment)))

(define (eval-program program-expr-list env)
  (let ([res (eval-expr-list (second program-expr-list) env)])
    (last res)))

(define (eval-expr-list expr-list env)
  (let* ([expr (second expr-list)]
         [optExprList (third expr-list)])
    (cons
        (eval-expr expr env)
        (eval-opt-expr-list optExprList env))))

(define (eval-expr expr env)
    (let* ([expr-data (second expr)]
           [name (first expr-data)])
      (case name
        ['atom (eval-atom expr-data env)]
        ['invocation (eval-invocation expr-data env)]
        ['let (eval-let expr-data env)]
        ['define (eval-define expr-data env)]
        ['lambda (eval-lambda expr-data env)]
        [else (error (~a "Unexpected name: " name))])))

(define (eval-atom atom env)
  (let* ([atom-data (second atom)]
           [name (first atom-data)])
        (cond [(eq? name 'STRING) (eval-string atom-data)]
              [(eq? name 'number) (eval-number atom-data)]
              [else (eval-name atom-data env)])))

;; let => 'let LET OPAREN NAME expr CPAREN expr
(define (eval-let let-expr env)
  (let* ([name (fourth let-expr)]
         [inner-expr (fifth let-expr)]
         [outer-expr (seventh let-expr)])
    (eval-expr outer-expr (add-binding env (second name) (eval-expr inner-expr env)))))

;; define => 'define DEFINE NAME expr
(define (eval-define define-expr env)
    (let* ([name (third define-expr)]
         [expr-to-eval (fourth define-expr)])
    (hash-set! global-env (second name) (eval-expr expr-to-eval env)))
  (lookup-name global-env (second (third define-expr))))

;; lambda => lambda LAMBDA OPAREN NAME CPAREN expr
(define (eval-lambda lambda-expr env)
  (let* ([name (fourth lambda-expr)]
         [expr-to-eval (sixth lambda-expr)])
    (create-lambda-form 'LAMBDA (list (second name)) expr-to-eval env)))

;; Creates A lambda form as a list containing
;; Lambda's name, list of arguments, body expression, and the local enviornment at the time it is created
(define (create-lambda-form lambda-name lambda-args lambda-body-expr env)
  (list lambda-name lambda-args lambda-body-expr env))

(define (eval-lambda-form lambda-form args)
  (let* ([lambda-name (first lambda-form)]
         [lambda-args (second lambda-form)]
         [lambda-body-expr (third lambda-form)]
         [env (fourth lambda-form)]
         [updated-env (add-all-to-env env lambda-args args)])
   (eval-expr lambda-body-expr updated-env)))

(define (add-all-to-env env argsSymbolList argsValList)
  (if (empty? argsSymbolList)
      env
      (add-all-to-env (add-binding env (first argsSymbolList) (first argsValList)) (rest argsSymbolList) (rest argsValList))))


(define (eval-string string)
  (second string))

(define (eval-number number)
  (second (second number)))

(define and-special
  (λ ands
    (cond
      [(eq? (length ands) 0) #t]
      [(eq? (length ands) 1) (first ands)]
      [else (and (first ands) (apply and-special (rest ands)))])))

(define or-special
  (λ ors
    (cond
      [(eq? (length ors) 0) #f]
      [(eq? (length ors) 1) (first ors)]
      [else (or (first ors) (apply or-special (rest ors)))])))

;;Names hash
(define names (hash
                   '+ +
                   '- -
                   '* *
                   '/ /
                   'string-append string-append
                   'string<? string<?
                   'string=? string=?
                   'not not
                   '= =
                   '< <
                   'and and-special
                   'or or-special))

(define (eval-name name env)
  (let ([nameKey (second name)])
    (cond
    [(hash-has-key? env nameKey) (lookup-name env nameKey)]
    [(hash-has-key? global-env nameKey) (lookup-name global-env nameKey)]
    [else (hash-ref names nameKey)])))

(define (eval-invocation invocation env)
  (let* ([exprRes (eval-expr-list (third invocation) env)]
         [rator (first exprRes)]
         [rand (rest exprRes)])
    (cond [(or (number? rator) (string? rator)) rator]
          [(and (list? rator) (eq? (first rator) 'LAMBDA)) (eval-lambda-form rator rand)]
          [else (apply rator rand)])))

(define (eval-opt-expr-list optExprList env)
  (if (= (length optExprList) 1)
      '()
      (eval-expr-list (second optExprList) env)))