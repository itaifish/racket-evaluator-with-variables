#lang racket
(require (only-in (file "lex.rkt") lex))

(provide parse)

(define tokens (make-parameter '()))

(define (parse code)
  (parameterize ([tokens (lex code)])
    (parse-program)))

(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token "remaining tokens are:" (tokens))))
    (tokens (rest (tokens)))  ; update tokens: remove first token
    token))

;; program := exprList
(define (parse-program)
  (list 'program (parse-expr-list)))
;; exprList := expr optExprList
(define (parse-expr-list)
  (list 'exprList (parse-expr) (parse-opt-expr-list)))
;; optExprList := ɛ | exprList
(define (parse-opt-expr-list)
  (let ([type (peek)])
    (if (or (eq? type 'CPAREN) (eq? type 'EMPTY))
      '(optExprList)
      (list 'optExprList (parse-expr-list)))))
  
;; expr := atom | invocation | let | define | lambda
(define (parse-expr)
  (list 'expr
        (case (peek)
          ['OPAREN (parse-invocation)]
          ['LET (parse-let)]
          ['DEFINE (parse-define)]
          ['LAMBDA (parse-lambda)]
          [else (parse-atom)])))

;; let := LET OPAREN NAME expr CPAREN expr
(define (parse-let)
  (list 'let (consume 'LET) (consume 'OPAREN) (consume 'NAME) (parse-expr) (consume 'CPAREN) (parse-expr)))

;; define := DEFINE NAME expr
(define (parse-define)
  (list 'define (consume 'DEFINE) (consume 'NAME) (parse-expr)))

;; lambda := LAMBDA OPAREN NAME CPAREN expr
(define (parse-lambda)
  (list 'lambda (consume 'LAMBDA) (consume 'OPAREN) (consume 'NAME) (consume 'CPAREN) (parse-expr)))
  
;; atom := NAME | STRING | number
(define (parse-atom)
    (list 'atom
          (cond
            [(is-number? (peek)) (parse-number)]
            [(eq? (peek) 'STRING)(consume 'STRING)]
            [else (consume 'NAME)]
            )))
;; number := INT | FLOAT
(define (parse-number)
  (list 'number (cond
    [(eq? (peek) 'FLOAT) (consume 'FLOAT)]
    [else (consume 'INT)]
      )))

;; invocation := OPAREN exprList CPAREN
(define (parse-invocation)
  (list 'invocation (consume 'OPAREN) (parse-expr-list) (consume 'CPAREN)))
  
;; Returns the type of the first token
(define (peek)
  (if (empty? (tokens))
      'EMPTY
      (first (first (tokens)))))
;; is Type a number?
(define (is-number? type)
  (or (eq? type 'INT) (eq? type 'FLOAT)))

(module+ test
  (require (only-in rackunit
                    check-equal?))
  (define str "
(define factorial
  (fun (n)
    (if (< n 0.9)
        1  ;; base case
        (factorial (- n 1) ;* recursive case *; ))))

(print (+ \"5! is \" (factorial 5)))")
  (define res '(program
  (exprList
   (expr
    (invocation
     (OPAREN #f)
     (exprList
      (expr
       (atom
        (NAME define)))
      (optExprList
       (exprList
        (expr
         (atom
          (NAME factorial)))
        (optExprList
         (exprList
          (expr
           (invocation
            (OPAREN #f)
            (exprList
             (expr
              (atom
               (NAME fun)))
             (optExprList
              (exprList
               (expr
                (invocation
                 (OPAREN #f)
                 (exprList
                  (expr
                   (atom
                    (NAME n)))
                  (optExprList))
                 (CPAREN #f)))
               (optExprList
                (exprList
                 (expr
                  (invocation
                   (OPAREN #f)
                   (exprList
                    (expr ; if
                     (atom
                      (NAME if)))
                    (optExprList
                     (exprList
                      (expr ; (< n 0.9)
                       (invocation
                        (OPAREN #f)
                        (exprList
                         (expr (atom (NAME <)))
                         (optExprList
                          (exprList
                           (expr (atom (NAME n)))
                           (optExprList
                            (exprList
                             (expr (atom (number (FLOAT 0.9))))
                             (optExprList))))))
                        (CPAREN #f)))
                      (optExprList
                       (exprList
                        (expr (atom (number (INT 1))))
                        (optExprList
                         (exprList
                          (expr
                           (invocation
                            (OPAREN #f)
                            (exprList
                             (expr (atom (NAME factorial)))
                             (optExprList
                              (exprList
                               (expr
                                (invocation
                                 (OPAREN #f)
                                 (exprList
                                  (expr (atom (NAME -)))
                                  (optExprList
                                   (exprList
                                    (expr (atom (NAME n)))
                                    (optExprList
                                     (exprList
                                      (expr (atom (number (INT 1))))
                                      (optExprList))))))
                                 (CPAREN #f)))
                               (optExprList))))
                            (CPAREN #f)))
                          (optExprList))))))))
                   (CPAREN #f)))
                 (optExprList))))))
            (CPAREN #f)))
          (optExprList))))))
     (CPAREN #f)))
   (optExprList
    (exprList
     (expr
      (invocation
       (OPAREN #f)
       (exprList
        (expr
         (atom (NAME print)))
        (optExprList
         (exprList
          (expr
           (invocation
            (OPAREN #f)
            (exprList
             (expr
              (atom
               (NAME +)))
             (optExprList
              (exprList
               (expr
                (atom
                 (STRING "5! is ")))
               (optExprList
                (exprList
                 (expr
                  (invocation
                   (OPAREN #f)
                   (exprList
                    (expr
                     (atom
                      (NAME factorial)))
                    (optExprList
                     (exprList
                      (expr
                       (atom
                        (number
                         (INT 5))))
                      (optExprList))))
                   (CPAREN #f)))
                 (optExprList))))))
            (CPAREN #f)))
          (optExprList))))
       (CPAREN #f)))
     (optExprList))))))
  (check-equal? (parse str) res))


