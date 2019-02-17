#lang racket

(provide (all-defined-out))

(require "simpleParser.rkt")

; interpreter - top level function called by the user
(define interpreter
  (lambda (filename)
    (program-interpret (parser filename) (list '() '()))))

; program-interpret - interprets a list of program statements
(define program-interpret
  (lambda (lis state)
    (if (null? lis)
        state
        (let ([newstate (statement-interpret (car lis) state)])
          (program-interpret (cdr lis) newstate)))))

; statement-interpret - interprets a single statement in a program list
(define statement-interpret
  (lambda (lis state)
    (cond
      [(null? lis) (error 'statement-interpret "undefined statement")]
      [(eq? (car lis) 'var) (declare lis state)]
      [(eq? (car lis) '=) (assign lis state)]
      ; [(eq? (car lis) 'return) (return-interpret lis state)]
      [(eq? (car lis) 'if) (if-interpret lis state)]
      ; [(eq? (car lis) 'while) (while-interpret lis state)]
      [else (error 'interpreter "undefined statement")])))

; declare - interprets a variable declaration/initialization statement
(define declare
  (lambda (stmt state)
    (cond
      [(null? stmt) (error 'declare-interpret "invalid declare statement")]
      [(null? (cdr (var-name stmt))) (state-add (car (var-name stmt)) 'novalue state)]
      [(null? (cdr (var-value stmt))) (state-add (car (var-name stmt)) (car (var-value stmt)) state)])))

; assign - interprets a variable assignment statement
(define assign
  (lambda (stmt state)
    (if (null? stmt)
      (error 'assign-interpret "invalid assign statement")
      (let ([state-temp (state-remove (car (var-name stmt)) state)])
        (state-add (car (var-name stmt)) (M-value (car (var-value stmt)) state) state-temp)))))

(define var-name cdr)
(define var-value cddr)

; if-interpret - interprets a single if-statement
(define if-interpret
  (lambda (stmt state)
    (if (null? stmt)
        (error 'if-interpret "invalid if statement")
        (let ([bool (M-value (condition stmt) state)])
          (cond
            [bool (statement-interpret (statement stmt) state)]
            [(not bool) state]
            [else (error 'if-interpret "invalid if statement")])))))

(define condition cadr)
(define statement caddr)

; state-add - add the specified variable and its value to the program state
(define state-add
  (lambda (name value state)
    (list (append (car state) (list name)) (append (cadr state) (list value)))))

; state-remove - remove the specified variable and its value from the program state
(define state-remove
  (lambda (name state)
    (remove name (car state) (cadr state) (list '() '()))))

; remove - helper function for state-remove using an accumulator
(define remove
  (lambda (name vars vals acc)
    (cond
      [(null? vars) (error 'state-remove "variable not found")]
      [(eq? (car vars) name) (list (append (car acc) (cdr vars)) (append (cadr acc) (cdr vals)))]
      [else (remove name (cdr vars) (cdr vals) (list (append (car acc) (list (car vars))) (append (cadr acc) (list (car vals)))))])))

; M-name - retrieve the value of the specified variable/value
(define M-name
  (lambda (name state)
    (if (or (number? name) (boolean? name))
        name
        (get name (car state) (cadr state)))))

; get - helper function for M-name
(define get
  (lambda (name vars vals)
    (cond
      [(null? vars) (error 'M-name "variable not found")]
      [(eq? (car vars) name) (car vals)]
      [else (get name (cdr vars) (cdr vals))])))

; M-value - returns the value of a arithmetic expression
(define M-value
  (lambda (expr state)
    (cond
      [(null? expr) (error 'M-value "undefined expression")]
      [(not (list? expr)) (M-name expr state)]
      [(eq? (math-operator expr) '+) (+ (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (math-operator expr) '-) (- (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (math-operator expr) '*) (* (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (math-operator expr) '/) (quotient (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (math-operator expr) '%) (remainder (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (comp-operator expr) '==) (eq? (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (comp-operator expr) '!=) (not (eq? (M-value (left-operand expr) state) (M-value (right-operand expr) state)))]
      [(eq? (comp-operator expr) '<) (< (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (comp-operator expr) '>) (> (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (comp-operator expr) '<=) (<= (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (comp-operator expr) '>=) (>= (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (comp-operator expr) '&&) (and (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (comp-operator expr) '||) (or (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (comp-operator expr) '!) (not (M-value (left-operand expr) state))])))

(define math-operator car)
(define comp-operator car)
(define left-operand cadr)
(define right-operand caddr)
