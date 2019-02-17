#lang racket

(provide (all-defined-out))

(require "simpleParser.rkt")

; interpreter - top level function called by the user
(define interpreter
  (lambda (filename)
    (program-interpret (parser filename) (list '() '()))))

(define program-interpret
  (lambda (lis state)
    (cond
      [(null? lis) '()]
      ; [(eq? (caar lis) 'var) (declare-interpret (cdar lis) state)]
      ; [(eq? (caar lis) '=) (assign-interpret (cdar lis) state)]
      ; [(eq? (caar lis) 'return) (return-interpret (cdar lis) state)]
      ; [(eq? (caar lis) 'if) (if-interpret (cdar lis) state)]
      ; [(eq? (caar lis) 'while) (while-interpret (cdar lis) state)]
      [else (error 'interpreter "Undefined statement")])))

(define declare-interpret
  (lambda (stmt state)
    (cond
      [(null? stmt) (error 'interpreter "Invalid declare statement")]
      [(null? (cdr stmt)) (state-add (car stmt) 'novalue state)]
      [(null? (cddr stmt)) (state-add (car stmt) (cadr stmt) state)]
      [else (error 'declare-interpret "invalid declare statement")])))

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
    (if (number? name)
        name
        (get name (car state) (cadr state)))))

; get - helper function for M-name
(define get
  (lambda (name vars vals)
    (cond
      [(null? vars) (error 'M-value "variable not found")]
      [(eq? (car vars) name) (car vals)]
      [else (get name (cdr vars) (cdr vals))])))

(define M-value
  (lambda (expr state)
    (cond
      [(null? expr) (error 'M-value "undefined expression")]
      [(number? expr) expr]
      [(eq? (math-operator expr) '+) (+ (M-value (M-name (left-operand expr) state) state) (M-value (M-name (right-operand expr) state) state))]
      [(eq? (math-operator expr) '-) (- (M-value (M-name (left-operand expr) state) state) (M-value (M-name (right-operand expr) state) state))]
      [(eq? (math-operator expr) '*) (* (M-value (M-name (left-operand expr) state) state) (M-value (M-name (right-operand expr) state) state))]
      [(eq? (math-operator expr) '/) (quotient (M-value (M-name (left-operand expr) state) state) (M-value (M-name (right-operand expr) state) state))]
      [(eq? (math-operator expr) '%) (remainder (M-value (M-name (left-operand expr) state) state) (M-value (M-name (right-operand expr) state) state))])))

(define math-operator car)
(define left-operand cadr)
(define right-operand caddr)
