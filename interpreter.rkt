#lang racket

(provide (all-defined-out))

(require "simpleParser.rkt")

; interpret - top level function called by the user
(define interpret
  (lambda (filename)
    (program-interpret (parser filename) empty-state)))

(define empty-state (list '() '()))

; program-interpret - interprets a list of program statements
(define program-interpret
  (lambda (lis state)
    (cond
      [(number? state) state]
      [(not (eq? (M-name 'return state) 'novalue)) (M-name 'return state)]
      [(null? lis) (error 'program-interpret "No return statement")]
      [else (program-interpret (next-stmts lis) (M-state (first-stmt lis) state))])))

(define first-stmt car)
(define next-stmts cdr)

; M-state - changes the program state with a single statement in a program list
(define M-state
  (lambda (lis state)
    (cond
      [(null? lis) state]
      [(not (list? lis)) state]
      [(eq? (stmt-type lis) 'var) (declare lis state)]
      [(eq? (stmt-type lis) '=) (assign lis state)]
      [(eq? (stmt-type lis) 'if) (if-else lis state)]
      [(eq? (stmt-type lis) 'while) (while-interpret lis state)]
      [(eq? (stmt-type lis) 'return) (return lis state)]
      [else (M-state (next-stmts lis) (M-state (stmt-type lis) state))])))

(define stmt-type car)

; declare - interprets a variable declaration/initialization statement
(define declare
  (lambda (stmt state)
    (cond
      [(null? stmt) (error 'declare-interpret "invalid declare statement")]
      [(is-declared? (var-name stmt) (get-vars-list state)) (error 'declare-intrepret "Redefining variable error, variable previously declared")]
      [(null? (cddr stmt)) (state-add (var-name stmt) 'novalue state)]
      [else (state-add (var-name stmt)
                       (M-value (var-value stmt) state)
                       (M-state (var-value stmt) state))])))


;;Helper function to check if a variable is previously declared. Address issue of redefining variable
(define is-declared?
  (lambda (var-name-c vars-list)
    (cond
      [(null? vars-list) #f]
      [(eq? var-name-c (car vars-list)) #t]
      [else (is-declared? var-name-c (cdr vars-list))])))


; assign - uses version of assign which returns a value and state
(define assign
  (lambda (stmt state)
    (if (null? stmt)
      (error 'assign-interpret "invalid assign statement")
      (state-add (var-name stmt)
                 (M-value (var-value stmt) state)
                 (state-remove (var-name stmt) (M-state (var-value stmt) state))))))

(define var-name cadr)
(define var-value caddr)

; return - interprets a return statement
(define return
  (lambda (stmt state)
    (state-add 'return (M-value (return-value stmt) state) state)))

(define return-value cadr)

; if-else - interprets an if-else statement
(define if-else
  (lambda (stmt state)
      (cond
        [(M-value (condition stmt) state) (M-state (statement stmt) (M-state (condition stmt) state))]
        [(null? (else-statement stmt)) (M-state (condition stmt) state)]
        [else (M-state (car (else-statement stmt)) (M-state (condition stmt) state))])))

;;while-interpret: interprets a while statement
(define while-interpret
  (lambda (stmt state)
    (if (M-value (condition stmt) state)
          (while-interpret stmt (M-state (statement stmt) (M-state (condition stmt) state)))
          (M-state (condition stmt) state))))


(define condition cadr)
(define statement caddr)
(define else-statement cdddr)
;;;;; These functions need more abstraction ;;;;;

; state-add - add the specified variable and its value to the program state
(define state-add
  (lambda (name value state)
    (list (append (get-vars-list state) (list name)) (append (get-val-list state) (list value)))))

(define get-val-list cadr)
(define get-vars-list car)

; state-remove - remove the specified variable and its value from the program state
(define state-remove
  (lambda (name state)
    (remove name (get-vars-list state) (get-val-list state) empty-state)))

; remove - helper function for state-remove using an accumulator
(define remove
  (lambda (name vars vals acc)
    (cond
      [(null? vars) (error 'state-remove "variable not found, using before declaring")]
      [(eq? (car vars) name) (list (append (get-vars-list acc) (cdr vars)) (append (get-val-list acc) (cdr vals)))]
      [else (remove name (cdr vars) (cdr vals) (list (append (get-vars-list acc) (list (car vars))) (append (get-val-list acc) (list (car vals)))))])))

; M-name - retrieve the value of the specified variable/value
(define M-name
  (lambda (name state)
    (cond
      [(number? name) name]
      [(eq? name 'true) #t]
      [(eq? name 'false) #f]
      [else(get name (get-vars-list state) (get-val-list state))])))

; get - helper function for M-name
(define get
  (lambda (name vars vals)
    (cond
      [(and (null? vars) (eq? name 'return)) 'novalue]
      [(null? vars) (error 'M-name "variable not found, using before declaring")]
      [(and (eq? (car vars) name) (eq? (car vals) 'novalue)) (error 'M-name "Variable not assigned with value")]
      [(and (eq? (car vars) name) (eq? #t (car vals)) 'true)]
      [(and (eq? (car vars) name) (eq? #f (car vals)) 'false)]
      [(eq? (car vars) name) (car vals)]
      [else (get name (cdr vars) (cdr vals))])))

; M-value - returns the value of a arithmetic expression
(define M-value
  (lambda (expr state)
    (cond
      [(null? expr) (error 'M-value "undefined expression")]
      [(not (list? expr)) (M-name expr state)]
      [(eq? (math-operator expr) '=) (M-value (var-value expr) state)]
      [(eq? (math-operator expr) '+) (+ (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(and (eq? (math-operator expr) '-) (is-right-operand-null? expr)) (* -1 (M-value (left-operand expr) (M-state (left-operand expr) state)))]  ;Address negative sign
      [(eq? (math-operator expr) '-) (- (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (math-operator expr) '*) (* (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (math-operator expr) '/) (quotient (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (math-operator expr) '%) (remainder (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (comp-operator expr) '==) (eq? (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (comp-operator expr) '!=) (not (eq? (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state))))]
      [(eq? (comp-operator expr) '<) (< (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (comp-operator expr) '>) (> (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (comp-operator expr) '<=) (<= (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (comp-operator expr) '>=) (>= (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (comp-operator expr) '&&) (and (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (comp-operator expr) '||) (or (M-value (left-operand expr) state) (M-value (right-operand expr) (M-state (left-operand expr) state)))]
      [(eq? (comp-operator expr) '!) (not (M-value (left-operand expr) (M-state (left-operand expr) state)))])))

(define math-operator car)
(define comp-operator car)
(define left-operand cadr)
(define right-operand caddr)
(define is-right-operand-null?
  (lambda (expr)
    (null? (cddr expr))))
