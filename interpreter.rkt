#lang racket

(provide (all-defined-out))

(require "simpleParser.rkt")

; interpret - top level function called by the user
(define interpret
  (lambda (filename)
    (call/cc (lambda (return)
       (M-state (parser filename) empty-state return (lambda (state) (error 'break error "invalid break")) (lambda (state) (error 'continue error "invalid continue")))))))

(define empty-state (list '() '()))

; M-state - changes the program state with a single statement in a program list
(define M-state
  (lambda (lis state return break continue)
    (cond
      [(null? lis) state]
      [(not (list? lis)) state]
      [(eq? (stmt-type lis) 'var) (declare lis state return break continue)]
      [(eq? (stmt-type lis) '=) (assign lis state state return break continue)]
      [(eq? (stmt-type lis) 'if) (if-else lis state return break continue)]
      [(eq? (stmt-type lis) 'while) (call/cc (lambda (break) (while-interpret lis state return break continue)))]
      [(eq? (stmt-type lis) 'return) (return (M-value (return-value lis) state))]
      [(eq? (stmt-type lis) 'begin) (remove-top-layer (M-state (cdr lis) (add-layer state) return break continue))]
      [(eq? (stmt-type lis) 'break)    (break state)]
      [(eq? (stmt-type lis) 'continue) (continue state)]
      [else (M-state (next-stmts lis) (M-state (first-stmt lis) state return break continue) return break continue)])))

(define stmt-type car)
(define first-stmt car)
(define next-stmts cdr)
(define return-value cadr)

;;helper function to deal with adding and deleting layer
(define add-layer
  (lambda (state)
    (cons '() (cons '() state))))

(define remove-top-layer
  (lambda (state)
    (cddr state)))
; declare - interprets a variable declaration/initialization statement
; declare adds the variable only to the first layer
(define declare
  (lambda (stmt state return break continue)
    (cond
      [(null? stmt) (error 'declare-interpret "invalid declare statement")]
      [(is-declared? (var-name stmt) (get-vars-list state)) (error 'declare-intrepret "Redefining variable error, variable previously declared")]
      [(null? (cddr stmt)) (append (state-add (var-name stmt) 'novalue state) (next-layer state))]
      [else (append (state-add (var-name stmt)
                       (M-value (var-value stmt) state)
                       (M-state (var-value stmt) state return break continue)) (next-layer state))])))


;;Helper function to check if a variable is previously declared. Address issue of redefining variable
(define is-declared?
  (lambda (var-name-c vars-list)
    (cond
      [(null? vars-list) #f]
      [(eq? var-name-c (car vars-list)) #t]
      [else (is-declared? var-name-c (cdr vars-list))])))


;; assign - uses version of assign which returns a value and state
;; Has to first find which layer the variable is in, so it recursively goes through layers and look for it
(define assign
  (lambda (stmt state original-state return break continue)
    (cond
      [(null? stmt) (error 'assign-interpret "invalid assign statement")]
      [(null? state) (error 'assign-error "variable not found, using before declaring")]
      [(contain-var? (var-name stmt) (get-vars-list state))
      (append (state-add (var-name stmt)
                 (M-value (var-value stmt) original-state)
                 (state-remove (var-name stmt) (M-state (var-value stmt) state return break continue))) (next-layer state))]
      [else (cons (get-vars-list state) (cons (get-val-list state) (assign stmt (next-layer state) original-state return break continue)))])))

(define var-name cadr)
(define var-value caddr)

;;helper function to see if a list contains the variable
(define contain-var?
  (lambda (var lis)
    (cond
      [(null? lis) #f]
      [(eq? (car lis) var) #t]
      [else (contain-var? var (cdr lis))])))
; if-else - interprets an if-else statement
(define if-else
  (lambda (stmt state return break continue)
      (cond
        [(M-value (condition stmt) state) (M-state (statement stmt) (M-state (condition stmt) state return break continue) return break continue)]
        [(null? (else-statement stmt)) (M-state (condition stmt) state return break continue)]
        [else (M-state (car (else-statement stmt)) (M-state (condition stmt) state return break continue) return break continue)])))

;;while-interpret: interprets a while statement
(define while-interpret
  (lambda (stmt state return break continue)
       (if (M-value (condition stmt) state)
          (while-interpret stmt (call/cc (lambda (continue) (M-state (statement stmt) (M-state (condition stmt) state return break continue) return break continue))) return break continue)
          (M-state (condition stmt) state return break continue))))


(define condition cadr)
(define statement caddr)
(define else-statement cdddr)

; state-add - add the specified variable and its value to the program state
(define state-add
  (lambda (name value state)
    (list (append (get-vars-list state) (list name)) (append (get-val-list state) (list value)))))

(define get-val-list cadr)
(define get-vars-list car)
(define next-layer cddr)
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
; Has to recursively look for variable through state layers
(define M-name
  (lambda (name state)
    (cond
      [(null? state) (error 'M-name "variable not found, using before declaring")]
      [(number? name) name]
      [(eq? name 'true) #t]
      [(eq? name 'false) #f]
      [(contain-var? name (get-vars-list state)) (get name (get-vars-list state) (get-val-list state))]
      [else (M-name name (next-layer state))])))

; get - helper function for M-name
(define get
  (lambda (name vars vals)
    (cond
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
      [(eq? (math-operator expr) '+) (+ (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(and (eq? (math-operator expr) '-) (is-right-operand-null? expr)) (* -1 (M-value (left-operand expr) state))]  ;Address negative sign
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
(define is-right-operand-null?
  (lambda (expr)
    (null? (cddr expr))))
