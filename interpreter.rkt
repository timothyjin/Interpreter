#lang racket

(provide (all-defined-out))

(require "simpleParser.rkt")

;; interpret - top level function called by the user
(define interpret
  (lambda (filename)
    (call/cc (lambda (return)
       (M-state (parser filename)
                (list empty-layer)
                return
                (lambda (state) (error 'break "invalid break"))
                (lambda (state) (error 'continue "invalid continue"))
                (lambda (state) (error 'throw "invalid throw")))))))

(define empty-layer (list '() '()))

;; M-state - given a statement and a state, returns the state resulting from applying the statement
;; to the given state
(define M-state
  (lambda (lis state return break continue throw)
    (cond
      [(null? lis)                     state]
      [(not (list? lis))               state]
      [(eq? (stmt-type lis) 'var)      (declare lis state return break continue throw)]
      [(eq? (stmt-type lis) '=)        (assign lis state state return break continue throw)]
      [(eq? (stmt-type lis) 'if)       (if-else lis state return break continue throw)]
      [(eq? (stmt-type lis) 'while)    (call/cc (lambda (break) (while lis state return break continue throw)))]
      [(eq? (stmt-type lis) 'return)   (return (M-value (return-value lis) state))]
      [(eq? (stmt-type lis) 'begin)    (remove-top-layer (M-state (next-stmts lis) (add-layer state)
                                                                  return break continue throw))]
      [(eq? (stmt-type lis) 'break)    (break (remove-top-layer state))]
      [(eq? (stmt-type lis) 'continue) (continue (remove-top-layer state))]
      [(eq? (stmt-type lis) 'try)      (M-state (finally-body lis)
                                                (M-state (catch-body lis)
                                                         (call/cc (lambda (throw) (M-state (try-body lis) state
                                                                                           return break continue throw)))
                                                         return break continue throw)
                                                return break continue throw)]
      [(eq? (stmt-type lis) 'catch)    (catch lis state return break continue throw)]
      [(eq? (stmt-type lis) 'finally)  (finally lis state return break continue throw)]
      [(eq? (stmt-type lis) 'throw)    (throw (cons (list (list 'error) (list (return-value lis))) state))]
      [else                            (M-state (next-stmts lis)
                                                (M-state (first-stmt lis) state
                                                         return break continue throw)
                                                return break continue throw)])))

(define stmt-type car)
(define first-stmt car)
(define next-stmts cdr)
(define return-value cadr)

(define finally-body cadddr)
(define try-body cadr)
(define catch-body caddr)

;; catch - interprets a catch clause
(define catch
  (lambda (stmt state return break continue throw)
    (cond
      [(null? state) state]
      [(null? (get-vars-list state)) state]
      [(eq? (car (get-vars-list state)) 'error)        (M-state (caddr stmt) (cons (list (cadr stmt) (cadar state)) (cdr state)) return break continue throw)]
      [else state])))

;; finally - interprets a finally clause
(define finally
  (lambda (stmt state return break continue throw)
    (if (null? stmt)
        state
        (M-state (cadr stmt) state return break continue throw))))

;; add-layer - adds an empty state layer on top of the current state
(define add-layer
  (lambda (state)
    (cons empty-layer state)))

;; remove-top-layer - removes the top-most state layer from the current state
(define remove-top-layer
  (lambda (state)
    (next-layer state)))

;; declare - interprets a variable declaration/initialization statement, adding the declared variable to
;; the top-most state layer
(define declare
  (lambda (stmt state return break continue throw)
    (cond
      [(null? stmt)
       (error 'declare "invalid declare statement")]
      [(var-in-scope? (var-name stmt) (get-vars-list state))
       (error 'declare "Redefining variable error, variable previously declared")]
      [(null? (cddr stmt))
       (state-add (var-name stmt) 'novalue state)]
      [else
        (state-add (var-name stmt)
                   (M-value (var-value stmt) state)
                   (M-state (var-value stmt) state return break continue throw))])))

;; is-declared? - returns true if the given variable name has been declared in the current scope,
;; otherwise false
(define is-declared?
  (lambda (name state)
    (cond
      [(null? state)                    #f]
      [(eq? name (get-vars-list state)) #t]
      [else                             (is-declared? name (next-layer state))])))

;; assign - interprets a varible assignment statement, returns a value and state
(define assign
  (lambda (stmt state original-state return break continue throw)
    (cond
      [(null? stmt)
       (error 'assign-interpret "invalid assign statement")]
      [(null? state)
       (error 'assign-error "variable not found, using before declaring")]
      [(var-in-scope? (var-name stmt) (get-vars-list state))
       (state-add (var-name stmt)
                  (M-value (var-value stmt) original-state)
                  (state-remove (var-name stmt)
                                (M-state (var-value stmt) state return break continue throw)))]
      [else
        (cons (car state)
              (assign stmt (next-layer state) original-state return break continue throw))])))

(define var-name cadr)
(define var-value caddr)

;; var-in-scope? - return true if the given list contains the given variable, otherwise false
(define var-in-scope?
  (lambda (var lis)
    (cond
      [(null? lis)         #f]
      [(eq? (car lis) var) #t]
      [else                (var-in-scope? var (cdr lis))])))

;; if-else - interprets an if-else statement
(define if-else
  (lambda (stmt state return break continue throw)
      (cond
        [(M-value (condition stmt) state)
         (M-state (statement stmt)
                  (M-state (condition stmt) state return break continue throw)
                  return break continue throw)]
        [(null? (else-statement stmt))
         (M-state (condition stmt) state
                  return break continue throw)]
        [else
          (M-state (car (else-statement stmt))
                   (M-state (condition stmt) state return break continue throw)
                   return break continue throw)])))

;; while - interprets a while statement
(define while
  (lambda (stmt state return break continue throw)
       (if (M-value (condition stmt) state)
           (while stmt (call/cc (lambda (continue)
                                  (M-state (statement stmt)
                                           (M-state (condition stmt) state return break continue throw)
                                           return break continue throw))) return break continue throw)
           (M-state (condition stmt) state
                    return break continue throw))))

(define condition cadr)
(define statement caddr)
(define else-statement cdddr)

;; state-add - add the specified variable and its value to the top-most state layer in the program state
(define state-add
  (lambda (name value state)
    (if (null? (next-layer state))
        (list (list (append (get-vars-list state) (list name)) (append (get-val-list state) (list value))))
        (cons (list (append (get-vars-list state) (list name)) (append (get-val-list state) (list value)))
              (next-layer state)))))

(define get-val-list cadar)
(define get-vars-list caar)
(define top-layer car)
(define next-layer cdr)

;; state-remove - removes the specified variable and its value from the top-most layer in which it appears in the program state
(define state-remove
  (lambda (name state)
    (cond
      [(null? state)
       (error 'state-remove "variable not found, using before declaring")]
      [(var-in-scope? name (get-vars-list state))
       (cons (remove name (get-vars-list state) (get-val-list state) empty-layer) (next-layer state))]
      [else
        (cons (top-layer state) (state-remove name (next-layer state)))])))

;; remove - helper function for state-remove using an accumulator, returns a state layer with the
;; specified variable removed
(define remove
  (lambda (name vars vals acc)
    (cond
      [(null? vars)
       acc]
      [(eq? (car vars) name)
       (list (append (car acc) (cdr vars)) (append (cadr acc) (cdr vals)))]
      [else
        (remove name (cdr vars) (cdr vals)
                (list (append (car acc) (list (car vars))) (append (cadr acc) (list (car vals)))))])))

;; M-name - returns the value of the specified variable/value
(define M-name
  (lambda (name state)
    (cond
      [(null? state)                              (error 'M-name "variable not found, using before declaring")]
      [(number? name)                             name]
      [(eq? name 'true)                           #t]
      [(eq? name 'false)                          #f]
      [(var-in-scope? name (get-vars-list state)) (get-value name (get-vars-list state) (get-val-list state))]
      [else                                       (M-name name (next-layer state))])))

;; get-value - helper function for M-name, returns the value bound to the given variable name
(define get-value
  (lambda (name vars vals)
    (cond
      [(and (eq? (car vars) name) (eq? (car vals) 'novalue)) (error 'M-name "Variable not assigned with value")]
      [(and (eq? (car vars) name) (eq? #t (car vals)))       'true]
      [(and (eq? (car vars) name) (eq? #f (car vals)))       'false]
      [(eq? (car vars) name)                                 (car vals)]
      [else                                                  (get-value name (cdr vars) (cdr vals))])))

;; M-value - returns the value of a arithmetic/boolean expression
(define M-value
  (lambda (expr state)
    (cond
      [(null? expr) (error 'M-value "undefined expression")]
      [(not (list? expr)) (M-name expr state)]
      [(eq? (math-operator expr) '=) (M-value (var-value expr) state)]
      [(eq? (math-operator expr) '+) (+ (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(and (eq? (math-operator expr) '-) (is-right-operand-null? expr)) (* -1 (M-value (left-operand expr) state))]
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
      [(eq? (bool-operator expr) '&&) (and (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (bool-operator expr) '||) (or (M-value (left-operand expr) state) (M-value (right-operand expr) state))]
      [(eq? (bool-operator expr) '!) (not (M-value (left-operand expr) state))])))

(define math-operator car)
(define comp-operator car)
(define bool-operator car)
(define left-operand cadr)
(define right-operand caddr)

;; is-right-operand-null? - returns true if the given expression uses a unary operator, otherwise false
(define is-right-operand-null?
  (lambda (expr)
    (null? (cddr expr))))
