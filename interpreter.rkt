#lang racket

(provide (all-defined-out))

(require "functionParser.rkt")

;; interpret - top level function called by the user
(define interpret
  (lambda (filename)
    (call/cc (lambda (return)
       (interpret-global-scope (parser filename)
                               (list empty-layer)
                               return
                               (lambda (state) (error 'break "invalid break"))
                               (lambda (state) (error 'continue "invalid continue"))
                               (lambda (state) (error 'throw "invalid throw")))))))

(define empty-layer (list '() '()))

(define interpret-global-scope
  (lambda (lis state return break continue throw)
    (cond
      [(null? lis)                     (funcall 'main (closure-params (M-name 'main state)) state throw)]
      [(not (list? lis))               state]
      [(eq? (stmt-type lis) 'var)      (declare lis state return break continue throw)]
      [(eq? (stmt-type lis) 'function) (function (function-name lis) (function-params lis) (function-body lis) state)]
      [else                            (interpret-global-scope (next-stmts lis)
                                                               (M-state (first-stmt lis) state return break continue throw)
                                                               return break continue throw)])))

(define function-name cadr)
(define function-params caddr)
(define function-body cadddr)

;; function - interprets a function definition
(define function
  (lambda (name params body state)
    (state-add name (make-closure params body state) state)))

;; make-closure - return the closure of a function
(define make-closure
 (lambda (params body state)
   (list params body (get-function-environment state))))

(define closure-params car)
(define closure-body cadr)
(define closure-env caddr)

;; get-function-environment - returns a function that takes creates a function environment by appending
;; the state at the function call onto the function's state in scope
(define get-function-environment
  (lambda (global)
    (lambda (local)
      (append local global))))

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
      [(eq? (stmt-type lis) 'try)      (M-state (finally-block lis)
                                                (M-state (catch-block lis)
                                                         (call/cc (lambda (throw) (M-state (try-block lis) state
                                                                                           return break continue throw)))
                                                         return break continue throw)
                                                return break continue throw)]
      [(eq? (stmt-type lis) 'catch)    (catch lis state return break continue throw)]
      [(eq? (stmt-type lis) 'finally)  (finally lis state return break continue throw)]
      [(eq? (stmt-type lis) 'throw)    (throw (append (state-add 'error (return-value lis) (list empty-layer)) state))]
      [(eq? (stmt-type lis) 'function) (function (function-name lis) (function-params lis) (function-body lis) state)]
      [(eq? (stmt-type lis) 'funcall)  (funcall (funcall-name lis) (funcall-params lis) state throw)]
      [else                            (M-state (next-stmts lis)
                                                (M-state (first-stmt lis) state
                                                         return break continue throw)
                                                return break continue throw)])))

(define stmt-type car)
(define first-stmt car)
(define next-stmts cdr)
(define return-value cadr)
(define try-block cadr)
(define catch-block caddr)
(define finally-block cadddr)
(define funcall-name cadr)
(define funcall-params cddr)

;; funcall - interprets a functional call statement
(define funcall
  (lambda (name params state throw)
    (M-state (closure-body (M-name name state))
             (bind-params (closure-params (M-name name state))
                          params
                          ((closure-env (M-name name state)) (add-layer (filter-params params state))))
             (lambda (value) state)
             (lambda (state) (error 'break "invalid break"))
             (lambda (state) (error 'continue "invalid continue"))
             throw)))

;; funcallv - interprets a functional call statement
(define funcallv
  (lambda (name params state throw)
    (call/cc (lambda (return)
       
    (M-state (closure-body (M-name name state))
             (bind-params (closure-params (M-name name state))
                          params
                          ((closure-env (M-name name state)) (add-layer (filter-params params state))))
             return
             (lambda (state) (error 'break "invalid break"))
             (lambda (state) (error 'continue "invalid continue"))
             throw)))))

;; bind-params - returns the given state with the formal parameters bound to the actual parameters
;; in the topmost layer, has an accumulator-style structure
(define bind-params
  (lambda (formal actual state)
    (cond
      [(null? formal)
       state]
     ; [(eq? (car actual) ref-operator)
     ;  (bind-params (cdr formal) (cddr actual) (state-add (car formal) (cadr actual) state))]    ; this is pass-by-reference, comment out if it does not work
      [else
       (bind-params (cdr formal) (cdr actual) (state-add (car formal) (M-value (car actual) state) state))])))        ; pass-by-value

;; filter-params - returns a state containing only the specified parameters
(define filter-params
  (lambda (params state)
    (list (cons params (list (get-var-values params state))))))

;; get-var-values - returns the corresponding values given a list of variables
(define get-var-values
  (lambda (vars state)
    (if (null? vars)
        '()
        (cons (get-box-value (car vars) state) (get-var-values (cdr vars) state)))))

;; get-box-value - returns the box pointed to by the given name in the given state
(define get-box-value
  (lambda (name state)
    (cond
      [(null? state)                              (error 'M-name "variable not found, using before declaring")]
      [(number? name)                             name]
      [(eq? name 'true)                           #t]
      [(eq? name 'false)                          #f]
      [(var-in-scope? name (var-list state))      (get-value name (var-list state) (val-list state))]
      [else                                       (get-box-value name (next-layer state))])))

(define ref-operator '&)

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
      [(var-in-scope? (var-name stmt) (var-list state))
       (error 'declare "Redefining variable error, variable previously declared")]
      [(null? (cddr stmt))
       (state-add (var-name stmt) 'novalue state)]
      [else
        (state-add (var-name stmt)
                   (M-value (var-value stmt) state)
                   (M-state (var-value stmt) state return break continue throw))])))

;; assign - interprets a varible assignment statement, returns a value and state
(define assign
  (lambda (stmt state original-state return break continue throw)
    (cond
      [(null? stmt)
       (error 'assign-interpret "invalid assign statement")]
      [(null? state)
       (error 'assign-error "variable not found, using before declaring")]
      [(var-in-scope? (var-name stmt) (var-list state))
       (begin (set-box! (get-value (var-name stmt) (var-list state) (val-list state))
                        (M-value (var-value stmt) state))
              (M-state (var-value stmt) state return break continue throw))]
      [else
        (cons (car state)
              (assign stmt (next-layer state) original-state return break continue throw))])))

(define var-name cadr)
(define var-value caddr)

;; var-in-scope? - returns true if the given variable has been declared in the current scope,
;; otherwise false
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
        [(null? (cdddr stmt))
         (M-state (condition stmt) state
                  return break continue throw)]
        [else
          (M-state (else-statement stmt)
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
(define else-statement cadddr)

;; catch - interprets a catch clause
(define catch
  (lambda (stmt state return break continue throw)
    (cond
      [(null? state)
       state]
      [(null? (var-list state))
       state]
      [(eq? (car (var-list state)) 'error)
       (M-state (catch-body stmt) (cons (list (catch-exception stmt) (val-list state)) (next-layer state))
                return break continue throw)]
      [else
        state])))

(define catch-exception cadr)
(define catch-body caddr)

;; finally - interprets a finally clause
(define finally
  (lambda (stmt state return break continue throw)
    (if (null? stmt)
        state
        (M-state (finally-body stmt) state return break continue throw))))

(define finally-body cadr)

;; state-add - add the specified variable and its value to the top-most state layer in the program state
(define state-add
  (lambda (name value state)
    (if (null? (next-layer state))
        (list (list (append (var-list state) (list name)) (append (val-list state) (list (box value)))))
        (cons (list (append (var-list state) (list name)) (append (val-list state) (list (box value))))
              (next-layer state)))))

(define val-list cadar)
(define var-list caar)
(define top-layer car)
(define next-layer cdr)

;; state-remove - removes the specified variable and its value from the top-most layer in which it
;; appears in the program state
(define state-remove
  (lambda (name state)
    (cond
      [(null? state)
       (error 'state-remove "variable not found, using before declaring")]
      [(var-in-scope? name (var-list state))
       (cons (remove name (var-list state) (val-list state) empty-layer) (next-layer state))]
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
      [(var-in-scope? name (var-list state))      (unbox (get-value name (var-list state) (val-list state)))]
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
      [(eq? (stmt-type expr) 'funcall)  (funcallv (funcall-name expr) (funcall-params expr) state (lambda (throw) (error 'placeholder "Placeholder Error")))]
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
