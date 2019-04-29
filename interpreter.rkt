#lang racket

(provide (all-defined-out))

(require "classParser.rkt")

(define empty-layer (list '() '()))

;; interpret - top level function called by the user, call main on a specific class
(define interpret
  (lambda (filename class-name)
    (funcall-value-static 'main
                   '()
                   (get-function-list (string->symbol class-name)
                                      (instantiate-class-state (parser filename)
                                                               (list empty-layer)
                                                               (lambda (state) (error 'throw "invalid throw")))) ;Need change
                   (instantiate-class-state (parser filename)
                                            (list empty-layer)
                                            (lambda (state) (error 'throw "invalid throw")))
                   'null
                   (lambda (state) (error 'throw "invalid throw")))))

(define get-function-list
  (lambda (class-name state)
    (function-list (M-name class-name state))))

(define get-field-list
  (lambda (class-name state)
    (field-list (M-name class-name state))))

;; class closure order: parent class/instance field names/methods
(define empty-class-closure (list (list empty-layer) (list empty-layer)))
(define parent-class car)
(define field-list cadr)
(define function-list caddr)

;; instantiate a list of class definition(global state)
(define instantiate-class-state
  (lambda (lis state throw)
    (cond
      [(null? lis) state]
      [(eq? (stmt-type lis) 'class) (add-class-closure (cdr lis) state throw)]
      [else (instantiate-class-state (next-stmts lis) (instantiate-class-state (first-stmt lis) state throw) throw)])))

;; binds a class to a class closure, add them to the list of class definition(state). ex: lis = [A (extends B) body]
(define add-class-closure
  (lambda (lis state throw)
    (if (pair? (extend-stmt lis))
        (state-add (class-name lis)
                   (combine-class-closure (M-name (extend-class-name lis) state)
                                          (find-class-closure (class-name lis)
                                                              (class-body lis)
                                                              (cons (extend-class-name lis) empty-class-closure)
                                                              throw))
                   state)
        (state-add (class-name lis)
                   (find-class-closure (class-name lis)
                                       (class-body lis)
                                       (cons '() empty-class-closure)
                                       throw)
                   state))))

(define combine-class-closure
  (lambda (parent-closure class-closure)
    (list (parent-class class-closure)
          (list (list (append (field-names (field-list class-closure)) (field-names (field-list parent-closure)))
                      (append (field-values (field-list class-closure)) (field-values (field-list parent-closure)))))
          (list (list (append (function-names (function-list class-closure)) (function-names (function-list parent-closure)))
                      (append (function-closures (function-list class-closure)) (function-closures (function-list parent-closure))))))))

(define field-names caar)
(define field-values cadar)
(define function-names caar)
(define function-closures cadar)

(define class-body caddr)
(define extend-stmt cadr)
(define extend-class-name cadadr)
(define class-name car)

;; interpret the class-body and finds the class closure: add instance fields to field list and add functions to function list
(define find-class-closure
  (lambda (class lis class-closure throw)
    (cond
      [(null? lis)
       class-closure]
      [(eq? (stmt-type lis) 'var)
       (list (parent-class class-closure)
             (declare lis (field-list class-closure) 'null throw)
             (function-list class-closure))]
      [(eq? (stmt-type lis) 'static-function)
       (list (parent-class class-closure)
             (field-list class-closure)
             (function class (function-name lis) (function-params lis) (function-body lis) (function-list class-closure)))]
      [(eq? (stmt-type lis) 'function)
       (list (parent-class class-closure)
             (field-list class-closure)
             (function class (function-name lis) (cons 'this (function-params lis)) (function-body lis) (function-list class-closure)))]
      [else
        (find-class-closure class (next-stmts lis) (find-class-closure class (first-stmt lis) class-closure throw) throw)])))

;; function - interprets a function definition
(define function
  (lambda (class name params body state)
    (state-add name (make-closure params body class) state)))

;; make-closure - return the closure of a function
(define make-closure
 (lambda (params body class)
   (list params body get-function-environment class)))

;; get-function-environment - returns a function that takes creates a function environment by appending
;; the state at the function call onto the function's state in scope
(define get-function-environment (lambda (state) state))

;; M-state - given a statement and a state, returns the state resulting from applying the statement
;; to the given state
(define M-state
  (lambda (lis state current-type return break continue throw)
    (cond
      [(null? lis)                     state]
      [(not (list? lis))               state]
      [(eq? (stmt-type lis) 'var)      (declare lis state current-type throw)]
      [(eq? (stmt-type lis) '=)        (assign lis state state  current-type return break continue throw)]
      [(eq? (stmt-type lis) 'if)       (if-else lis state current-type return break continue throw)]
      [(eq? (stmt-type lis) 'while)    (call/cc (lambda (break) (while lis state current-type return break continue throw)))]
      [(eq? (stmt-type lis) 'return)   (return (M-value (return-value lis) state current-type throw))]
      [(eq? (stmt-type lis) 'begin)    (remove-top-layer (M-state (next-stmts lis) (add-layer state) current-type
                                                                  return break continue throw))]
      [(eq? (stmt-type lis) 'break)    (break (remove-top-layer state))]
      [(eq? (stmt-type lis) 'continue) (continue (remove-top-layer state))]
      [(eq? (stmt-type lis) 'try)      (M-state (finally-block lis)
                                                (M-state (catch-block lis)
                                                         (call/cc (lambda (throw)
                                                                    (M-state (try-block lis)
                                                                             state current-type return break continue throw)))
                                                        current-type return break continue throw)
                                                current-type return break continue throw)]
      [(eq? (stmt-type lis) 'catch)    (catch lis state current-type return break continue throw)]
      [(eq? (stmt-type lis) 'finally)  (finally lis state current-type return break continue throw)]
      [(eq? (stmt-type lis) 'throw)    (throw (state-add 'error (M-value (return-value lis) state current-type throw) (add-layer state)))]
      [(eq? (stmt-type lis) 'function) (function current-type(function-name lis) (function-params lis) (function-body lis) state)]
      [(and (list? (stmt-type lis)) (eq? (stmt-type lis) 'funcall))  (funcall (cons 'dot (cons 'this (list (funcall-name lis)))) (funcall-params lis) state current-type throw)]
      [(eq? (stmt-type lis) 'funcall)  (funcall (funcall-name lis) (funcall-params lis) state current-type throw)]
      [else                            (M-state (next-stmts lis)
                                                (M-state (first-stmt lis) state
                                                         current-type return break continue throw)
                                                current-type return break continue throw)])))

;; funcall - interprets a functional call statement and returns the resulting state
(define funcall
  (lambda (dot-expression params state current-type throw)
    (begin (funcall-value dot-expression params state current-type throw) state)))

;; funcall-value - interprets a functional call statement and returns the value
(define funcall-value
  (lambda (dot-expression params state current-type throw)
    (call/cc (lambda (return)
               (cond
                 ((and (list? dot-expression) (eq? 'super (left-operand dot-expression)))
                   (M-state (function-closure-body (super-look-up-function (right-operand dot-expression) state current-type throw))
                        (bind-params (function-closure-params (super-look-up-function (right-operand dot-expression) state current-type throw))
                                     (cons 'this params)
                                     state
                                     ((function-closure-env (super-look-up-function (right-operand dot-expression) state current-type throw)) (add-layer state))
                                     current-type)
                        (function-closure-class (super-look-up-function (right-operand dot-expression) state current-type throw))
                        return
                        (lambda (state) (error 'break "invalid break"))
                        (lambda (state) (error 'continue "invalid continue"))
                        throw))
               
               ((list? dot-expression) (M-state (function-closure-body (look-up-function dot-expression state current-type throw))
                        (bind-params (function-closure-params (look-up-function dot-expression state current-type throw))
                                     (cons (left-operand dot-expression) params)
                                     state
                                     ((function-closure-env (look-up-function dot-expression state current-type throw)) (add-layer state))
                                     current-type)
                        (function-closure-class (look-up-function dot-expression state current-type throw))
                        return
                        (lambda (state) (error 'break "invalid break"))
                        (lambda (state) (error 'continue "invalid continue"))
                        throw))
                      )))))

;; look up the function in the instance closure, return the function closure
(define look-up-function
  (lambda (dot-expression state current-type throw)
    (M-value (right-operand dot-expression) (function-list (instance-class-closure (M-value (left-operand dot-expression) state current-type throw))) current-type throw)))

(define super-look-up-function
  (lambda (name state current-type throw)
    (M-value name (function-list (M-value (parent-class (M-value current-type state current-type throw)) state current-type throw)) current-type throw)))
;; funcall-value-static - interprets a functional call statement and returns the value
(define funcall-value-static
  (lambda (name params function-state state current-type throw)
    (call/cc (lambda (return)
               (M-state (function-closure-body (M-name name function-state))
                        (bind-params (function-closure-params (M-name name function-state))
                                     params
                                     state
                                     ((function-closure-env (M-name name function-state)) (add-layer state))
                                     current-type)
                        current-type
                        return
                        (lambda (state) (error 'break "invalid break"))
                        (lambda (state) (error 'continue "invalid continue"))
                        throw)))))

(define find-global-state
  (lambda (name state)
    (cond
      [(var-in-scope? name (var-list state)) state]
      [else (find-global-state name (next-layer state))])))

;; bind-params - returns the given state with the formal parameters bound to the actual parameters
;; in the topmost layer, has an accumulator-style structure
(define bind-params
  (lambda (formal actual current-state state current-type)
    (cond
      [(and (null? formal) (null? actual))
       state]
      [(or (null? formal) (null? actual))
       (error 'parameters "Mismatched parameters and arguments")]
      [else
       (bind-params (cdr formal)
                    (cdr actual)
                    current-state
                    (state-add (car formal)
                               (M-value (car actual)
                                        current-state
                                        current-type
                                        (lambda (state) (error 'throw "invalid throw")))
                               state)
                    current-type)])))


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
  (lambda (stmt state current-type throw)
    (cond
      [(null? stmt)
       (error 'declare "invalid declare statement")]
      [(var-in-scope? (var-name stmt) (var-list state))
       (error 'declare "Redefining variable error, variable previously declared")]
      [(null? (cddr stmt))
       (state-add (var-name stmt) 'novalue state)]
      [else
        (state-add (var-name stmt)
                   (M-value (var-value stmt) state current-type throw)
                   state)])))

;; assign - interprets a varible assignment statement, returns a value and state
(define assign
  (lambda (stmt state original-state current-type return break continue throw)
    (cond
      [(null? stmt)
       (error 'assign-interpret "invalid assign statement")]
      [(null? state)
       (field-update (cons 'dot (cons 'this (list (var-name stmt)))) (var-value stmt) original-state current-type return break continue throw)]
       ;(error 'assign-error "variable not found, out-of-scope")]
      [(list? (var-name stmt)) (field-update (var-name stmt) (var-value stmt) state current-type return break continue throw)]
      [(var-in-scope? (var-name stmt) (var-list state))
       (begin (set-box! (get-value (var-name stmt) (var-list state) (val-list state))
                        (M-value (var-value stmt) original-state current-type throw))
              state)]
      [else
        (cons (car state)
              (assign stmt (next-layer state) original-state current-type return break continue throw))])))

;;update the non-static fields
(define field-update
  (lambda (dot-expression value state current-type return break continue throw)
    (begin (set-box! (get-value (right-operand dot-expression)
                                (field-names (field-list (instance-class-closure (M-value (left-operand dot-expression) state current-type throw))))
                                (instance-field-values (M-value (left-operand dot-expression) state current-type throw)))
                     (M-value value state current-type throw))
              state)))

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
  (lambda (stmt state current-type return break continue throw)
      (cond
        [(M-value (condition stmt) state current-type throw)
         (M-state (statement stmt)
                  (M-state (condition stmt) state current-type return break continue throw)
                  current-type return break continue throw)]
        [(null? (cdddr stmt))
         (M-state (condition stmt) state
                  current-type return break continue throw)]
        [else
          (M-state (else-statement stmt)
                   (M-state (condition stmt) state current-type return break continue throw)
                   current-type return break continue throw)])))

;; while - interprets a while statement
(define while
  (lambda (stmt state current-type return break continue throw)
       (if (M-value (condition stmt) state current-type throw)
           (while stmt (call/cc (lambda (continue)
                                  (M-state (statement stmt)
                                           (M-state (condition stmt) state current-type return break continue throw)
                                           current-type return break continue throw))) current-type return break continue throw)
           (M-state (condition stmt) state
                    current-type return break continue throw))))

;; catch - interprets a catch clause
(define catch
  (lambda (stmt state current-type return break continue throw)
    (cond
      [(null? state)
       state]
      [(null? (var-list state))
       state]
      [(eq? (car (var-list state)) 'error)
       (M-state (catch-body stmt) (cons (list (catch-exception stmt) (val-list state)) (next-layer state))
                current-type return break continue throw)]
      [else
        state])))

;; finally - interprets a finally clause
(define finally
  (lambda (stmt state current-type return break continue throw)
    (if (null? stmt)
        state
        (M-state (finally-body stmt) state current-type return break continue throw))))

;; state-add - add the specified variable and its value to the top-most state layer in the program state
(define state-add
  (lambda (name value state)
    (if (null? (next-layer state))
        (list (list (append (var-list state) (list name)) (append (val-list state) (list (box value)))))
        (cons (list (append (var-list state) (list name)) (append (val-list state) (list (box value))))
              (next-layer state)))))

;; M-name - returns the value of the specified variable/value
(define M-name
  (lambda (name state)
    (cond
      [(null? state)                         'error];(error 'M-name "variable not found, using before declaring")];(lookup-field name (M-name 'this state))];
      [(number? name)                        name]
      [(eq? name 'true)                      #t]
      [(eq? name 'false)                     #f]
      [(var-in-scope? name (var-list state)) (unbox (get-value name (var-list state) (val-list state)))]
      [else                                  (M-name name (next-layer state))])))

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
  (lambda (expr state current-type throw)
    (cond
      [(null? expr) (error 'M-value "undefined expression")]
      [(and (not (list? expr)) (eq? 'error (M-name expr state))) (lookup-field expr (M-name 'this state) state current-type)]
      [(not (list? expr)) (M-name expr state)]
      [(eq? (math-operator expr) '=) (M-value (var-value expr) state current-type throw)]
      [(and (eq? (stmt-type expr) 'dot) (eq? (left-operand expr) 'this)) (lookup-field (right-operand expr) (M-value (left-operand expr) state current-type throw) state current-type)]
      [(eq? (stmt-type expr) 'dot) (dot-lookup-field (right-operand expr) (M-value (left-operand expr) state current-type throw) state)]
      [(eq? (stmt-type expr) 'new) (make-instance-closure (var-name expr) state)]
      [(and (eq? (stmt-type expr) 'funcall) (list? (funcall-name expr)))  (funcall-value (funcall-name expr) (funcall-params expr) state current-type throw)]
      [(eq? (stmt-type expr) 'funcall)  (funcall-value (cons 'dot (cons 'this (list (funcall-name expr)))) (funcall-params expr) state current-type throw)]
      [(eq? (math-operator expr) '+) (+ (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(and (eq? (math-operator expr) '-) (is-right-operand-null? expr)) (* -1 (M-value (left-operand expr) state current-type throw))]
      [(eq? (math-operator expr) '-) (- (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (math-operator expr) '*) (* (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (math-operator expr) '/) (quotient (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (math-operator expr) '%) (remainder (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '==) (eq? (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '!=) (not (eq? (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw)))]
      [(eq? (comp-operator expr) '<) (< (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '>) (> (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '<=) (<= (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '>=) (>= (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (bool-operator expr) '&&) (and (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (bool-operator expr) '||) (or (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (bool-operator expr) '!) (not (M-value (left-operand expr) state current-type throw))])))


;; An instance closure:list of field values + class closure
(define make-instance-closure
  (lambda (class-name state)
    (list (rebox (field-values (get-field-list class-name state))) (M-name class-name state))))

(define rebox
  (lambda (lis)
    (cond
      [(null? lis) lis]
      [else (cons (box (unbox (car lis))) (rebox (cdr lis)))])))

(define instance-field-values car)
(define instance-class-closure cadr)

;; look up a field in the instance closure given a variable name
(define lookup-field
  (lambda (name instance-closure state current-type)
    (unbox (reverse-find name (field-names (field-list (M-name current-type state))) (reverse (instance-field-values instance-closure)) #f))))

(define dot-lookup-field
  (lambda (name instance-closure state)
    (unbox (reverse-find name (field-names (field-list (instance-class-closure instance-closure))) (reverse (instance-field-values instance-closure)) #f))))

;;helper function - find the values using the right index in the reversed list - solve polymorphism
(define reverse-find
  (lambda (name variable-list reversed-value-list trigger)
    (cond
      [(null? variable-list) (car reversed-value-list)]
      [trigger (reverse-find name (cdr variable-list) (cdr reversed-value-list) trigger)]
      [(eq? name (car variable-list)) (reverse-find name (cdr variable-list) reversed-value-list #t)]
      [else (reverse-find name (cdr variable-list) reversed-value-list trigger)])))

;; helper-function to reverse a list
(define reverse
  (lambda (lis)
    (cond
      [(null? lis) lis]
      [else (append (reverse (cdr lis)) (list (car lis)))])))

;; is-right-operand-null? - returns true if the given expression uses a unary operator, otherwise false
(define is-right-operand-null?
  (lambda (expr)
    (null? (cddr expr))))

;; Helper Functions (Abstraction) -----------------------------------------------------------------

;; State representation
(define var-list caar)
(define val-list cadar)
(define top-layer car)
(define next-layer cdr)

;; Closures
(define function-closure-params car)
(define function-closure-body cadr)
(define function-closure-env caddr)
(define function-closure-class cadddr)
(define class-closure-superclass car)
(define class-closure-field-names cadr)
(define class-closure-methods caddr)

;; Statement interpretation
(define stmt-type car)
(define first-stmt car)
(define next-stmts cdr)
(define return-value cadr)
(define try-block cadr)
(define catch-block caddr)
(define finally-block cadddr)
(define function-name cadr)
(define function-params caddr)
(define function-body cadddr)
(define funcall-name cadr)
(define funcall-params cddr)

;; Statement components
(define var-name cadr)
(define var-value caddr)
(define condition cadr)
(define statement caddr)
(define else-statement cadddr)
(define catch-exception cadr)
(define catch-body caddr)
(define finally-body cadr)

;; Expression evaluation
(define math-operator car)
(define comp-operator car)
(define bool-operator car)
(define left-operand cadr)
(define right-operand caddr)

