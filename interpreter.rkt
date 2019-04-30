;;;; ***************************************************
;;;;	Amrish Selvam (axs1330), Kai Wang (kxw367), Timothy Jin (tcj16)
;;;;	EECS 345 - Spring 2019
;;;;	Interpreter
;;;; ***************************************************

#lang racket

(provide (all-defined-out))

(require "classParser.rkt")

;; Basic representation of state
(define empty-layer (list '() '()))

;; Top Level Functions ----------------------------------------------------------------------------

;; interpret - top level function called by the user, calls main on the specified class
(define interpret
  (lambda (filename main-class)
    (call-main 'main
               '()
               (get-function-list (string->symbol main-class)
                                  (interpret-class-definitions (parser filename)
                                                               (list empty-layer)
                                                               (lambda (state) (error 'throw "invalid throw"))))
               (interpret-class-definitions (parser filename)
                                            (list empty-layer)
                                            (lambda (state) (error 'throw "invalid throw")))
               'null
               (lambda (state) (error 'throw "invalid throw")))))

;; call-main - calls the main method of the specified class
(define call-main
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

;; State Definitions Interpretation -------------------------------------------------------------------

;; interpret-class-definitions - returns a state with all class definitions
(define interpret-class-definitions
  (lambda (lis state throw)
    (cond
      [(null? lis) state]
      [(eq? (stmt-type lis) 'class) (add-class-closure (cdr lis) state throw)]
      [else (interpret-class-definitions (next-stmts lis) (interpret-class-definitions (first-stmt lis) state throw) throw)])))

;; add-class-closure - binds a class to its corresponding class closure in the state
(define add-class-closure
  (lambda (lis state throw)
    (if (pair? (extends-stmt lis))
        (state-add (class-name lis)
                   (combine-class-closures (M-name (superclass-name lis) state)
                                           (make-class-closure (class-name lis)
                                                               (class-body lis)
                                                               (cons (superclass-name lis) empty-class-closure)
                                                               throw))
                   state)
        (state-add (class-name lis)
                   (make-class-closure (class-name lis)
                                       (class-body lis)
                                       (cons '() empty-class-closure)
                                       throw)
                   state))))

;; combine-class-closures - combines a descendant class's closure with its parent class's closure
(define combine-class-closures
  (lambda (parent-closure child-closure)
    (list (class-closure-superclass child-closure)
          (list (list (append (field-names (class-closure-fields child-closure))
                              (field-names (class-closure-fields parent-closure)))
                      (append (field-values (class-closure-fields child-closure))
                              (field-values (class-closure-fields parent-closure)))))
          (list (list (append (function-names (class-closure-functions child-closure))
                              (function-names (class-closure-functions parent-closure)))
                      (append (function-closures (class-closure-functions child-closure))
                              (function-closures (class-closure-functions parent-closure))))))))

;; make-class-closure - interprets the given class definition and returns the closure
(define make-class-closure
  (lambda (class lis class-closure throw)
    (cond
      [(null? lis)
       class-closure]
      [(eq? (stmt-type lis) 'var)
       (list (class-closure-superclass class-closure)
             (declare lis (class-closure-fields class-closure) 'null throw)
             (class-closure-functions class-closure))]
      [(eq? (stmt-type lis) 'static-function)
       (list (class-closure-superclass class-closure)
             (class-closure-fields class-closure)
             (function class (function-name lis) (function-params lis) (function-body lis) (class-closure-functions class-closure)))]
      [(eq? (stmt-type lis) 'function)
       (list (class-closure-superclass class-closure)
             (class-closure-fields class-closure)
             (function class (function-name lis) (cons 'this (function-params lis)) (function-body lis) (class-closure-functions class-closure)))]
      [else
        (make-class-closure class (next-stmts lis) (make-class-closure class (first-stmt lis) class-closure throw) throw)])))

;; State Manipulation -----------------------------------------------------------------------------

;; M-state - given a statement and a state, returns the state resulting from applying the statement
;; to the given state
(define M-state
  (lambda (lis state current-type return break continue throw)
    (cond
      [(null? lis)                     state]
      [(not (list? lis))               state]
      [(eq? (stmt-type lis) 'var)      (declare lis state current-type throw)]
      [(eq? (stmt-type lis) '=)        (assign lis state state current-type return break continue throw)]
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
      [(eq? (stmt-type lis) 'function) (function current-type (function-name lis) (function-params lis) (function-body lis) state)]
      [(and (eq? (stmt-type lis) 'funcall) (list? (funcall-name lis)))
       (funcall (funcall-name lis) (funcall-params lis) state current-type throw)]
      [(and (eq? (stmt-type lis) 'funcall) (eq? 'error (M-name (funcall-name lis) state)))
       (funcall (cons 'dot (cons 'this (list (funcall-name lis)))) (funcall-params lis) state current-type throw)]
      [(eq? (stmt-type lis) 'funcall) (funcall (funcall-name lis) (funcall-params lis) state current-type throw)]
      [else                            (M-state (next-stmts lis)
                                                (M-state (first-stmt lis) state
                                                         current-type return break continue throw)
                                                current-type return break continue throw)])))

;; function - interprets a function definition
(define function
  (lambda (class name params body state)
    (state-add name (make-function-closure params body class) state)))

;; make-function-closure - return the closure of a function
(define make-function-closure
 (lambda (params body class)
   (list params body get-function-environment class)))

;; get-function-environment - returns a function that takes creates a function environment by appending
;; the state at the function call onto the function's state in scope
(define get-function-environment (lambda (state) state))

;; funcall - interprets a function call statement and returns the state
(define funcall
  (lambda (dot-expression params state current-type throw)
    (begin (funcall-value dot-expression params state current-type throw) state)))

;; funcall-value - interprets a function call statement and returns the value
(define funcall-value
  (lambda (call-expr params state current-type throw)
    (call/cc (lambda (return)
               (cond
                 [(and (list? call-expr) (eq? 'super (left-operand call-expr)))
                  (M-state (function-closure-body (lookup-super-function (right-operand call-expr) state current-type throw))
                           (bind-params (function-closure-params (lookup-super-function (right-operand call-expr) state current-type throw))
                                        (cons 'this params)
                                        state
                                        ((function-closure-env (lookup-super-function (right-operand call-expr) state current-type throw))
                                         (add-layer state))
                                        current-type)
                           (function-closure-class (lookup-super-function (right-operand call-expr) state current-type throw))
                           return
                           (lambda (state) (error 'break "invalid break"))
                           (lambda (state) (error 'continue "invalid continue"))
                           throw)]
                 [(list? call-expr)
                  (M-state (function-closure-body (lookup-function call-expr state current-type throw))
                           (bind-params (function-closure-params (lookup-function call-expr state current-type throw))
                                        (cons (left-operand call-expr) params)
                                        state
                                        ((function-closure-env (lookup-function call-expr state current-type throw)) (add-layer state))
                                        current-type)
                           (function-closure-class (lookup-function call-expr state current-type throw))
                           return
                           (lambda (state) (error 'break "invalid break"))
                           (lambda (state) (error 'continue "invalid continue"))
                           throw)]
                 [else
                  (M-state (function-closure-body (M-name call-expr state))
                           (bind-params (function-closure-params (M-name call-expr state))
                                        params
                                        state
                                        ((function-closure-env (M-name call-expr state)) (add-layer state))
                                        current-type)
                           current-type
                           return
                           (lambda (state) (error 'break "invalid break"))
                           (lambda (state) (error 'continue "invalid continue"))
                           throw)])))))

;; lookup-function - looks up and returns the specified function in the instance closure
(define lookup-function
  (lambda (dot-expression state current-type throw)
    (M-value (right-operand dot-expression)
             (class-closure-functions (instance-closure-class (M-value (left-operand dot-expression) state current-type throw)))
             current-type
             throw)))

;; lookup-super-function - looks up and returns the specified function in the instance's parent class's closure
(define lookup-super-function
  (lambda (name state current-type throw)
    (M-value name
             (class-closure-functions (M-value (class-closure-superclass (M-value current-type state current-type throw)) state current-type throw))
             current-type
             throw)))

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
       (update-field (cons 'dot (cons 'this (list (var-name stmt))))
                     (var-value stmt)
                     original-state current-type return break continue throw)]
      [(list? (var-name stmt))
       (update-field (var-name stmt)
                     (var-value stmt)
                     state current-type return break continue throw)]
      [(var-in-scope? (var-name stmt) (var-list state))
       (begin (set-box! (get-value (var-name stmt) (var-list state) (val-list state))
                        (M-value (var-value stmt) original-state current-type throw))
              state)]
      [else
        (cons (car state)
              (assign stmt (next-layers state) original-state current-type return break continue throw))])))

;; update-field - updates the value of the given instance field with the specified value
(define update-field
  (lambda (dot-expression value state current-type return break continue throw)
    (begin (set-box! (get-value (right-operand dot-expression)
                                (field-names (class-closure-fields (instance-closure-class (M-value (left-operand dot-expression) state current-type throw))))
                                (instance-closure-field-values (M-value (left-operand dot-expression) state current-type throw)))
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
       (M-state (catch-body stmt) (cons (list (catch-exception stmt) (val-list state)) (next-layers state))
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
    (if (null? (next-layers state))
        (list (list (append (var-list state) (list name)) (append (val-list state) (list (box value)))))
        (cons (list (append (var-list state) (list name)) (append (val-list state) (list (box value))))
              (next-layers state)))))

;; add-layer - adds an empty state layer on top of the current state
(define add-layer
  (lambda (state)
    (cons empty-layer state)))

;; remove-top-layer - removes the top-most state layer from the current state
(define remove-top-layer
  (lambda (state)
    (next-layers state)))

;; State Information Retrieval --------------------------------------------------------------------

;; M-name - returns the value of the specified variable/value
(define M-name
  (lambda (name state)
    (cond
      [(null? state)                         'error]
      [(number? name)                        name]
      [(eq? name 'true)                      #t]
      [(eq? name 'false)                     #f]
      [(var-in-scope? name (var-list state)) (unbox (get-value name (var-list state) (val-list state)))]
      [else                                  (M-name name (next-layers state))])))

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
      [(null? expr)                   (error 'M-value "undefined expression")]
      [(and (not (list? expr)) (eq? 'error (M-name expr state)))
       (lookup-field expr (M-name 'this state) state current-type)]
      [(not (list? expr))
       (M-name expr state)]
      [(eq? (math-operator expr) '=)  (M-value (var-value expr) state current-type throw)]
      [(and (eq? (stmt-type expr) 'dot) (eq? (left-operand expr) 'this))
       (lookup-field (right-operand expr) (M-value (left-operand expr) state current-type throw) state current-type)]
      [(eq? (stmt-type expr) 'dot)
       (dot-lookup-field (right-operand expr) (M-value (left-operand expr) state current-type throw) state)]
      [(eq? (stmt-type expr) 'new) (make-instance-closure (var-name expr) state)]
      [(and (eq? (stmt-type expr) 'funcall) (list? (funcall-name expr)))
       (funcall-value (funcall-name expr) (funcall-params expr) state current-type throw)]
      [(eq? (stmt-type expr) 'funcall)
       (funcall-value (cons 'dot (cons 'this (list (funcall-name expr)))) (funcall-params expr) state current-type throw)]
      [(eq? (math-operator expr) '+)  (+ (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(and (eq? (math-operator expr) '-) (is-right-operand-null? expr))
       (* -1 (M-value (left-operand expr) state current-type throw))]
      [(eq? (math-operator expr) '-)
       (- (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (math-operator expr) '*)  (* (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (math-operator expr) '/)  (quotient (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (math-operator expr) '%)  (remainder (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '==) (eq? (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '!=) (not (eq? (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw)))]
      [(eq? (comp-operator expr) '<)  (< (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '>)  (> (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '<=) (<= (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (comp-operator expr) '>=) (>= (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (bool-operator expr) '&&) (and (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (bool-operator expr) '||) (or (M-value (left-operand expr) state current-type throw) (M-value (right-operand expr) state current-type throw))]
      [(eq? (bool-operator expr) '!)  (not (M-value (left-operand expr) state current-type throw))])))

;; make-instance-closure - creates and returns an instance closure of the specified class
(define make-instance-closure
  (lambda (class-name state)
    (list (rebox (field-values (get-field-list class-name state))) (M-name class-name state))))

;; rebox - given a list of boxed values, returns a list of different boxes with the same values
(define rebox
  (lambda (lis)
    (cond
      [(null? lis) lis]
      [else (cons (box (unbox (car lis))) (rebox (cdr lis)))])))

;; lookup-field - looks up the given field in the instance closure
(define lookup-field
  (lambda (name instance-closure state current-type)
    (unbox (reverse-find name
                         (field-names (class-closure-fields (M-name current-type state)))
                         (reverse (instance-closure-field-values instance-closure))
                         #f))))

;; dot-lookup-field
(define dot-lookup-field
  (lambda (name instance-closure state)
    (unbox (reverse-find name
                         (field-names (class-closure-fields (instance-closure-class instance-closure)))
                         (reverse (instance-closure-field-values instance-closure))
                         #f))))

;; reverse-find - find the value using the right index in the reversed list
(define reverse-find
  (lambda (name variable-list rev-value-list flag)
    (cond
      [(null? variable-list) (car rev-value-list)]
      [flag (reverse-find name (cdr variable-list) (cdr rev-value-list) flag)]
      [(eq? name (car variable-list)) (reverse-find name (cdr variable-list) rev-value-list #t)]
      [else (reverse-find name (cdr variable-list) rev-value-list flag)])))

;; is-right-operand-null? - returns true if the given expression uses a unary operator, otherwise false
(define is-right-operand-null?
  (lambda (expr)
    (null? (cddr expr))))

;; get-function-list - returns the list of functions and closures of the specified class
(define get-function-list
  (lambda (class-name state)
    (class-closure-functions (M-name class-name state))))

;; get-field-list - returns the list of instance fields of the specified class
(define get-field-list
  (lambda (class-name state)
    (class-closure-fields (M-name class-name state))))

;; Helper Functions (Abstraction) -----------------------------------------------------------------

;; State representation
(define var-list caar)
(define val-list cadar)
(define top-layer car)
(define next-layers cdr)

;; Closure components
(define empty-class-closure (list (list empty-layer) (list empty-layer)))
(define class-closure-superclass car)
(define class-closure-fields cadr)
(define class-closure-functions caddr)
(define field-names caar)
(define field-values cadar)
(define function-names caar)
(define function-closures cadar)
(define instance-closure-field-values car)
(define instance-closure-class cadr)
(define function-closure-params car)
(define function-closure-body cadr)
(define function-closure-env caddr)
(define function-closure-class cadddr)

;; Statement interpretation
(define class-name car)
(define extends-stmt cadr)
(define superclass-name cadadr)
(define class-body caddr)
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
