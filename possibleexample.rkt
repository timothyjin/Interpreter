;; Imran Hossain
;; Calvin Vuong
;; Ben Young

#lang racket

(require "classParser.rkt")
;(require "functionParser.rkt")

(define initial-state '((() ())))

(define initialContinuations 
  (lambda (return)
    (make-immutable-hash
      (list (cons 'return return)
            (cons 'break 
                  (lambda (v) 
                    (error 'error "Improper break placement.")))
            (cons 'continue 
                  (lambda (v) 
                    (error 'error "Improper continue placement.")))
            (cons 'throw 
                  (lambda (v) 
                    (error 'invalid_throw "Not in try/catch. Cannot throw exception")))))))

;; calls interpret-tree on parsed code
;; r is the continuation for return
; lambda (v) v is just the "default" continuation for break and continue
(define interpret
  (lambda (filename class)
    (translate-boolean
      (call/cc
       (lambda (return) ; continuation for return
         ;;(M-state expr state return break continue throw)
         ;;initially break, continue, and throw should give errors
         ;;because we aren't in a structure where we can call them
         ;; assumes main takes no parameters
         ;(cons 'funcall (list (cons 'dot (cons class '(main))))))))))
         (M-value  (cons 'funcall (list (cons 'dot (cons class '(main)))))
                   (M-state-global (parser filename) initial-state)
                   (initialContinuations return)))))))



(define get-main-class-closure
  (lambda (class state)
    (get-var-value state class))) ; returns the hashset closure of the class

;;turns #f and #t into 'false and 'true for final return
(define translate-boolean
  (lambda (x)
    (cond
      [(eq? x #f) 'false]
      [(eq? x #t) 'true]
      [else x])))

;;returns length of lis
(define list-length-cps
  (lambda (lis return)
    (cond
      [(null? lis) (return 0)]
      [else (list-length-cps (cdr lis) (lambda (v) 
                                         (return (+ 1 v))))])))

(define list-length
  (lambda (lis)
    (list-length-cps lis (lambda (v) v))))

(define get-class-method-closure
  (lambda (class var state)
    (get-var-value (hash-ref (get-var-value class state) 'methods) var)))
              
;; returns the value of variable in current state
(define get-var-value
  (lambda (state var)
    (cond
      [(eq? (find-box var state) 'none)            (error 'undeclared "Variable not declared.")]
      [(eq? (unbox (find-box var state)) 'null)    (error 'uninitialized "Using before assigning.")]
      [else                                        (unbox (find-box var state))])))

;;returns numeric or boolean value of expression
(define M-value-cps
  (lambda (expr state cps-return continuations)
    (cond 
      [(null? expr)                     (error 'undefined "Empty expression")]
      [(number? expr)                   (cps-return expr)]
      ;; if expr isn't a pair and isn't a number, it's a variable
      ;; so look it up in the state
      [(eq? expr 'false)                (cps-return #f)]
      [(eq? expr 'true)                 (cps-return #t)]
      [(not (list? expr))               (cps-return (get-var-value state expr))]  
      ;; if next exprression's length is 2, it's unary -
      [(and (eq? (list-length expr) 2)
            (eq? (get-operator expr) '-))
       (M-value-cps (exp1 expr) 
                    state 
                    (lambda (v) 
                      (cps-return (* -1 v)))
                    continuations)]
      ;;function
      [(and (eq? (get-operator expr) 'funcall)
            (>= (list-length expr) 3))
       (cps-return (M-value-function expr state continuations))]
      [(eq? (get-operator expr) 'funcall) (cps-return (M-value-function expr state continuations))]
      [(eq? (get-operator expr) 'new) (cps-return (instance-closure (exp1 expr) state))]
      [(eq? (get-operator expr) 'return) (cps-return (M-value (exp1 expr) state continuations))]
      [(eq? (list-length expr) 2)        (error 'undefined "Incorrect number of arguments")]
      [(not (eq? (list-length expr) 3))  (error 'undefined "Incorrect number of arguments")]
      [(eq? (get-operator expr) '+) (M-value-math-operator expr state + cps-return continuations)]
      [(eq? (get-operator expr) '-) (M-value-math-operator expr state - cps-return continuations)]
      [(eq? (get-operator expr) '*) (M-value-math-operator expr state * cps-return continuations)]
      [(eq? (get-operator expr) '/) (M-value-math-operator expr state quotient cps-return continuations)]
      [(eq? (get-operator expr) '%) (M-value-math-operator expr state remainder cps-return continuations)]
      [else (cps-return (M-boolean expr state continuations))])))

;;calls M-value-cps
(define M-value
  (lambda (expr state continuations)
    (M-value-cps expr state (lambda (v) v) continuations)))

;; M-value for evaluating a function call
;; This expects a dot expression in the expr
(define M-value-function
  (lambda (expr state continuations)
    (call/cc
     (lambda (r) ;; new continuation for return
       ((lambda (method-closure)
         (remove-layer
           (M-state (hash-ref method-closure 'body) ;;get function body
                    (append (bind-params (hash-ref method-closure 'params)
                                 (get-actual-params expr)
                                 (push-layer
                                  ((hash-ref method-closure 'env) (get-method-call-name expr) state))
                                 state
                                 continuations)
			    state)
                    (hash-set* continuations 'return r))))
         (get-method-closure (get-method-call-name expr) (get-class expr) state))))))
;; TODO Write helper to get class closure

;; M-value for creating a new object - returns an object closure
;; just call the class's constructor
(define instance-closure
  (lambda (class state)
    (make-immutable-hash
      (list (cons 'class class)
            (cons 'inst-vals ((hash-ref (get-var-value state class) 'const)))))))

;; env "new state" which is what get-func-env returns
;; state "old state"
;; returns the env, not the state
(define bind-params
  (lambda (formal actual env state continuations)
    (cond
      [(and (null? formal) (null? actual))       env]
      ;; formal params and actual params differ in lenght
      [(or (null? formal) (null? actual))       (error 'error "Function received incorrect number of arguments.")]
      [else                                     (bind-params (cdr formal) (cdr actual)
                                                             (add-to-state (list (car formal)
                                                                                 (M-value (car actual) state continuations))
                                                                           env)
                                                             state
                                                             continuations)])))

;; gets the closure of method from class
;; find the method in class's method list whose name matches method-name
(define get-method-closure
  (lambda (method-name class state)
      (car (filter (lambda (h) (eq? (hash-ref h 'name) method-name))
              (hash-ref (get-var-value state class) 'methods)))))

;;;;;;; TODO: do we need these? $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;; returns a list of the formal params from the function closure
(define get-formal-params-class
  (lambda (name class state)
    (get-formal-params-class-helper name (hash-ref (get-var-value state class) 'methods))))

(define get-formal-params-class-helper
  (lambda (name state)
    (cond
      [(null? (car state))         (error 'error "Nothing found.")]
      [(eq? (caar state) name)     (caaadr state)]
      [else                        (get-formal-params-class-helper name (list (cdar state) (cdadr state)))])))

(define get-body
  (lambda (name state)
    (cadr (get-var-value state name))))

;; gets the body of the function name that resides within class
(define get-body-class
  (lambda (name class state)
    (get-body-class-helper name (hash-ref (get-var-value state class) 'methods))))

(define get-body-class-helper
  (lambda (name state)
    (cond
      [(null? (car state))       (error 'error "Nothing found.")]
      [(eq? (caar state) name)   (car (cdaadr state))]
      [else                      (get-body-class-helper name (list (cdar state) (cdadr state)))])))
;;;;;;; TODO: do we need these?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    

;; returns the function for get-env in the closure
(define get-env-func
  (lambda (name state)
    (caddr (get-var-value state name))))

;; returns the get-env in the closure, but for methods in classes
(define get-env-func-class
  (lambda (name class state)
     (get-env-func-class-helper name (hash-ref (get-var-value state class) 'methods))))

(define get-env-func-class-helper
  (lambda (name state)
    (cond
      [(null? (car state))          (error 'error "Nothing found.")]
      [(eq? (caar state) name)      (cadr (cdaadr state))]
      [else                         (get-env-func-class-helper name (list (cdar state) (cdadr state)))])))
       
         

;; abstracted macros
(define get-method-def-name cadar)
(define get-method-call-name (compose car cddadr))
(define get-class cadadr)
(define get-actual-params cddr)

    
;;abstraction for when M-value-cps is given an arithmetic operation
(define M-value-math-operator 
  (lambda (expr state operation cps-return continuations)
    (M-value-cps (exp1 expr) 
                 state
                 (lambda (v1) 
                   (M-value-cps (exp2 expr) 
                                state
                                (lambda (v2) 
                                  (cps-return (operation v1 v2)))
                                continuations))
                 continuations)))

;;returns whether an expression is true or false
(define M-boolean-cps
  (lambda (expr state cps-return continuations)
    (cond
      [(null? expr)                       (error 'undefined "Incorrect number of arguments")]
      [(eq? expr 'true)                   (cps-return #t)]
      [(eq? expr 'false)                  (cps-return #f)]
      ;;otherwise, this is a variable
      [(not (list? expr))                 (cps-return (get-var-value state expr))] 
      ;;or a function
      [(eq? (get-operator expr) 'funcall) (cps-return (M-value-function expr state continuations))]
      [(eq? (get-operator expr) '==)      (M-boolean-comparator expr state cps-return eq? continuations)]
      [(eq? (get-operator expr) '!=)      (M-boolean-comparator expr 
                                                                state 
                                                                cps-return
                                                                (lambda (a b)
                                                                  (not (eq? a b)))
                                                                continuations)]
      [(eq? (get-operator expr) '<)       (M-boolean-comparator expr state cps-return < continuations)]
      [(eq? (get-operator expr) '>)       (M-boolean-comparator expr state cps-return > continuations)]
      [(eq? (get-operator expr) '<=)      (M-boolean-comparator expr state cps-return <= continuations)]
      [(eq? (get-operator expr) '>=)      (M-boolean-comparator expr state cps-return >= continuations)]
      [(eq? (get-operator expr) '&&)      (M-boolean-logic-operator 
                                            expr state 
                                            (lambda (a b) 
                                              (and a b))
                                            cps-return continuations)] 
      [(eq? (get-operator expr) '||)      (M-boolean-logic-operator 
                                            expr state 
                                            (lambda (a b) 
                                              (or a b))
                                            cps-return continuations)] 
      [(eq? (get-operator expr) '!)       (M-boolean-cps (exp1 expr) 
                                                         state
                                                         (lambda (v) 
                                                           (cps-return (not v)))
                                                         continuations)]
      [else (error 'invalid_expression "Invalid value or boolean expression!")])))
      
;;calls M-boolean-cps
(define M-boolean
  (lambda (expr state continuations)
    (M-boolean-cps expr state (lambda (v) v) continuations)))

;;abstraction for when M-boolean-cps is given an comparator (<, >, ==, etc) operation
(define M-boolean-comparator
  (lambda (expr state cps-return operation continuations)
    (cps-return (operation (M-value (exp1 expr) state continuations)
                           (M-value (exp2 expr) state continuations)))))

;;abstraction for when M-boolean-cps is given a logic operation
(define M-boolean-logic-operator 
  (lambda (expr state operation cps-return continuations)
    (M-boolean-cps (exp1 expr) 
                    state
                    (lambda (v1) 
                      (M-boolean-cps (exp2 expr) 
                                   state
                                   (lambda (v2) 
                                     (cps-return (operation v1 v2)))
                                   continuations))
                    continuations)))

;; Add a binding to the state
;; takes a list of two elements: key value pair
;; max 1 recursion: cps not necessary
(define add-to-state
  (lambda (binding state)
    (if (null? state)
        '()
        (cons (cons (cons (car binding) (caar state)) (list (cons (box (cadr binding)) (cadar state)))) (cdr state)))))

;; s1 are the list of variable names
;; s2 are the list of values
;; returns the state
(define update-binding
  (lambda (var val state)
    (cond
      [(eq? (find-box var state) 'none)            (error 'undeclared "Variable not declared.")]
      [else                                        (begin (set-box! (find-box var state) val)
                                                          state)])))

;; find
;; returns the box of the value of variable v
;; returns 'none if the value is not found
;; uses a break continuation to find first value only
(define find-box
  (lambda (var state)
    (call/cc
     (lambda (k)
       (find-box-break var state k)))))

(define find-box-break
  (lambda (var state break)
    (cond
      [(null? state)                                                           'none]
      ; found in first layer
      [(eq? (find-box-layer-break var (caar state) (cadar state) break) 'none)
       (find-box-break var (cdr state) break)]
      ; search deeper layers
      [else 
        (find-box-layer-break var (caar state) (cadar state) break)])))

; s1 is the list of vars
; s2 is the list of values
; finds in the top layer only                 
(define find-box-layer-break
  (lambda (var s1 s2 break)
    (cond
      [(null? s1)           'none] ; variable not found
      [(eq? (car s1) var)   (break (car s2))]
      [else                 (find-box-layer-break var (cdr s1) (cdr s2) break)])))

;; "outer layer" of interpreter
;; goes through global variables and function definitions, adds them to state
#|(define M-state-global
  (lambda (expr state continuations)
    (cond
      [(null? expr)                       state]
      ;; handle global var definitions
      ;; note: M-state-declare calls M-value, so this handles global var = functioncall
      [(eq? (get-keyword expr) 'var)      (M-state-global (cdr expr)
                                                          (M-state-declare (car expr) state continuations)
                                                          continuations)]
      [(eq? (get-keyword expr) 'function) (M-state-global (cdr expr)
                                                          (M-state-function (car expr) state)
                                                          continuations)]
      ;; not allowed on the global level
      [else                               (error 'error "Cannot do this outside a function.")])))
|#

;; get the variable name out of expr for a variable declaration
(define get-var-name cadar)
;; get the method's parameters from its definition
(define get-params caddar)
;; get the method's body from its definition
(define get-method-body (compose car cdddar))

;; returns the list of superclass(es)
(define get-super
  (lambda (cls-expr)
    (if (null? (caddr cls-expr))
        '()
        (cdr (caddr cls-expr)))))

;; gets the body of a class from the entire class definition
(define cls-bod cadddr)
;; gets the name of a class from the entire class definition
(define cls-name cadr)
;;get value of initialized variable in class definition
(define get-var-init-value cddar)

(define M-state-global
  (lambda (expr state)
    (cond
      [(null? expr)                      state]
      [(eq? (get-keyword expr) 'class)   (M-state-global (cdr expr)
                                                         (M-state-class (car expr) state))]
      [else                              (error 'error "Illegal statement outside of a class definition.")])))

;; returns the state after a class closure has been added to it
(define M-state-class
  (lambda (cls-expr state)
    (add-to-state (list (cls-name cls-expr) (make-class-closure cls-expr)) state)))

;; returns the closure for the class represented as a hashmap
(define make-class-closure
  (lambda (cls-expr)
    (make-class-closure-body (cls-bod cls-expr) (cls-name cls-expr) (make-immutable-hash
                                       (list (cons 'super (get-super cls-expr))
                                             (cons 'methods '())
                                             (cons 'const '())
                                             (cons 'inst-vars '())
                                             (cons 'static-vars '())
                                             (cons 'static-vals '()))))))
                                        
         
;; might be broken
;; cls-body?
(define make-class-closure-body 
  (lambda (cls-body class-name closure)
    (cond
      [(null? cls-body) (make-constructor closure)]
      [(and (eq? (get-keyword cls-body) 'var) (eq? (list-length (car cls-body)) 2)) 
       (make-class-closure-body (cdr cls-body) (hash-set closure
                                                         'inst-vars
                                                         (append (hash-ref closure 'inst-vars)
                                                                 (list (get-var-name cls-body)))
                                                         'const
                                                         (append (hash-ref closure 'const)
                                                                 (list 'null))))]
      [(and (eq? (get-keyword cls-body) 'var) (eq? (list-length (car cls-body)) 3)) 
       (make-class-closure-body (cdr cls-body) (hash-set* closure
                                                         'inst-vars
                                                         (append (hash-ref closure 'inst-vars)
                                                                 (list (get-var-name cls-body)))
                                                         'const
                                                         (append (hash-ref closure 'const)
                                                                 (list (get-var-init-value cls-body)))))]
      [(or (eq? (get-keyword cls-body) 'function) (eq? (get-keyword cls-body) 'static-function))
       (make-class-closure-body 
         (cdr cls-body) (hash-set closure
                                 'methods
                                 (append (hash-ref closure 'methods)
                                         (list (make-method-closure (get-method-def-name cls-body)
                                                                    (get-params cls-body)
                                                                    (get-method-body cls-body)
                                                                    get-func-env 
                                                                    class-name)))))]
      [else (error 'error "Improper statement in class definition")])))


;; turns the list of field values currently in 'const into a function that just makes 
;; the list of instance variable values
(define make-constructor
  (lambda (closure)
    (hash-set closure
              'const
              (lambda ()
                (map (lambda (v) (box v)) (hash-ref closure 'const))))))

;; returns a STATE whereas M-value-function returns a VALUE
(define M-state-funcall
  (lambda (expr state continuations)
    (let ((x (M-value expr state continuations))) state)))
    
;; adds function definition to closure
;; TODO: handle nested functions
;; returns a state
(define M-state-function
  (lambda (expr state)
    (add-to-state (list (func-name expr)
                        (make-method-closure '()
                                             (param-list expr)
                                             (func-body expr)
                                             get-func-env
                                             '()))
                  state)))

(define make-method-closure
  (lambda (name params body env class-name)
    (make-immutable-hash
      (list (cons 'name name)
            (cons 'params params)
            (cons 'body body)
            (cons 'env env)
            (cons 'class-name class-name)))))

;; finds the layer with the function name
;; returns that layer of the state and all layers below it
(define get-func-env
  (lambda (name state)
    (cond
      [(null? state)                '()]
      ;; TODO function is not at top of state anymore, need to search class
      [(in-list? name (caar state)) (flatten-state state)]
      [else                         (get-func-env name (cdr state))])))


;; helper for get-func-env
(define in-list?
  (lambda (name lis)
    (cond
      [(null? lis)            #f]
      [(eq? (car lis) name)   #t]
      [else                   (in-list? name (cdr lis))])))
      
;; Flatten existing layers into one layer
; Most recent layer should be first in flattened layer
(define flatten-state
  (lambda (state)
    (flatten-state-acc state (car initial-state))))
(define flatten-state-acc
  (lambda (state acc)
    (cond
      [(null? state)            (cons acc state)]
      [else                     (flatten-state-acc (cdr state)
                                                   (merge-layers acc (car state) (car initial-state)))])))

;; Merge two layers into one -- helper function for flatten-layer
(define merge-layers
  (lambda (layer1 layer2 acc)
    (cond
      [(null? (car layer1)) (list (append (car acc) (car layer2))
                                  (append (cadr acc) (cadr layer2)))]
      [else                 (merge-layers (list (cdar layer1) (cdadr layer1))
                                          layer2
                                          (list (append (car acc) (list (caar layer1)))
                                                (append (cadr acc) (list (caadr layer1)))))])))

;; abstracted macros
(define func-name cadr)
(define param-list caddr)
(define func-body cadddr)

;;calls one of many M-state-** functions depending on nature of input
(define M-state
  (lambda (expr state continuations)
    (cond
      [(null? expr)                         state]
      ;; to handle if (true) or while (true)
      [(eq? (car expr) 'true)               state]
      [(eq? (car expr) 'false)              state]
      [(eq? (get-keyword expr) 'var)        (M-state (cdr expr) 
                                                     (M-state-declare (car expr) 
                                                                      state
                                                                      continuations)
                                                     continuations)]
      
      [(eq? (get-keyword expr) '=)          (M-state (cdr expr)
                                                     (M-state-assign (car expr) 
                                                                     state
                                                                     continuations)
                                                     continuations)]
      ; a "goto" construct return
      [(eq? (get-keyword expr) 'return)     ((hash-ref continuations 'return)
                                               (M-state-return (car expr) 
                                                               state
                                                               continuations))]
      ; a "goto" construct break
      [(eq? (get-keyword expr) 'break)      ((hash-ref continuations 'break))]
      ; a "goto" construct continue
      [(eq? (get-keyword expr) 'continue)   ((hash-ref continuations 'continue))]
      [(eq? (get-keyword expr) 'throw)      ((hash-ref continuations 'throw)
                                               (get-throw-value 
                                                expr state continuations))]
      ; this handles a nested function
      ; TODO We might not want to handle nesteds here
      ; should scan for nested funcs immediately when we enter the outer function
      [(eq? (get-keyword expr) 'function)   (M-state (cdr expr)
                                                     (M-state-function (car expr) state)
                                                     continuations)]

      ; handles function calls that are not assigned to a var value
      [(eq? (get-keyword expr) 'funcall)   (M-state (cdr expr)
                                                    (M-state-funcall (car expr) state continuations)
                                                    continuations)]
                                                             
                                                     
      
      [(eq? (get-keyword expr) 'if)         (M-state (cdr expr) 
                                                     (M-state-if (car expr) 
                                                                 state
                                                                 continuations)
                                                      continuations)]
   
      [(eq? (get-keyword expr) 'while)      (M-state (cdr expr)
                                                     (call/cc
                                                      (lambda (br) ; br parameter: break continuation
                                                        (M-state-while (car expr) 
                                                                       state
                                                                       (hash-set 
                                                                         continuations 
                                                                         'break 
                                                                         (lambda () (br state))))))
                                                      continuations)]
      
      [(eq? (get-keyword expr) 'begin)      (M-state (cdr expr) 
                                                     (M-state-block (car expr) 
                                                                    state
                                                                    continuations)
                                                      continuations)]

      [(eq? (get-keyword expr) 'try)      (M-state (cdr expr) 
                                                     (M-state-trycatchblock 
                                                       (car expr) 
                                                       state
                                                       continuations)
                                                     continuations)]
      [else                                 state])))

; gets the whole state except the last layer for checking 
; whether a varibable was already declared
; The bottom layer is stuff that was declared outside this function, 
; so it can be redefined in the function body and hidden 
(define remove-last-layer
  (lambda (state)
    (remove-last-layer-cps state (lambda (v) v))))

(define remove-last-layer-cps
  (lambda (state return)
    (if (null? (cdr state)) 
      (return '())
      (remove-last-layer-cps (cdr state) 
                             (lambda (v) (return (cons (car state) v)))))))

;; returns the last layer of the state
(define get-last-layer
  (lambda (state)
    (if (null? (cdr state))
        state
        (get-last-layer (cdr state)))))
               


;;checks to see if var is arleady declared in this state
;;For use in M-state-declare
(define var-declared
  (lambda (var state)
    (not (eq? (find-box var state) 'none))))

;;Updates state for a variable declaration
(define M-state-declare
  (lambda (expr state continuations)
    (cond
      ;[(var-declared (declare-var expr) (remove-last-layer state))
      ; (error 'error "Variable already declared!")]
      [(eq? (list-length expr) 3)                  (add-to-state (cons (declare-var expr) 
                                                                       (list (M-value (caddr expr)
                                                                                      state
                                                                                      continuations))) 
                                                                 state)]
      [(not (eq? (list-length expr) 2))            (error 'error "Invalid declare expression.")]
      [else                                        (add-to-state (cons (declare-var expr) '(null))
                                                                 state)])))

;; updates the state in variable assignment
(define M-state-assign
  (lambda (expr state continuations)
    (if (eq? (list-length expr) 3)
        (update-binding (exp1 expr) 
                        (M-value (exp2 expr)
                                 state
                                 continuations)
                        state)
        (error 'error "Invalid assign."))))

;; return does not update state, so just pass control to M-value
;; checks if valid length
(define M-state-return
  (lambda (expr state continuations)
    (if (eq? (list-length expr) 2)
        (M-value expr state continuations)
        (error 'error "Invalid return."))))

(define conditional 
  (lambda (expr) 
      (list (cadr expr))))

(define body 
  (lambda (expr) 
      (list (caddr expr))))

(define rest-if cdddr) ; else if statements
;; update state if function
(define M-state-if
  (lambda (expr state continuations)
    (cond
      [(eq? (M-boolean (car (conditional expr)) 
                       state 
                       continuations)
            #t) 
       (M-state (body expr) 
                (M-state (conditional expr)
                         state 
                         continuations)
                continuations)]
      [(> (list-length expr) 3)                      
       (M-state (rest-if expr) 
                (M-state (conditional expr) state continuations)
                continuations)]
      [else (M-state (conditional expr) 
                     state
                     continuations)])))

(define M-state-trycatchblock
  (lambda (expr state continuations)
    (if (and (null? (catch-block expr)) 
             (null? (finally-block expr)))
      (error 'error "Must have either catch or finally block")
      (M-state-catch 
        expr
            (M-state-try 
              (try-block expr) 
              state 
              (hash-set*
                continuations
                'return
                (lambda (return-statement) 
                  (M-state-finally (finally-expression expr) 
                                   state 
                                   (hash-set* continuations
                                              'do-at-end 
                                              (hash-ref continuations 'return)
                                              'do-at-end-name
                                              (list 'return return-statement))))

                'break
                (lambda (v) 
                  (M-state-finally (finally-expression expr) 
                                   (remove-layer state)
                                   (hash-set* continuations
                                              'do-at-end 
                                              (hash-ref continuations 'break)
                                              'do-at-end-name
                                              'break)))
                'continue
                (lambda (v) 
                  (M-state-finally (finally-expression expr) 
                                   (remove-layer state)
                                   (hash-set* continuations
                                              'do-at-end 
                                              (hash-ref continuations 'continue)
                                              'do-at-end-name
                                              'continue)))))
        (hash-set* 
          continuations
          'return
          (lambda (return-statement) 
            (M-state-finally (finally-expression expr) 
                             state 
                             (hash-set* continuations
                                        'do-at-end 
                                        (hash-ref continuations 'return)
                                        'do-at-end-name
                                        (list 'return return-statement))))
          'break
          (lambda (v) 
            (M-state-finally (finally-expression expr) 
                             (remove-layer state)
                             (hash-set* continuations
                                        'do-at-end 
                                        (hash-ref continuations 'break)
                                        'do-at-end-name
                                        'break)))
          'continue
          (lambda (v) 
            (M-state-finally (finally-expression expr) 
                             (remove-layer state)
                             (hash-set* continuations
                                        'do-at-end 
                                        (hash-ref continuations 'continue)
                                        'do-at-end-name
                                        'continue)))
          'throw
          (lambda (throw-statement)
            (M-state-finally (finally-expression expr) 
                             (remove-layer state)
                             (hash-set* continuations
                                        'do-at-end 
                                        (hash-ref continuations 'throw)
                                        'do-at-end-name
                                        (list 'throw throw-statement)))))))))

(define M-state-try
  (lambda (expr state continuations)
    (call/cc
      (lambda (new-throw)
        (M-state-trycatchexpr (cons 'begin expr) 
                              state 
                              (hash-set continuations
                                        'throw
                                        (lambda (throw-value) 
                                          (new-throw (list throw-value
                                                           state)))))))))

(define get-caught-var 
  (lambda (expr) 
    (caadr (catch-block expr))))

(define catch-expression
  (lambda (expr)
    (caddr (catch-block expr))))

(define finally-expression
  (lambda (expr)
    (if (null? (finally-block expr))
      '()
      (cadr (finally-block expr)))))

(define M-state-catch
  (lambda (expr state continuations)
    (cond
      ;;if there was no catch block
      [(null? (catch-block expr)) (M-state-finally (finally-expression expr)
                                                   state
                                                   (hash-set* continuations
                                                              'do-at-end 
                                                              (lambda (v) v)
                                                              'do-at-end-name
                                                              'null))]
      ;;This is the case where we didn't throw an exception:
      ;;If we threw an exception, state will be input from the call/cc
      ;;in which case its car is the first argument to catch, which isn't a list
      ;;so this case is the same as if we didn't have a catch block
       [(list? (car state)) (M-state-finally (finally-expression expr)
                                            state
                                            (hash-set* continuations
                                                       'do-at-end 
                                                       (lambda (v) v)
                                                       'do-at-end-name
                                                       'null))] 
      ;;if we did throw an exception
      [else (M-state-finally (finally-expression expr)
                             (M-state-catch-recurse (catch-expression expr)
                                                    (add-to-state (cons (get-caught-var expr)
                                                                        (list (car state)))
                                                                  (cadr state))
                                                                  ;(remove-layer (cadr state)))
                                                    continuations)
                             (hash-set* continuations
                                        'do-at-end
                                        (lambda (v) v)
                                        'do-at-end-name
                                        'null))])))

(define M-state-catch-recurse
  (lambda (expr state continuations)
    (M-state-trycatchexpr (cons 'begin expr) state continuations)))

(define M-state-finally-recurse
  (lambda (expr state continuations)
    (cond 
      [(not (null? expr)) (M-state-finally-recurse (cdr expr) 
                                                   (M-state (list (car expr))
                                                            state 
                                                            continuations)
                                                   continuations)]
      [(eq? (hash-ref continuations 'do-at-end-name) 'null) 
       ((hash-ref continuations 'do-at-end) state)]
      ;;if do-at-end-name is a list, then this is a return or a throw
      [(and (list? (hash-ref continuations 'do-at-end-name))
            (eq? (car (hash-ref continuations 'do-at-end-name))
                 'return))
       ((hash-ref continuations 'do-at-end) 
        (M-state-return (list 'return (cadr (hash-ref continuations 'do-at-end-name))) 
                                                            state 
                                                            continuations))]
      [(and (list? (hash-ref continuations 'do-at-end-name))
            (eq? (car (hash-ref continuations 'do-at-end-name))
                 'throw))
       ((hash-ref continuations 'do-at-end) (cadr (hash-ref continuations 'do-at-end-name)))]
      [(eq? (hash-ref continuations 'do-at-end-name) 'continue) ((hash-ref continuations 'do-at-end) state)]
      [(eq? (hash-ref continuations 'do-at-end-name) 'break) ((hash-ref continuations 'do-at-end) state)])))

(define M-state-finally
  (lambda (expr state continuations)
    (remove-layer (M-state-finally-recurse
                    expr (push-layer state) continuations))))
         
;;Like M-state-block, but we don't overwrite the continue continuation.
;;Used for what would be the 'block' inside try and catch
(define M-state-trycatchexpr
  (lambda (expr state continuations)
    (remove-layer
          (M-state (block-body expr)
                   (push-layer state)
                   continuations))))

(define try-block cadr)
(define catch-block caddr)
(define finally-block cadddr)

;; update state while loop
(define M-state-while
  (lambda (expr state continuations)
    (if (eq? (M-boolean (car (conditional expr)) state continuations) #t)
        (M-state-while expr 
                       (M-state (body expr) 
                                (M-state (conditional expr) state continuations)
                                continuations)
                       continuations)
        (M-state (conditional expr) state continuations))))
 
(define block-body cdr)

;; state with blocks - first add a new layer, then remove it
(define M-state-block
  (lambda (expr state continuations)
    (remove-layer
      (call/cc 
        (lambda (cont) ; continuation for continue
          (M-state (block-body expr)
                   (push-layer state)
                   (hash-set continuations
                             'continue
                             (lambda () (cont (push-layer state))))))))))
  
;; Adds a new layer to top of state
(define push-layer
  (lambda (state)
    (cons empty-layer state)))

(define remove-layer cdr)




; ----- Macros -----
(define empty-layer '(()()))

(define get-operator car)
(define exp1 cadr)
(define exp2 caddr)

(define get-keyword caar)

(define get-throw-value 
  (lambda (expr state continuations)
    (M-value (cadar expr) state continuations)))

; M-state-declare
(define declare-var cadr)

(define (atom? x) (not (or (pair? x) (null? x))))

; Provide the interpret function for rackunit
(provide interpret interpret)
(interpret "test" 'A)
;(M-state-global (parser "test") initial-state)
;(hash-ref (get-var-value (M-state-global (parser "test") initial-state) 'A) 'methods)
;(get-body-class 'main 'A (M-state-global (parser "test") initial-state))