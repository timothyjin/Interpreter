#lang racket

(define M_state
  (lambda (variable value state)
    (

(define interpret
  (lambda (lis, state)
    (cond
      [(null? lis) state]
      [(eq? (caar lis) 'var) (declare (car lis))]
      [(eq? (caar lis) '=) (assign (car lis))]
      [(eq? (caar lis) 'return) (return (car lis))]
      [(eq? (caar lis) 'if) (evaluate-if (car lis))]
      [(eq? (caar lis) 'while) (evaluate-while (car lis))])))

(define declare
  (lambda (exp)
    
                          
                         
    