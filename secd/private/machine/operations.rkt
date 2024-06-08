#lang racket
(require "secd-vm-defs.rkt"
         "stack.rkt")
(provide push! pop! prim-type-pred-op)

(define push!-tmpl (make-op 'push! (lambda (the-stack value)
                                     (the-stack 'push! value)) (list) 0 1 1))
(define pop!-tmpl (make-op 'pop! (lambda (the-stack)
                                   (the-stack 'pop!)) (list) 0 0 0))
 
(define push! (lambda (in-values)
                     (make-op (op-code push!-tmpl)
                              (op-operation push!-tmpl)
                              in-values
                              (op-stack-arity push!-tmpl)
                              (op-stack-out push!-tmpl)
                              (op-parm-arity push!-tmpl)
                              )
                     ))

(define pop! (lambda ()
                    (make-op (op-code pop!-tmpl)
                             (op-operation pop!-tmpl)
                             (op-params pop!-tmpl)
                             (op-stack-arity pop!-tmpl)
                             (op-stack-out pop!-tmpl)
                             (op-parm-arity pop!-tmpl)
                             )))


(define prim-type-pred-op
  (lambda (predicate)
    (make-op 'nothing-to-set predicate 
                                 'place-holder 0 0 0)
    ))