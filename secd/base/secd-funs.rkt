#lang racket

(provide get-fun fun? get-var set!-var )

;; hre the definition of the symbols follow something like secd-lambda is e.g.: a make-abst expression
;; import secd-vm-defs .....

(define secd-funs
  (hasheq 'lambda make-abst
          'define secd-define
          'apply-fun secd-apply-fun
          'where-cond secd-where-cond
          'cond-branch secd-cond-branch
          'is? secd-is?))

(define vars (make-hash))

(define (fun? key)
  (hash-has-key? funs key))
  
(define (var? key)
  (hash-has-key? vars key))

(define (get-fun key)
  (if (fun? key)
      (hash-ref funs key)
      (error "fun: no such function. " key)))
  
(define (get-var key)
  (if (var? key)
      (hash-ref vars key)
      (error "var: no such variable. " key)))

(define (set!-var key val)
  (hash-set! vars key val))
