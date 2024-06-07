#lang racket
 (require  "secd-vm-defs.rkt"
           "thread-chan-sem-defs.rkt"
           "secd-dbg-print-it.rkt"
  "stack.rkt"
  "operations.rkt")

(provide
 debug-snap-stack
 debug-snap-env
 debug-snap-frame
 debug-snap-dump
 debug-to-print-cl
 debug-to-print-app-fun
 debug-to-print-abst
 write-object-nl
 debug-to-print-secd
 print-stack-env-code
 debug-snap-heap
 debug-compile-term
 write-newline
 write-string
 print-out-code
 )


(define  write-newline
  (lambda ()
  ""))

(define  write-string
  (lambda (str)
    (put str)))

(define debug-snap-stack
  (lambda (op-stack   message)
    (begin
      (write-object-nl message)
      (op-stack 'print-stack)
      )))

(define debug-snap-env
  (lambda (act-env message)
    (begin
      (write-object-nl message)
      (write-newline)          
      (put "------ Begin Actual Env --------------")
      (write-newline)
      (if (not (empty? act-env))      
          (for-each (lambda (bind)
                      (begin
                        (write-newline)
                        (put "------ Begin Binding --------------")
                        (write-newline)
                        (put (binding-variable bind))
                        (write-newline)
                        (put (binding-value bind))
                        (write-newline)
                        (put "------ End Binding --------------")
                        (write-newline)))
                    act-env)
          (write-newline))
      (put "------ End Actual Env --------------")
      (write-newline)
      )))

(define debug-snap-heap
  (lambda (the-heap-rec message)
    (let ([the-heap (heap-storage the-heap-rec)])
    (begin
      (write-object-nl message)
      (write-newline)          
      (print "------ Begin The Heap --------------")
      (write-newline)
      (if (not (empty? the-heap))      
          (for-each (lambda (bind)
                      (begin
                        (write-newline)
                        (print "------ Begin Heap Cell --------------")
                        (write-newline)
                        (print (heap-cell-variable bind))
                        (write-newline)
                        (print (heap-cell-init-value bind))
                        (write-newline)
                        (print "------ End Heap Cell --------------")
                        (write-newline)))
                    the-heap)
                    (write-newline))
      (print "------ End The Heap --------------")
      (write-newline)
      ))))

(define debug-snap-frame
  (lambda (frame message)
    (begin
      (write-object-nl message)
    
      (write-object-nl "------ Begin Frame Stack --------------")
      (debug-snap-stack (frame-stack frame) message)
      (write-object-nl "------ End Frame Stack --------------") 
     
      (write-object-nl"------ Begin Frame Code --------------")
      (write-object-nl (frame-code frame))
      (write-object-nl "------ End Frame Code --------------")  
     
      (write-object-nl "------ Begin Frame Environment --------------")
      (debug-snap-env (frame-environment frame) message)
      (write-object-nl "------ End Frame Environment --------------")
     
      )))

(define debug-snap-dump
  (lambda (dump message)
    (begin
      (write-object-nl message)
      (if (not (empty? dump))      
          (for-each (lambda (frame)
                      (debug-snap-frame frame message))
                    dump)
          (write-newline)
          ))))


(define debug-compile-term
  (lambda (code)
    (begin
    (write-object-nl "The code:")
    (if (cons? code)
        (begin
          (write-object-nl code)
          
        (write-object-nl (first code)))
    (write-object-nl code)))))
    
(define debug-to-print-cl
  (lambda (cl message)

    
    (begin
      (write-object-nl message)
      (write-newline)
      (write-object-nl "------ Begin Function Closure --------------")
      
      (write-object-nl (closure-variable cl))
      
      (write-object-nl (closure-code cl))
     
      (debug-snap-env (closure-environment cl) message)
      
      (write-object-nl "-------- End Function Closure --------------")
    
      )))

(define debug-to-print-app-fun
  (lambda (app-fun-rec message)
    (begin
      (write-object-nl message)
     
      )))



(define debug-to-print-secd
  (lambda (secd-rec message)
    (begin
      (write-object-nl message)
      (write-object-nl  "------------ Begin SECD --------------------")
      (write-object-nl  "------------ Begin SECD OP STACK  --------------------")
      (debug-snap-stack (secd-stack secd-rec) message)
      (write-object-nl  "------------ End SECD OP STACK  --------------------")

      (write-object-nl  "------------ Begin SECD FUN STACK  --------------------")
      (debug-snap-stack (secd-fun-stack secd-rec) message)
      (write-object-nl  "------------ End SECD FUN STACK  --------------------")
    
      (debug-snap-stack (secd-stack secd-rec) message)
      (write-object-nl  "------------ Begin SECD ENV --------------------")
      (debug-snap-env (secd-environment secd-rec) message)
      (write-object-nl  "------------ End SECD ENV --------------------")
      (write-object-nl  "------------ Begin SECD  CODE--------------------")
      (print (secd-code secd-rec))
      (write-object-nl  "------------ End SECD CODE--------------------")
      (debug-snap-dump (secd-dump secd-rec) message)
      (write-object-nl  "------------Begin SECD Heap--------------------")
      (debug-snap-heap (secd-heap secd-rec) message)
       (write-object-nl  "------------End SECD Heap--------------------")
        (write-object-nl  "------------End SECD -------------------------")
      )))

(define write-object-nl
  (lambda (message)
    (begin
      (put message)
      )))

(define print-stack-env-code
  (lambda (op-stack env code message)
    (begin
     (write-object-nl  "------------ Begin ACT OP STACK  --------------------")
      (debug-snap-stack op-stack message)
      (write-object-nl  "------------ End ACT OP STACK  --------------------")
      (write-object-nl  "------------ Begin ACT ENV --------------------")
      (debug-snap-env env message)
      (write-object-nl  "------------ End ACT ENV --------------------")
      (write-object-nl  "------------ Begin ACT CODE --------------------")
      (write-object-nl  code)
      (write-object-nl  "------------ End ACT CODE --------------------")
      )))


;; AST-Printer
(define debug-to-print-ast-code
  (lambda (ast-rec)
    (print-out-code (ast-code ast-rec))
    ))

(define print-out-code
  (lambda (code)
    (if (empty? (rest code))
        'return
        (let ([next-code (rest code)])
    (cond
      ((empty? code)
       'ready)
      ((define-def? (first code))
       (debug-to-print-define-def (first code))
       (print-out-code next-code)
      )
      ((abst? (first code))
       (debug-to-print-abst (first code))
       (print-out-code next-code)
            )
      ((conditions? (first code))
       (debug-to-print-conds (first code))
       (print-out-code next-code))

       ((single-cond? (first code))
       (debug-to-print-single-cond (first code))
       (print-out-code next-code))

        ((op? (first code))
       (debug-to-print-op (first code))
       (print-out-code next-code))

        ((where? (first code))
         (debug-to-print-where (first code))
         (print-out-code next-code))
        
        ((code-block? (first code))
         (print-out-code (code-block-code (first code)))
         (print-out-code next-code))

        ((heap-alloc? (first code))
         (write-object-nl 'heap-alloc)
          (print-out-code next-code))

         ((heap-free? (first code))
          (write-object-nl 'heap-free)
          (print-out-code next-code))

          ((heap-set-at!? (first code))
           (write-object-nl 'heap-set-at!)
          (print-out-code next-code))
         
          ((heap-get-at? (first code))
           (write-object-nl 'heap-get-at)
          (print-out-code next-code))

          ((ap? (first code))
           (write-object-nl 'ap)
           (print-out-code next-code))

           ((tailap? (first code))
           (write-object-nl 'tailap)
           (print-out-code next-code))
         
                 
      
      )))))

    (define debug-to-print-where
      (lambda (where-rec)
        (print "Begin Cond - Branch")
        (print-out-code
         (where-condition where-rec))
         (print-out-code (where-if-branch where-rec))
        (print-out-code (where-else-branch where-rec))
        ))

(define debug-to-print-op
  (lambda (op-rec)
    (write-newline)
    (print "----------- Op - Start --------------")
    (write-newline)
  (print  (op-code op-rec))
     (write-newline)
   (print (op-operation op-rec))
     (write-newline)
   (print (op-params op-rec))
     (write-newline)
   (print (op-stack-arity op-rec))
     (write-newline)
   (print (op-stack-out op-rec))
       (write-newline)
   (print (op-parm-arity op-rec))

    (write-newline)
    (print "---------- Op - End ---------- ")))

(define debug-to-print-conds
  (lambda (conds-rec)
     (print "------ Begin conditions --------------")
    (write-newline)
    (conditions-conds conds-rec)
    (print-out-code (conditions-conds conds-rec))
    (print "------ En conditions --------------")))

(define debug-to-print-single-cond
  (lambda (cond-rec)
     (print "------ Begin conditions --------------")
    (write-newline)
   (print-out-code (single-cond-what  cond-rec))
   (print-out-code (single-cond-code cond-rec))
    (print "------ En conditions --------------")))
    
(define debug-to-print-define-def
  (lambda (define-def-rec)
    (print "------ Begin define--------------")
    (write-newline)
    (print (define-def-bind define-def-rec))    
    (write-newline)
    (print-out-code (define-def-value define-def-rec))
     (print "------ End define Abstraction--------------")
    
     ))
(define debug-to-print-abst
  (lambda (abst-rec)
    (begin
      (print "------ Begin Abstraction--------------")
      (write-newline)
      (print (abst-variable abst-rec))
      (write-newline)
      (print-out-code  (abst-code abst-rec))
      (write-newline)
      (print "------ End Abstraction --------------")
      (write-newline)
      )))
