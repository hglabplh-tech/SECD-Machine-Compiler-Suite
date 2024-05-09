#lang racket
(require 
  "secd-vm-defs.rkt"
  "secd-compiler.rkt"
  "stack.rkt"
  "operations.rkt"
  "debug-out.rkt")

(provide eval-secd debug-channel debug-on)

;;TODO :: make in all places like code-block and conditions real closure inits like in app-fun
;; enable break functionality
(define temp-stack (make-stack (list)))



(define process (lambda (ast)
                  (let* ([op-stack (ast-stack ast)]
                         [code (ast-code ast)]
                         [env (ast-env ast )]
                         [dump (ast-dump ast)]
                         [act-frame (first dump)]
                         [state (make-secd (make-stack (list) ) (make-stack (list)) env code (list) (make-heap (list)))])
                    (begin
                      
                      (secd-step* state)))))


(define secd-step*
  (lambda (state)
    (if (and (empty? (secd-code state))
             (empty? (secd-dump state)))
        state
        (begin           
        (let ([secd-rec (really-process state)])
          (secd-step* secd-rec)
          (debug-step secd-rec)          
          )))))

(define secd-exec-prim
  (lambda (state)
    (if  (empty? (secd-code state))             
         (let ([secd-ret (make-secd
          (secd-stack state)
          (secd-fun-stack state)
          (secd-environment state)
          (secd-code state)
          (secd-dump state)
          (secd-heap state))])
           (debug-step secd-ret)
           secd-ret
           )       
           (let ([secd-rec (really-process state)])
             (debug-step secd-rec)
             (secd-exec-prim secd-rec)
             (debug-step secd-rec)
             secd-rec))))


(define call-abstract-pop! (lambda (term op-stack)
                             (if (abst? (first term))
                                 (begin
                                   (op-stack 'pop!)
                                   (abst-code (first term)))
                                 term
                                 )
                             ))
(define really-process (lambda (state)
                         (define op-stack (secd-stack state))
                         (define fun-stack (secd-fun-stack state))
                         (define env (secd-environment state))
                         (define code (secd-code state))
                         (define dump (secd-dump state))
                         (define heap-inst (secd-heap state))
                         (begin
                           (if (not (empty? code))
                               (begin
                                 (cond

                                   ;; Die Definition einer Abstraktion:
                                   ;; Es wird eine Closure definiert
                                   ;; Operations Stack: Keine Änderung
                                   ;; Funktions Stack: davor ->NONE danach -> Closure aus der Abstraktion
                                   ;; Environment: Keine Änderung
                                   ;; Heap: Keine Änderung
                                   ((abst? (first code))
                                    (begin
                                      (print-stack-env-code op-stack fun-stack env code  "debug call abstraction")
                                      (let* ([param-var  (abst-variable (first code))]                                     
                                              
                                             [the-closure  (make-closure
                                                            param-var
                                                            (abst-code (first code))                                                                           
                                                            env)])
                                        (begin
                                                                          
                                          (fun-stack 'push! the-closure)
                                          (fun-stack 'print-stack)                               
                                          (op-stack 'print-stack)                               
                                   
                                          (make-secd
                                           op-stack
                                           fun-stack
                                           env
                                           (rest code)
                                           dump
                                           heap-inst)))))
                                          
                                  
                                  

                                   ;; Eine Operation wird ausgeführt ... bis jetzt Stack Operationen
                                   ;; 1. Push Konstante
                                   ;;        Operations Stack: davor NONE danach Konstante auf dem Stack
                                   ;; 2. Push Binding Value (Unterschied ? Das Environment wird abgefragt
                                   ;;    der Wert wird geholt
                                   ;;         Operations Stack: davor NONE danach Binding Value auf dem Stack
                                   ;; Funktions Stack:Keine Änderung
                                   ;; Environment: Keine Änderung
                                   ;; Heap: Keine Änderung
                                   ((op?  (first code))                     
                                    (let ([val (first (op-params (first code)))])
                                      (begin
                                        (cond ((base? val)
                                               (begin (write-newline)(write-string "get constant")
                                                      (write-newline)
                                                      (write-object-nl  val)
                                                      (write-string "push constant par: ")
                                                      (write-object-nl val)
                                                      (write-newline)
                                                      ((op-operation (first code)) op-stack (new-stack-element 'app val))       ;; NEW e have the proceedure
                                                      ;; here it is a 'push!
                                                      ) )
                                        
                                              ((var-symbol? val) ;; do the right order of income
                                               (begin
                                                 (write-newline)
                                                 (write-string "get var: ")
                                       
                                                 (write-string " push variable par: ")
                                                 (write-object-nl val)
                                                 (write-newline)
                                                 (let ([bound-value  (lookup-environment
                                                                      env
                                                                      dump
                                                                      val)])
                                          
                                                   (cond
                                                     ((number? bound-value)
                                                      (begin
                                                        (write-string (number->string bound-value))
                                                        ((op-operation (first code))  op-stack  (new-stack-element 'app
                                                                                                                   bound-value))))
                                                     (else
                                                      ((op-operation (first code))  op-stack  (new-stack-element 'app
                                                                                                                 val)))
                                                     )))))
                                        (make-secd
                                         op-stack
                                         fun-stack
                                         env
                                         (rest code)
                                         dump
                                         heap-inst)
                                        )))


                                   ;; Hier wird in die Konditionen (where-cond) verzweigt
                                   ;; Operations Stack: Keine Änderung
                                   ;; Funktions Stack: Keine Änderung
                                   ;; Environment: Keine Änderung
                                   ;; Heap: Keine Änderung
                                   ((conditions? (first code))
                                    (begin
                                      (write-object-nl "enter conditions -->")
                                      (write-object-nl (conditions-conds  (first code)))
                                      (make-secd
                                       op-stack
                                       fun-stack
                                       env
                                       (conditions-conds  (first code))                                  
                                       (cons
                                        (make-frame op-stack fun-stack env (rest code))
                                        dump)                                   
                                       heap-inst)))

                                   ;; Hier wird eine Kondition abgefragt und bei zutreffen verzweigt -> siehe 'do-is'
                                   ((single-cond? (first code))
                                    (begin
                                      (write-object-nl "enter single cond case -->")
                                      (do-is  (first code) state)
                                      ))

                                   ((break? (first code))
                                    (let* ([ frame (first dump) ]       
                                           [ret-stack (frame-stack frame)])
                                      (begin
                                        (cond
                                          ((> (op-stack 'size)  0)
                                           (op-stack 'add-all! ret-stack) 
                                           )
                                          (else 'do-nothing
                                                ))
                                        (make-secd
                                         (remove-params-from-stack op-stack (make-stack (list)))
                                         (frame-fun-stack (first dump))
                                         (frame-environment (first dump))
                                         (frame-code (first dump))
                                         (rest dump)
                                         heap-inst
                                         )
                                        )))
                                    
                             
                                   ((code-block? (first code))                                    
                                    (closure-apply state (code-block-code (first code)))                                      
                                    )
                             
                                   ((base? (first code))
                                    (begin
                        
                                      (op-stack 'push!(new-stack-element 'app (first code))) 
                                      (make-secd
                                       op-stack
                                       fun-stack
                                       env
                                       (rest code)
                                       dump
                                       heap-inst)
                                      ))

                                   ((heap-alloc? (first code))
                                    (let* ([the-value  (stack-element-value (op-stack 'pop!))]
                                           [identifier (stack-element-value (op-stack 'pop!))]
                                           [act-value (lookup-heap-stor  heap-inst  identifier)])
                                      (begin
                                        (if (empty? act-value)
                                            (let ([new-heap-inst (extend-heap-stor heap-inst identifier the-value) ])
                                              (begin
                                                ;;(op-stack 'push! (new-stack-element 'app the-value))
                                                (debug-snap-heap heap-inst "print heap in ALLOC")
                                                (make-secd
                                                 op-stack
                                                 fun-stack
                                                 env
                                                 (rest code)
                                                 dump
                                                 new-heap-inst)
                                                ))
                                            (make-secd
                                             op-stack
                                             fun-stack
                                             env
                                             (rest code)
                                             dump
                                             heap-inst)
                                            ))                              
                                      ))

                                   ((heap-free? (first code))                                
                                    (let* (
                                           [identifier (stack-element-value (op-stack 'pop!))]
                                           [heap-inst-new (free-heap-stor-cell  heap-inst  identifier)])
                                      (begin
                                        (debug-snap-heap heap-inst "print heap in free")
                                        ;; (op-stack 'push! (new-stack-element 'app 'DONE))
                                        (make-secd
                                         op-stack
                                         fun-stack
                                         env
                                         (rest code)
                                         dump
                                         heap-inst-new)
                                        )                                                
                                      ))
                                  
                                   ((heap-set-at!? (first code))
                                    (let* ([the-value  (stack-element-value (op-stack 'pop!))]
                                           [identifier (stack-element-value (op-stack 'pop!))]
                                           [act-value (lookup-heap-stor  heap-inst  identifier)])
                                      (begin
                                        (if (not (empty? act-value))                                       
                                            (let ([heap-inst-new  (extend-heap-stor heap-inst identifier the-value)])
                                              (begin
                                                (debug-snap-heap heap-inst "print heap in SET")
                                     
                                                (extend-heap-stor heap-inst identifier the-value)
                                                ;; (op-stack 'push! (new-stack-element 'app the-value)) 
                                                (make-secd
                                                 op-stack
                                                 fun-stack
                                                 env
                                                 (rest code)
                                                 dump
                                                 heap-inst-new)))
                                            (make-secd
                                             op-stack
                                             fun-stack
                                             env
                                             (rest code)
                                             dump
                                             heap-inst)
                                            ))))

                                   ((heap-get-at? (first code))                                
                                    (let* (
                                           [identifier (stack-element-value (op-stack 'pop!))]
                                           [act-value (lookup-heap-stor  heap-inst  identifier)])
                                      (begin
                                        (debug-snap-heap heap-inst "print heap in get")
                                        (op-stack 'push! (new-stack-element 'app act-value))
                                        (make-secd
                                         op-stack
                                         fun-stack
                                         env
                                         (rest code)
                                         dump
                                         heap-inst)
                                        )                                                
                                      ))
                                     
                                   ((where? (first code))
                                    (let*  (

                                            [fresh-stack (make-stack (list))]
                                            [dummy    ( cond
                                                         ((> (op-stack 'size) 0)
                                                          (fresh-stack 'push! (op-stack 'peekit))
                                                          )
                                                         (else (make-stack (list)))
                                                         )]
                                            [cond-secd (make-secd
                                                        fresh-stack
                                                        fun-stack
                                                        env
                                                        (where-condition (first code))
                                                        dump
                                                        heap-inst)])
                                      (begin
                                     (let* ([res-secd  (secd-exec-prim cond-secd)]
                                               [res-op-stack (secd-stack res-secd)]
                                               [res-fun-stack (secd-fun-stack res-secd)]
                                               [res-env (secd-environment res-secd)]
                                               [res-dump (secd-dump res-secd)]                                  
                                               [dummy (op-stack 'push! (res-op-stack 'peekit))]
                                               [cond-result (stack-element-value (op-stack 'pop!))]
                                               [the-code (if  cond-result
                                                              (where-if-branch (first code))
                                                              (where-else-branch (first code)))]
                                               [the-closure   (make-closure 'blubb
                                                                            the-code
                                                                            env)])
                                     
                                          (make-secd  op-stack
                                                      fun-stack
                                                      (closure-environment the-closure)                                                                                  
                                                      (closure-code the-closure)
                                                      (cons
                                                       ;;  (first stack))
                                                       (make-frame op-stack fun-stack env (rest code))
                                                       dump)
                                                      heap-inst)
                                          ) ) ))
                                     
                                   

                                   ((prim? (first code))                    
                                    (begin
                                      (print-stack-env-code op-stack fun-stack env (rest code) "prim? enter op stack--> ")                           
                                      (write-object-nl (list "OP - CODE:"(prim-operator (first code))))
                                      (let ([second-element  (stack-element-value (op-stack 'pop!))]
                                            [first-element (stack-element-value (op-stack 'pop!))])                                   
                                        (op-stack 'push! (new-stack-element 'app
                                                                            (apply-primitive (prim-operator (first code))
                                                                                             (list first-element                        
                                                                                                   second-element))))
                                        )

                                      (let ([secd-ret  (make-secd
                                                        op-stack
                                                        fun-stack
                                                        env
                                                        (rest code)
                                                        dump
                                                        heap-inst)])
                                        (begin
                                          (print-stack-env-code op-stack fun-stack env (rest code) "state given by prim? --> ")
                                          secd-ret)
                                        )))

                                   ((define-def? (first code))
                                    (begin
                                      (write-newline)
                                      (write-string "try to put to env: ")
                                      (write-string (symbol->string (define-def-bind (first code))))
                                      (write-newline)                               
                                      (let* ([the-binding (define-def-bind (first code))]
                                             [the-value (define-def-value (first code))]                                     
                                             [extended-env (extend-environment
                                                            env
                                                            the-binding                                                          
                                                            the-value)])
                               
                                        (make-secd
                                         op-stack
                                         fun-stack
                                         extended-env
                                         (rest code)
                                         dump
                                         heap-inst
                                         ))))                 
                                  
                               

                                   ((app-fun? (first code))                             
                                    (process-fun-app (first code) state
                                                     )
                                    )
                             
                                   ((ap? (first code))
                                    (begin
                                      (debug-snap-stack op-stack "Operating Stack")
                                      (debug-snap-stack fun-stack "Function Stack")
                                      (let ([parameter-binding (stack-element-value (op-stack 'pop!))]                                   
                                            [the-closure (fun-stack  'pop!)])
                                      
                                    
                              
                                        (begin
                                          (print-stack-env-code op-stack fun-stack env code  "debug the ap?")
                                          (write-object-nl "Enter application ->")
                                 
                              
                                    
                                 
                                    
                                          (op-stack 'print-stack)                        
                                          (write-newline )                              
                                          (write-string "Parameter Binding: ")
                                          (write-string (number->string parameter-binding))
                                          (write-newline)

                                          (let ([extended-env 
                                                 (extend-environment
                                                  (closure-environment the-closure)
                                                  (closure-variable the-closure)
                                                  parameter-binding)
                                                 ]
                                                [fresh-stack (make-stack (list))])
                                            (begin
                                              ( cond
                                                 ((> (op-stack 'size) 0)
                                                  (fresh-stack 'push! (op-stack 'peekit)) ;;;peekit before that
                                                  )
                                                 (else 'do-nothing)
                                                 )
                                              (make-secd   op-stack
                                                           (make-stack  (list))         
                                                           extended-env                                   
                                                           (closure-code the-closure)
                                                           (cons
                                                            ;;  (first stack))
                                                            (make-frame op-stack fun-stack env (rest code))
                                                            dump)
                                                           heap-inst)
                                              ) ) ))))
                   
                                   ((tailap? (first code)) ;; nach stack und environment sehen
                                    (let ([closure (if (> (fun-stack 'size) 0)
                                                       (fun-stack 'pop!)
                                                       (make-closure
                                                        'NONE
                                                        (rest code)
                                                        env))])
                                      (begin
                                        (make-secd op-stack
                                                   fun-stack
                                                   (extend-environment
                                                    (closure-environment closure)
                                                    (closure-variable closure)
                                                    (op-stack 'pop!))    
                                                   (closure-code closure)
                                                   dump
                                                   heap-inst))))              
                                   (else
                                    (leave-context op-stack fun-stack dump heap-inst)
                                    )))
                               (leave-context op-stack fun-stack dump heap-inst)))))

(define leave-context
  (lambda (op-stack fun-stack dump heap-inst)
    (let* ([ frame (first dump) ]       
           [ret-stack (frame-stack frame)]
           ;;  [ret-stack (op-stack 'add-all! ret-stack)]
           )
      (begin
        (write-object-nl "=============================== Begin fun shutdown =====================")
        (debug-snap-stack op-stack "Actual Stack")
        ;; (debug-snap-env env "Actual Environment")
        (write-object-nl "=============================== End fun shutdown =====================")
                                  
        -(write-object-nl "=============================== FINISH =====================")
        (write-object-nl "========THE FRAME FROM DUMP=====================")
        (debug-snap-frame frame "code is empty get from dump")
        (write-object-nl "========END THE FRAME FROM DUMP=====================")
        (write-object-nl "=============================== FINISH =====================")
                                  
        (begin
                                      
                               
          (cond
            ((> (op-stack 'size)  0)
             (op-stack 'add-all! ret-stack) 
             )
            (else 'do-nothing
                  ))
         
                                
          (make-secd
           (remove-params-from-stack op-stack (make-stack (list)))
           ;;ret-stack ;; ATTENT
           (frame-fun-stack frame)
           (frame-environment frame)
           (frame-code frame)
           (rest dump)
           heap-inst)
          ))
      )))

(define remove-params-from-stack
  (lambda (old-stack fresh-stack)
  
   
    (if (>  (old-stack 'size)0)
        (let
            ([act-value (old-stack 'pop!)]
             )
          (begin
            (if (equal? 'app (stack-element-type act-value))
                (fresh-stack 'push! act-value)
                'nothing )
            ( cond
               ((> (old-stack 'size) 0)
                (remove-params-from-stack old-stack fresh-stack)
                )
               (else
                (begin
                  (fresh-stack 'reverse!)                  
                  ))
               )
      
            ))
        (make-stack (list)
                    ))))
(define def-closure
  (lambda (env dump op-stack  fun-stack fun-ref)
    (let* ( [the-abst-code  (lookup-environment
                             env
                             dump
                             fun-ref)])
      (if (or (empty? the-abst-code) (not (abst? the-abst-code)))
          (fun-stack 'pop!)
          (begin
            (op-stack 'pop!)
            (make-closure
             (abst-variable (first  the-abst-code))
             (abst-code (first  the-abst-code))                                                              
             env)                           
            )))))


(define process-fun-app
  (lambda (app-fun-rec secd-state)
    (define code (secd-code secd-state))
    (define env (secd-environment  secd-state))
    (define op-stack (secd-stack secd-state))
    (define fun-stack (secd-fun-stack secd-state))
    (define dump (secd-dump secd-state))
    (define heap-inst (secd-heap secd-state))
    (begin
      (write-object-nl  "Enter app fun ->")
      (debug-snap-env env "start processing")
      (debug-snap-stack op-stack "start processing OP_STACK")
      (debug-snap-stack fun-stack "start processing FUN_STACK")

      (let* ( [fresh-stack (make-stack (list))]
              [dummy    ( cond
                           ((> (op-stack 'size) 0)
                            (fresh-stack 'push! (op-stack 'peekit))
                            )
                           (else (make-stack (list)))
                           )]
              [temp-secd (make-secd
                          op-stack
                          fun-stack
                          env
                          (app-fun-args-code (first code))
                          dump
                          heap-inst)])
        
             
       
        (let* ( [secd-new (secd-exec-prim temp-secd)]
                [reverted-result-stack ((secd-stack secd-new) 'reverse)]                                  
                [identifier (stack-element-value  (reverted-result-stack 'pop!)) ]
                [abstract-rec  (lookup-environment
                                env
                                dump
                                identifier)]
                [dummy (write-object-nl (cons "Identifier: " identifier))]
                [parameter-binding (stack-element-value (reverted-result-stack 'pop!))]
                [the-closure (make-closure (abst-variable (first abstract-rec))
                                           (abst-code (first abstract-rec))
                                           env)])
                                      
                                    
                              
          (begin
            (print-stack-env-code op-stack fun-stack env code  "debug the ap?")
            (write-object-nl "Enter application ->")
                                 
                              
                                    
                                 
                                    
            (op-stack 'print-stack)                        
            (write-newline )                              
            (write-string "Parameter Binding: ")
            (write-string (number->string parameter-binding))
            (write-newline)
              

            (let ( [fresh-stack (make-stack (list))]
                   [extended-env 
                    (extend-environment
                     (closure-environment the-closure)
                     (closure-variable the-closure)
                     parameter-binding)
                    ]
                   )
              (begin
                ( cond
                   ((> (op-stack 'size) 0)
                    (fresh-stack 'push! (op-stack 'peekit)) ;;;peekit before that
                    )
                   (else 'do-nothing)
                   )
           
                (make-secd   (remove-params-from-stack op-stack (make-stack (list)))
                             fun-stack 
                             extended-env                                   
                             (closure-code the-closure)
                             (cons
                              (make-frame op-stack fun-stack env (rest code))
                              dump)
                             heap-inst)
                ) ) ))))))
                   
  

                     
                   
                     
     
    
;; Abhandlung einer 'is?' Bedingung
;; Status QUO (KOPF-Salat): ->
;; describe later !!! it is hard to tell becuse i-know-what-i-am-doing ;-)
;; Zuerst
(define do-is
  (lambda (is-cond secd-state)
    (define code (secd-code secd-state))
    (define env (secd-environment  secd-state))
    (define op-stack (secd-stack secd-state))
    (define fun-stack (secd-fun-stack secd-state))
    (define dump (secd-dump secd-state))
    (define heap-inst (secd-heap secd-state))
    (let*  (

            [fresh-stack (make-stack (list))]
            [dummy-w (write-object-nl (list "the case object" is-cond))]
            [dummy    ( cond
                         ((> (op-stack 'size) 0)
                          (fresh-stack 'push! (op-stack 'peekit))
                          )
                         (else (make-stack (list)))
                         )]
            [cond-code  (single-cond-what (first code))]
            [cond-secd (make-secd
                        fresh-stack
                        fun-stack
                        env
                        cond-code
                        dump
                        heap-inst)])
      (begin
                                  
        (let* ([res-secd  (secd-exec-prim cond-secd)]
               [res-op-stack (secd-stack res-secd)]
               [res-fun-stack (secd-fun-stack res-secd)]
               [res-env (secd-environment res-secd)]
               [res-dump (secd-dump res-secd)]                                  
               [cond-result
                (stack-element-value (res-op-stack 'pop!))]
               [the-code  (if cond-result
                              (single-cond-code (first code))
                              (rest code))])
          (begin
            (write-object-nl (list "Code executed: -> " the-code))
            (make-secd  op-stack ;; op-stack
                        fun-stack
                        env                                                                               
                        the-code                   
                        dump
                        heap-inst)))))))
               

(define closure-apply
  (lambda (secd-rec new-code)
    (let* ([fresh-stack (make-stack (list))]            
           [the-closure  (make-closure
                          'NONE
                          new-code                                                             
                          (secd-environment secd-rec)) ]
           [op-stack (secd-stack secd-rec)])
      (begin
        ( cond
           ((> (op-stack 'size) 0)
            (fresh-stack 'push! (op-stack 'peekit))) ;;;peekit before that
                                            
           (else  list))
                                     
        (secd-exec-prim  (make-secd
                          op-stack
                          (make-stack (list))
                          (secd-environment secd-rec)                                                                                                        
                          (closure-code the-closure)
                          (cons
                           (make-frame op-stack
                                       (secd-fun-stack secd-rec)
                                       (secd-environment secd-rec) 
                                       (rest
                                        (secd-code secd-rec) ))
                           (secd-dump secd-rec) )
                          (secd-heap secd-rec)
                          ))))))
                   

(define eval-secd (lambda (ast-rec)
                    (let* ([fresh-stack (lambda ()
                                          (make-stack (list)))]
                           [secd-mach (make-secd  (fresh-stack)
                                                  (fresh-stack)
                                                  (ast-env ast-rec)
                                                  (ast-code ast-rec)
                                                  (ast-dump ast-rec)
                                                  (make-heap (list)))]
                           [new-frame (make-frame (fresh-stack) (fresh-stack) (ast-env ast-rec)  empty)]
                           [dump (append (list new-frame) (ast-dump ast-rec))]
                           [result
                            (process (make-ast (fresh-stack) (ast-code ast-rec) empty (ast-env ast-rec) dump))])
                    
                      (stack-element-value ((secd-stack result) 'pop!)
                                           ))))