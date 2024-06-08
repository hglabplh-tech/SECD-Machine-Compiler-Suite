#lang racket

(require rnrs/records/syntactic-6
         data/ring-buffer
         deinprogramm/sdp/record
         deinprogramm/signature/signature
         deinprogramm/signature/signature-german
         deinprogramm/signature/signature-syntax
         "../machine/secd-vm-defs.rkt")

(define thread-num 0)


;; here the things for setting and defining a thread state are defined

;; the state change mnemnonics

(define set-new-thread-cmd (gensym "new-thread-cmd"))
(define set-start-thread-cmd (gensym "start-thread-cmd"))
(define set-restore-thread-cmd (gensym "restore-thread-cmd"))
(define set-store-thread-cmd (gensym "store-thread-cmd"))

(define thread-cmd?
  (lambda (cmd)
    (and symbol? (or
                  (equal? cmd set-new-thread-cmd)
                  (equal? cmd set-start-thread-cmd)
                  (equal? cmd set-restore-thread-cmd)
                  (equal? cmd set-store-thread-cmd)                 
                  )))) 

(define thread-cmd-sig (signature (predicate thread-cmd?))) ;;
;; the state of a running/starting.... thread

(define initializing (gensym "initializing"))
(define ready (gensym "ready"))
(define starting (gensym "starting"))
(define blocked (gensym "blocked"))
(define wait (gensym "wait"))
(define suspended (gensym "suspended"))
(define running (gensym "running"))
(define terminated (gensym "terminated"))
(define sudden-death (gensym "sudden-death"))

(define thread-state?
  (lambda (state)
    (and symbol? (or
                  (equal? state initializing)
                  (equal? state ready)
                  (equal? state starting)
                  (equal? state blocked)
                  (equal? state wait)
                  (equal? state suspended)
                  (equal? state running)
                  (equal? state terminated)
                  (equal? state sudden-death)
                  )))) 

(define thread-state-sig (signature (predicate thread-state?))) ;; change this to enum 



  

;; Here we need a define Syntax !!!!!!!
(define enum-type
  (lambda (name . names)
    (map (lambda (val)
           (let ([val-sym (gensym val)])
             (append
              (list (cons val val-sym))
              (list (cons val-sym val)))))
         (cons name names))))
(define states (enum-type "blocked" "wait" "suspended"))


(define hasheq-sig (signature (predicate hash?)))


(: transact-proc? (any -> boolean))
(define transact-proc?
  (lambda (proc)
    (and (procedure? proc)
         (procedure-arity-includes? proc 2) )))

(define transact-proc-sig (signature (predicate transact-proc?)))

(define commarea-sig (signature (predicate vector?)))

(println states)

(define-record threadpool
  make-threadpool threadpool?
  
   (threadpool-thash hasheq-sig)
   (threadpool-thinking-trans-memory transact-proc-sig) ;; change to real sig CICS-like transactional threads
   (threadpool-commarea commarea-sig) ;; like in CICS ... smile
   )


(define the-pool (make-threadpool  (make-hasheq) (lambda (one two)
                                                   two)
                                   (make-vector))) ;; add transactional-logic



(define get-map
  (lambda()
    (threadpool-thash the-pool)))

(define add-block
  (lambda (tid block)
    (hash-set! (get-map) tid block)
    ))

(define remove-block
  (lambda (tid block)
    (hash-remove! (get-map) tid)
    ))

(define string-sig
  (signature (predicate string?)))

(define integer-sig
  (signature (predicate integer?)))

(define proc-with-arity-sig
  (signature (predicate procedure?))) ;; make a lambda asking also for arity

(define state-fun-sig
  (signature (predicate procedure?)))

;; arity ??

(define loc-space-sig (signature (list-of pair?)))

  
;;(block-states
 
(define-record thread-block
  make-thread-block thread-block? 
  (thread-block-name string-sig)
  (thread-block-tid integer-sig)  
  (thread-block-state thread-state-sig)
  (thread-block-state-proc state-fun-sig)
  (thread-block-runnable proc-with-arity-sig)
  (thread-block-local-space loc-space-sig))

(define new-fresh-thread
  (lambda (name runnable)
    (set! thread-num (+ thread-num 1))
    (make-thread-block name
                       thread-num
                       initializing
                       (lambda (intern-arg) intern-arg)
                       runnable
                       '((cons 'tid thread-num))
                       )
    ))

(define thread-block-change-state
  (lambda (thread-inst state) 
(make-thread-block (thread-block-name thread-inst)
                        (thread-block-tid thread-inst)
                        state
                        (thread-block-state-proc thread-inst)
                        (thread-block-runnable thread-inst)
                        (thread-block-local-space thread-inst))))


(define thread-block-sig (signature (predicate thread-block?)))

(: start-thread (thread-block-sig -> thread-block-sig))
(define start-thread
  (lambda (thread-inst)
    (let ([started-inst (thread-block-change-state thread-inst starting)])      
                        
    )))
    




(define kernel
    (lambda (ring-buff)
      (let ([act-thread-space ring-buff]
            [sched-preemtive (make-sched-meth-preempt)]
            [sched-non-preemptive (make-sched-meth-non-preempt)]
            [real-processor
             (lambda (cmd  args)
               (cond
                 ((equal? cmd set-start-thread-cmd)
                  (let-values ([[thread-inst] args]) 
                  (ring-buffer-push! act-thread-space
                                     (thread-block-change-state thread-inst ready)) ;; start the thread as being ready to run
           
            ))))])
        real-processor
        
        )))