#lang racket
(require rnrs/io/ports-6
         "secd-vm-defs.rkt"
         "debug-out.rkt"
         "secd-vm-proc.rkt"
         "secd-compiler.rkt"
         "secd-dbg-print-it.rkt"
         "stack.rkt"
         "operations.rkt")

(provide
 debug-main
 )


(define process-thread
  (lambda ()
    (let ([ast-rec (channel-get (debug-channel 'none))]
          [debug-inf (channel-get (debug-channel 'none))] 
          )       
      (let* ([secd-rec (eval-secd ast-rec)])          
        (channel-get  (debug-channel 'none))
        (put "application-end")
        (put (list "the-result-val"
                   (secd-stack secd-rec)))))))

(define start-it
  (lambda ()
    (let ([the-thread (thread process-thread)])      
      the-thread)))

(define start-debug-thread
  (lambda (ast debug-inf)
    (thread io-thread)
    (let thread-loop ([dummy 'start])
      (set-debug-on! #t)
      (let ([the-thread (start-it)])        
        (channel-put (debug-channel 'none) ast)
        (channel-put (debug-channel 'none) debug-inf)
        (write-object-nl "Type a debug command: ")
        (let the-loop ([input (read-line (current-input-port) 'any)])
          (cond ((equal? input "quit")
                 'exit)
                ((equal? input "run")    
                 (send-dbg-cmd "run")
                 (the-loop "run"))                 
                ((equal? input "dbg-restart")
                 (debug-channel 'init)
                 (kill-thread the-thread)
                 (thread-loop 'start)
                 )
                (else     
                 (begin
                   (send-dbg-cmd input)
                   (write-object-nl "Type a debug command: ")
                   (the-loop (read-line (current-input-port) 'any))))
                )
          )))))

(define test-code '((define calc-base
    (lambda (x)
      (mul x (div x 2))
      ))
(define test-west
  (lambda (x)
    (cond-branch (< x 9)
                 (mul x 18)
                 (add (apply-fun calc-base 4) 2))))
                   (define higher (lambda (u t e)
                  (cond-branch (< t 9)
                              (sqrt e)
                               (add t (apply-fun test-west u)
                                    ))))
 (apply-fun higher 10 5 17)))

(define small-test '((define parm-test
                (lambda (x y z)
                  (mul z (add y x))))
              (apply-fun parm-test 3 4 2)))

(define debug-main
  (lambda (fname)
    (let ([ast-rec (read-analyze-compile fname)]
          [info-rec (make-debug-info (list abst? where? app-fun?) #f)])
      (start-debug-thread ast-rec info-rec))))

(define debug-constant
  (lambda (const-code)
    (let ([ast-rec (compile-secd const-code)]
          [info-rec (make-debug-info (list abst? where? app-fun?) #f)])
      (start-debug-thread ast-rec info-rec))))

;;(debug-main "test-prog.secd.rkt")
(debug-constant small-test)


