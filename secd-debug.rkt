#lang racket
(require rnrs/io/ports-6
         "secd-vm-defs.rkt"
         "debug-out.rkt"
         "secd-vm-proc.rkt"
         "secd-compiler.rkt"
         "stack.rkt"
         "operations.rkt")

(provide
   
 )


(define process-thread
  (lambda ()
      (let ([ast-rec (channel-get (debug-channel 'none))]
            [debug-inf (channel-get (debug-channel 'none))] 
            )
       
        (eval-secd ast-rec))))

(define start-it
  (lambda ()
    (let ([the-thread (thread process-thread)])      
      the-thread)))

(define start-debug-thread
  (lambda (ast debug-inf)
    (let thread-loop ([dummy 'start])
       (set-debug-on! #t)
      (let ([the-thread (start-it)])        
        (channel-put (debug-channel 'none) ast)
        (channel-put (debug-channel 'none) debug-inf)
        (write-object-nl "Type a debug command: ")
        (let the-loop ([input (get-line (current-input-port))])
          (cond ((equal? input "quit")
                 'exit)
                ((equal? input "dbg-restart")
                 (debug-channel 'init)
                 (kill-thread the-thread)
                 (thread-loop 'start)
                 )
                (else     
                 (begin
                   (send-dbg-cmd input)
                   (write-object-nl "Type a debug command: ")
                   (the-loop (read-line (current-input-port)))))
                )
          )))))

(define debug-main
  (lambda (fname)
    (let ([ast-rec (read-analyze-compile fname)]
          [info-rec (make-debug-info (list abst? where? app-fun?) #f)])
      (start-debug-thread ast-rec info-rec))))

(debug-main "test-prog.secd.rkt")


