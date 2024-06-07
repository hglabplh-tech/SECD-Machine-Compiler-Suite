#lang racket

(require racket/async-channel)

(define ch (make-channel))

(define buffer '())

  

(define event-thread
  (lambda ()
    (let thread-loop ([msg  (sync ch)])
      (evt-queue msg)
     (thread-loop  (sync ch)))))

(define evt-queue
  (lambda (message)
   (set! buffer(append buffer (list message)))
    ))

(define read-and-send
  (lambda ()
    (let ([result (read-line (current-input-port))])
      
       (channel-put ch result)
      result)))

  
(define client
  (lambda ()
    (let the-loop ([input (read-and-send)])
      (if (equal? input "quit")
          'stop
     (the-loop
      (read-and-send))))))

(thread event-thread)
(client)
(println buffer)



