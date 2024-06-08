#lang racket

(require rnrs/records/syntactic-6
         "thread-chan-sem-defs.rkt")

(provide display-env
         io-thread
         put
         putln
         print
         exit
         print-buffer
         )


;; Start des I/O Thread Codes

  
(define-record-type
  (io-queue make-io-queue io-queue?)
  (fields
   (mutable buffer io-queue-buffer set-io-queue-buffer!)))

(define io-enqueue
  (lambda (queue message)
    (let ([content (io-queue-buffer queue)])
      (set-io-queue-buffer!
       queue
       (append content
               (list message)))
      (values queue (io-queue-buffer queue))
      )))

(define io-dequeue
  (lambda (queue message)
    (let* ([content (io-queue-buffer queue)]
           [element (if (not (empty? content))
                        (first content)
                        #f)]
           [rest-of-content (if (and (not (empty? content))
                                     (not (empty? (rest content))))
                                (rest content)
                                empty)]
           )
      (set-io-queue-buffer!
       queue
       rest-of-content)
      (values queue element)
      )))
    

(define io-handler
  (lambda (cmd queue message)
    (cond
      ((equal? cmd 'enqueue)
       (let-values ([(q new-content) (io-enqueue queue message)])
         (println message)
         new-content))
      ((equal? cmd 'dequeue)
       (let-values ([(q element) (io-dequeue queue message)])
         element))
      )
         
    ))


(define io-thread
  (lambda ()
    (let ([io/queue
           (make-io-queue (list))])
      (let thread-loop
        ([cmd 'enqueue]
         [message (channel-get io-channel)])
        (if (equal? cmd 'exit)
            (channel-put result-channel (current-thread))
            'nothing
            )
        
        (cond
          ((and (equal? cmd 'dequeue)
                (empty? (io-queue-buffer io/queue)))
           (channel-put result-channel 'end-of-buffer))
          ((or (equal? cmd 'enqueue) (not (empty?  (io-queue-buffer io/queue))))           
           (let ([result (io-handler cmd io/queue message)])            
             (cond
               ((equal? cmd 'enqueue)                
                (channel-put result-channel result)
                )
               ((equal? cmd 'dequeue)                
                (channel-put result-channel result)
                )
               (else  (channel-put result-channel 'end-of-buffer)))
             )))
        (thread-loop 'enqueue
                     (channel-get io-channel)))                   
          
      )))

(define put
  (lambda (message)    
   ;; (channel-put io-channel 'enqueue)
    (channel-put io-channel message)
    (let ([result (channel-get result-channel)])
      result
      )))

(define out
  (lambda ()
    ;;(channel-put io-channel 'dequeue)
    (channel-put io-channel "")
    (let ([result (channel-get result-channel)])
     
      result
      )))

(define putln
  (lambda (message)
    (let ([msg (string-append message "\n")])
      (out))))

(define exit
  (lambda ()
    ;;(channel-put io-channel 'exit)
    (channel-put io-channel "")
    (let ([the-thread (channel-get result-channel)])
      (kill-thread the-thread)
      )))


(define print-buffer
  (lambda () 
    (let buffer-loop
      ([thread-res (print)])
  (if (equal? thread-res 'end-of-buffer)
      'end-of-buffer
      (begin (println thread-res)
             (buffer-loop (print))))
)))
  
;; Ende des I/O Thread Codes

(define display-env
  (lambda (bindings-list)
    (let the-loop ([intern-lst bindings-list])
      (if (empty? intern-lst)
          'end
          (begin
            (println "Binding: ->")
            (println (first intern-lst))
            (println (rest intern-lst))
            (println "<- End Binding")
            (the-loop (rest  intern-lst))
            )))))

(thread io-thread)

;;(put "Hallo here I am")
;;(put "coming in like a hurrican")
;;(put "Next to debug")
;;(put "this is a cmd (lambda () (+ 7 8))")
;;(put "this is a cmd 'cd test'")
;;(put "test the west we are the best")
;;(put "Enter Command: ")
;;(println "=====")
;;(println "------ Result of call")
;;(println "=====")

#;(let ([thread-res (print)])
  (println (list "result: " thread-res))
  (if (or (equal? thread-res 'end-of-buffer) (not thread-res))
      (println "I/O is ready")
      'nothing))
#;(let ([thread-res (print)])
  (println (list "result: " thread-res))
  (if (or (equal? thread-res 'end-of-buffer) (not thread-res))
      (println "I/O is ready")
      'nothing))
#;(put "test the west we are the best") 
#;(let ([thread-res (print)])
  (println (list "result: " thread-res))
  (if (or (equal? thread-res 'end-of-buffer) (not thread-res))
      (begin
        (println "I/O is ready")
        'eof)
      (begin
        (println "printed")
                 'nothing)))   
#;(let ([thread-res (print)])
  (println (list "result: " thread-res))
  (if (or (equal? thread-res 'end-of-buffer) (not thread-res))
      (begin
        (println "I/O is ready")
        'eof)
      (begin
        (println "printed")
                 'nothing)))
;;(print-buffer)
;;(put "this command is important:")
;;(put "cd /etc")
;;(put "vi test.dmp")
;;(print-buffer)
;;(exit)

