#lang racket
(provide (all-defined-out))


(define make-stack
  (lambda (stk)  
      (lambda (message . args)
        
        (case message

          ;; If the message is empty?, the stack returns #t if its
          ;; storage location contains the null object, #f if it
          ;; contains a non-empty list. 

          ((empty?) (null? stk))

          ((swap-last!)
           (let ([first (car stk)]
                 [dummy (set! stk (cdr stk))]
                 [second (car stk)]
                  [dummy2 (set! stk (cdr stk))])
             (begin
             (print first)
             (print second )
             (set! stk (append (list second first) stk) )
              )))

          ;; The push! message should be accompanied by an extra
          ;; argument -- the value to be pushed onto the stack.  This
          ;; value is simply added to the front of the list in the
          ;; private storage location. 

          ((push!) (set! stk (append (list (car args)) stk)))

           ((reverse!) (set! stk (reverse stk)) (make-stack stk))
          ((reverse) (make-stack (reverse stk)))

          ((push-swapped!) (set! stk (append (list (car stk)) (list (car args)) (cdr stk)))) ;; FIXME for first lement


         ((clear!) (set! stk (list)))

          ;; If the message is top, the stack returns the first
          ;; element of the list in private storage, or signals an
          ;; error if that list is empty.

          ((top) (if (null? stk)
                     (error "top: The stack is empty.")
                     (car stk)))

          ;; If the message is pop!, the stack returns the first
          ;; element of the list in private storage after removing
          ;; that element from that list.

          ((pop!) (if (null? stk)
                    (error "pop!: The stack is empty.")
                    (let ([pop-val  (car stk)])
                     (set! stk (cdr stk)) ;; side-effects in that manner are evil
                       pop-val
                           )))
                     
                     

          ;; Comment out any of the following operations that are not
          ;; wanted in a particular application of stacks.

          ;; When it receives the size message, the stack reports the
          ;; number of elements in the list in private storage.

          ((size) (length stk))

          ;; If the message is nth, there should be an extra argument
          ;; -- the (zero-based) position of the item desired, or in
          ;; other words the number of values on the stack that
          ;; precede the one to be returned.

          ((nth) (list-ref stk (car args)))

           ((peekit)  (if (null? stk)
                     (error "top: The stack is empty.")
                     (car stk)))

          ((duplicate) (make-stack stk))

           ((add-all)
            (let* ([arg-list ((car args) 'get-intern)]
                   [stk-list (append stk arg-list)])
              (make-stack stk-list)))
          

          ((add-all!)
            (let* ([arg-list ((car args) 'get-intern)]
                   [stk-list (append stk arg-list)])
              (set! stk stk-list)
              (make-stack stk-list)))
            

          ((get-intern) stk)

          ((print-stack)
           (printf "\nStart Stack\n")
           (for-each (lambda (arg)                                     
              (printf "Element ~a\n" arg)
              arg)
            stk)
                         (printf "\nEnd Stack\n"))
          (else (error "unknown command"))

     

          ))))


(let ([the-stack (make-stack (list))]
      [stack-two (make-stack (list))])
  (the-stack 'push! 8)
  (the-stack 'push! 10)
  (the-stack 'push! 12)
   (stack-two 'push! 1)
  (stack-two 'push! 3)
  (stack-two 'push! 5)
  (display (stack-two 'peekit))
  (newline)
  (display (stack-two 'nth 2))
   (newline)
  (let ([new-stack (the-stack 'add-all stack-two)])
     (the-stack 'size)
   (stack-two 'size)
  (new-stack 'size)
    (new-stack 'get-intern)
    (new-stack 'push-swapped! 'A)
    (new-stack 'push-swapped! 'B)
    (new-stack 'push-swapped! 'C)
    (new-stack 'push-swapped! 'D)
     (new-stack 'push-swapped! 'E)
    (new-stack 'push-swapped! 'F)
    (new-stack 'print-stack)
    (new-stack 'swap-last!)
    (new-stack 'print-stack)
    
  ))
    
 
