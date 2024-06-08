#lang racket

(require 
         "secd-compiler.rkt" "secd-vm-defs.rkt" "operations.rkt" "stack.rkt")

;; Diese Audrücke müssen erneut geprüft werden
;;(check-expect (compile-secd '(((lambda (x) (lambda (y) (mul y  (add x y)))) 1) 2)) 3);; this works now
;;(check-expect (compile-secd '((lambda (x) (mul 5 x)) 2)) 10)
;;(check-expect (compile-secd '(lambda (x) (lambda (y) (div 120 (mul y  (add x y)))) 1) 2) 20)
;; FIXME have to fix multiple nested expressions

;; Ein kleinerAusblick auf die nächste mögliche Funktionalität
#;(check-expect (compile-secd '(define test-add3 (lambda (x)
                                                   (lambda (y)
                                                     (add x y)           
                                                     )))) 7)

#;(check-expect (compile-secd '(define mul-add3 (lambda (y)
                                                  (lambda(fun)
                                                    (mul y ((test-add3 y) 9)))))) 8)

;; hier mussnoch ((fun arg) arg2)  abgedeckt werden
#;(check-expect (compile-secd'((define test-west
                                 (lambda (x)
                                 
                                   (mul x 18)))
                               (define higher (lambda (u)
                                                (lambda ()
                                                  (cond-branch (== u 17)
                                                               (add 5 (apply-fun test-west u))
                                                               (add 5 (apply-fun test-west 12)))                                               
                                                  )))
                               ((apply-fun higher 6)))) 42) ;

#;(check-expect (compile-secd'((define test-west
                                 (lambda (x)                                 
                                   (mul x 18)))
                               (define higher (lambda (u)
                                                (lambda ()
                                                  (cond-branch (< u 17)
                                                               (add 5 (apply-fun test-west u))
                                                               (add 5 ((higher 10))))
                                                  )))
                               ((apply-fun higher 10)))) 185)


              
#;(check-expect  (compile-secd'((define test-west
                                  (lambda (x)                                             
                                    (mul x 9) ))
                                (define higher (lambda (u)
                                                 (add 5 (apply-fun test-west u))
                                                 ))
                                (apply-fun higher 11)
                                (apply-fun higher 7) 
                                )) 42) ;

#;(check-expect  (compile-secd'((define test-west
                                  (lambda (x)                                             
                                    (mul x 9) ))
                                (define higher (lambda (u)
                                                 (add 5 (apply-fun test-west u))
                                                 ))
                                (apply-fun higher 10)
                                         
                                )) 42)

#;(check-expect (compile-secd '((lambda (x) (lambda (y) (add x y) 1) 2))) 3)



;; Ultimativer Test der Rekursion
#;(compile-secd
 '((define calc-base
     (lambda (x)
       (mul x (div ((apply-fun higher 5)) 2))
       ))
   (define test-west
     (lambda (x)
       (cond-branch (< u 9)
                    (mul x 18)
                    (add 2 (apply-fun calc-base 3)))))
   (define higher (lambda (u)
                    (lambda ()
                      (cond-branch (< u 9)
                                   (mul 5 (add 7 u))
                                   (add (apply-fun test-west u) ((apply-fun higher 7))))
                      )))
   ((apply-fun higher 10))))

(compile-secd
 '((define allocator
     (lambda (x)
       (add (heap-alloc g 8)
            (sub (mul x (heap-set-at! g 16))
                 (heap-get-at g))
            )))
   (define higher (lambda (x)                                             
                    (cond-branch (< x 11)
                                 (mul 8 7)
                                 (add (apply-fun allocator 100) (apply-fun higher 10))
                                 )))
   (apply-fun higher 10)))



(compile-secd
 '((define allocator
     (lambda (x y z)
       (add (heap-alloc g 8)
            (sub (mul x (heap-set-at! g 16))
                 (heap-get-at g))
            )))))

(compile-secd
 '((define allocator
     (lambda (x)
       ((lambda (y)
          ((lambda (z)
             (add (heap-alloc g 8)
                  (sub (mul x (heap-set-at! g 16))
                       (heap-get-at g))
                  )) 9)) 116)))))

(compile-secd
 '((define allocator
     (lambda (x)
       (add (heap-alloc g 8)
            (sub (mul x (heap-set-at! g 16))
                 (heap-get-at g))
            )))))

#;(compile-secd
 '((define allocator
     (lambda (x)
       ((lambda ()
          (add (heap-alloc g 8)
               (sub (mul x (heap-set-at! g 16))
                    (heap-get-at g))
               )))))))

(compile-secd
 '((define allocator
     (lambda (x)
       (lambda (y)
         (lambda (y)
           (add (heap-alloc g 8)
                (sub (add x y) z)))
         )))
   (apply-fun allocator 7 8 9)))

(compile-secd
 '((define allocator
     ((lambda (x)
        ((lambda (y)
           ((lambda (z)
              (add (heap-alloc g 8)
                   (sub (add x y) z)
                   ))7 )) 8)) 9))))
                   




;; Einfacher Test für cond-branch

#;(compile-secd'((define higher (lambda (x)
                                (lambda ()
                                  (cond-branch (< x 11)
                                               (mul 5 (add 7 x))
                                               (add 5 ((apply-fun higher 10))))
                                  )))
               ((apply-fun higher 10))))




;; Verschachteltes where

(compile-secd'((define higher (lambda (x)
                                (lambda ()
                                  (cond-branch (< x 11)
                                               (mul 5 (add 7 x))
                                               (cond-branch (> x 20)

                                                            (add 5 (mul 9 x))
                                                            (sub x (div 100 5))
                                                             
                                                            )))))
               (apply-fun higher 10)
               (apply-fun higher 19)                
               (apply-fun higher 22)))


(compile-secd
 '((define allocator
     (lambda (x y z)                      
       (sub (add x y) z)))
   (apply-fun allocator 7 8 9)))

(compile-secd
 '((define allocator
     (lambda (x)
       (add (heap-alloc g 8)
            (sub (mul x (heap-set-at! g 16))
                 (heap-get-at g))
            )))
   (define higher (lambda (x)                          
                    (where-cond  
                     (is? (== x 40)
                          (add 3 4)
                          )
                     (is? (== x 80)
                          (sub x  10)
                          )
                     (is? (< x 11)
                          (mul 8 7)
                          )
                     (is? (> x 120)
                          (add (apply-fun allocator 100) 10)
                                                      
                          ))
                    (cond-branch (> x 1100)
                                 (mul 3 90)
                                 (add (apply-fun allocator 200) (apply-fun higher 110))
                                 )
                    ))
   (apply-fun higher 180)))

(compile-secd
 '((define allocator
     (lambda (x)
       (add (heap-alloc g 8)
            (sub (mul x (heap-set-at! g 16))
                 (heap-get-at g))
            )))
   (define higher (lambda (x)
                    (code-block
                     (where-cond  
                      (is? (== x 40)
                           (add 3 4)
                           )
                      (is? (== x 80)
                           (sub x (apply-fun higher 10)))                                          
                      (is? (< x 11)
                           (mul 8 7))
                      (is? (> x 12)
                           (add (apply-fun allocator 100) (apply-fun higher 10))
                                          
                           ))
                     (cond-branch (> x 1100)
                                  (mul 3 90)
                                  (add (apply-fun allocator 200) (apply-fun higher 110))
                                  ))
                    ))
   (apply-fun higher 10)))

#;(define result_10 (make-abst ((make-stack (list))
                                (list (make-define-def 'test-num-var 25)
                                      (make-define-def 'test-str-var "too much trouble")
                                      (make-define-def 'test-boolean-var #t)
                                      (make-define-def 'test-byte-var 250)
                                      (make-define-def 'allocator
                                                       (make-abst 'x
                                                                  (list (make-push!  (list 'push! 'g)) 
                                                                        (make-push!  (list 'push! 8)))))))))
                                         

(compile-secd
 '((define test-num-var 25)
   (define test-str-var "too much trouble")
   (define test-boolean-var #t)
   (define test-byte-var #xFA)
   (define allocator
     (lambda (x)
       (add (heap-alloc g 8)
            (sub (mul x (heap-set-at! g 16)) ;;check this again
                 (heap-get-at g))
            )))
   (define higher (lambda (x)                          
                    (where-cond  
                     (is? (== x 40)
                          (add 3 4))
                     (is? (== x 80)
                          (sub x  10))
                     (is? (== x 11)
                          (add (apply-fun allocator 9) (mul test-num-var 7))
                          ))
                                
                    ))
   (apply-fun higher 11)))

(read-analyze-compile "test-prog.secd.rkt")


(compile-secd
 '((define heap-test
     (lambda (x)
       (where-cond
        (is? (== x 1)
             (heap-alloc hx 5))
        (is? (== x 2)
             (heap-set-at! hx 10))
        (is? (== x 3)
             (heap-get-at hx))
        (is? (== x 4)
             (heap-free hx)))))
   (apply-fun heap-test 1)
   (apply-fun heap-test 2)
   (apply-fun heap-test 3)
   (apply-fun heap-test 4)))



(compile-secd
 '((define type-test
     (lambda (x y)
       (where-cond
        (is? (== x 1)
             (number? x))
        (is? (== x 2)
             (list? x))
        (is? (== x 3)
             (integer? x))
        (is? (== x 4)
             (boolean? x))
        (is? (== x 5)
             (byte? x))
        (is? (== x 6)
             (char? x))
        (is? (== x 7)
             (procedure? x))
        (is? (== x 8)
             (string? x)))))
   (apply-fun type-test 1 "a")
   (apply-fun type-test 2 78)
   (apply-fun type-test 3 56)
   (apply-fun type-test 4 #xFF)
   (apply-fun type-test 5 '(8 9 9))
   (apply-fun type-test 6 7)
   (apply-fun type-test 7 89)
   (apply-fun type-test 8 70)))


