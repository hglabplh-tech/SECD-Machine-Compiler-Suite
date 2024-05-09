#lang racket
(require   expect/rackunit "secd-compiler.rkt"
         "secd-vm-proc.rkt")

#;(check-expect (eval-secd (compile-secd
            '((define parm-test
                (lambda (x y z)
                  (mul z (add y x))))
              (apply-fun parm-test 3 4 2)))) 14)


(eval-secd
  (read-analyze-compile "test-prog.secd.rkt"))



#; (eval-secd (compile-secd
 '((define type-test
     (lambda (x)
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
   (apply-fun type-test 1)
   (apply-fun type-test 2)
   (apply-fun type-test 3)
   (apply-fun type-test 4)
   (apply-fun type-test 5)
   (apply-fun type-test 6)
   (apply-fun type-test 7)
   (apply-fun type-test 8))))
                         