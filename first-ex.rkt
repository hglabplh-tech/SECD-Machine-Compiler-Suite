#lang deinprogramm/sdp/advanced

(require (only-in racket/base println)
         )


(define test-expression '((define parm-test
                (lambda (x y z)
                  (mul z (add y x))))
              (apply-fun parm-test 3 4 2)))

(println test-expression)
;;(define syn-expr (datum->syntax #f (car (car (cdr (car (cdr (cdr (car expression)))))))))
;;(println (syntax->datum syn-expr))

(define traverse (lambda (expression)
                   (if (empty? expression)
                   (list)
                   (let* ([next-expr (if (cons? expression)
                                        
                                               (rest expression)
                                               expression)
                                               ]
                         [first-expr (if (cons? expression)
                                        
                                               (first next-expr)
                                               expression)])
                     (begin
                       (if 
                                 (cons? first-expr)
                          
                          (traverse (rest first-expr))
                                    (begin
                         (println (first expression))
                         (println (cons? (first expression)))
                         (println (or (symbol? (first expression)) (number? (first expression))))
                         (println first-expr)
                         (println (cons? first-expr))
                         (println (or (symbol? first-expr) (number? first-expr)))                         
                         (println next-expr)
                         (println (cons? next-expr))
                         (println (or (symbol? next-expr) (number? next-expr)))
                     (traverse (rest next-expr))
                     )))))))
                    

(traverse test-expression)
                     