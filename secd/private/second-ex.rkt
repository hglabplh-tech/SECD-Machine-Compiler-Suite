#lang racket/base


(define test-expression '((define parm-test
                (lambda (x y z)
                  (mul z (add y x))))
              (apply-fun parm-test 3 4 2)))
(define empty? null?)
(define cons? list?)
(define first car)
(define rest cdr)
(println test-expression)
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