#lang racket
(require parser-tools/yacc  "secd-lex.rkt")


(provide parse)

(define myparser
  (parser 
   (start expression)
   (end EOF)
   (tokens value-tokens op-tokens )
   (src-pos)
   (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))

   ;; only the first two lines are new -- change completely
   (grammar (expression   ;; here comes the block - recursive definition like for lhs / rhs and after that a closing bracket
             ;; instead of (ID ) a block containing the right things has to be defined
             [( (expression block)) ($1)] ;; repeat
             [(params (constant param-id block)) $1]
             [(fun-def (fun-name) (params)) $1]
              [(fun-name (built-in param-id)) ($1)]
             [(built-in (is? where? mul div add sub lambda def-define)) ($1)]
             
             
             ))))


                
  (define (parse ip)
  (port-count-lines! ip)  
  (myparser (lambda () (next-token ip))))   
 
