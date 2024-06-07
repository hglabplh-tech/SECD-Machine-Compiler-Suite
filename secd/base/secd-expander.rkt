#lang racket


(require (for-syntax syntax/parse) "secd-funs.rkt")

(provide 
 (rename-out [module-begin #%module-begin])
 #%top-interaction
 #%app
 #%datum
 quote
)
         
(define-syntax (module-begin stx) 
  (syntax-parse stx
      [(module-begin expr ...)       
       #'(#%module-begin
          expr ...)]))  
