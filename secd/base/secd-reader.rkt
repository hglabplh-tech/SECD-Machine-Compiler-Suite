#lang racket
(require "secd-keyword-tab.rkt" )
 
(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))
 
(define (my-read in)
  (syntax->datum
   (my-read-syntax #f in)))

(define (my-read-syntax path port)
  (datum->syntax
   #f
   `(module rpcalc-mod "secd-expander.rkt"
      ,@(parse port))))