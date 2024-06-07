#lang racket

(provide 
         io-channel
         result-channel
         the-dbg-channel
         set-dbg-channel!)

(define io-channel (make-channel))

(define result-channel (make-channel))

(define the-dbg-channel (make-channel))

(define set-dbg-channel!
  (lambda (value)
    (set! the-dbg-channel value)))
