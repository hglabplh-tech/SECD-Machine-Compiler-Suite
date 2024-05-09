#lang racket

(provide display-env
         )

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
          
