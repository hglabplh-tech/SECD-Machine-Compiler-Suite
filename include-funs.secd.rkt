((define calc-base
    (lambda (x)
      (mul x (div x 2))
      ))
(define test-west
  (lambda (x)
    (cond-branch (< x 9)
                 (mul x 18)
                 (add (apply-fun calc-base 4) 2)))))
 
