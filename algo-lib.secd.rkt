'((define fib
  (lambda (x res)
  (cond-branch (== x 0)
               (mul res 1)
               (mul (apply-fun fib (sub x 1) x) x))))
(define chain
  (lambda (x)
  (cond-branch (== x 0)
               (mul x 1)
               (add x (apply-fun chain (sub x 1)))))))

