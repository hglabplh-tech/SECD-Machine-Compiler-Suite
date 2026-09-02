#lang racket
(require rnrs/records/syntactic-6
         data/ring-buffer
         deinprogramm/sdp/record
         deinprogramm/signature/signature
         deinprogramm/signature/signature-german
         deinprogramm/signature/signature-syntax
         "trans-mem-defs.rkt"
         "../machine/secd-vm-defs.rkt")


;; Transaction isolation levels

(define read-uncomitted (gensym "read-uncommitted"))
(define read-comitted (gensym "read-committed"))
(define repeatable-read (gensym "repeatable-read"))

(define test-west
  (lambda (val #:iso-level level)
    (if (equal? level  read-comitted)
        (display "read-c")
        (if (equal? level read-uncomitted)
            (display "read-uc")
            (display "repeatable-r")))))

(test-west "hop" #:iso-level read-comitted)
(test-west "hop" #:iso-level read-uncomitted)
