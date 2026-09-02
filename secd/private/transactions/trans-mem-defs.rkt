#lang racket
(require rnrs/records/syntactic-6
         rnrs/bytevectors-6
         db/base
         data/ring-buffer
         deinprogramm/sdp/record
         deinprogramm/signature/signature
         deinprogramm/signature/signature-german
         deinprogramm/signature/signature-syntax)


(provide (all-defined-out))



;; transaction memory, transaction pool and shadow transaction mem

(define port-sig (signature (predicate integer?)))

(define path-sig (signature (predicate string?)))

(define mode-sig (signature (predicate integer?))) ;; change to 'enum

(define state-sig (signature (predicate integer?))) ;; change to 'enum

(define type-sig (signature (predicate integer?))) ;; change to 'enum

(define data-sig (signature (predicate bytevector?))) 

(define-record trans-file
  make-trans-file trans-file?
  (trans-file-t-port port-sig)
  (trans-file-mode mode-sig)
  (trans-file-state state-sig)
  (trans-file-type type-sig)
  (trans-file-path path-sig)
  (trans-file-temp-path path-sig))

(define trans-file-sig (signature (predicate trans-file?)))

(define db-conn-sig (signature (predicate connection?)))

(define db-trans-handle (signature (predicate cons?)))

(define-record trans-db
  make-trans-db trans-db?
  (trans-db-connection db-conn-sig) ;; ---> Here go on
  (trans-db-transaction-handle db-trans-handle))

(define trans-db-sig (signature (predicate trans-db?)))

(define-record trans-mem-block
  make-trans-mem trans-mem-block?
  (trans-mem-id integer)
  (trans-mem-files trans-file-sig)
  (trans-mem-db trans-db-sig))

(define trans-mem-block-sig (signature (predicate trans-mem-block?)))


;; signature definitions