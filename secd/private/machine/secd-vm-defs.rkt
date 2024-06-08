#lang racket

(require rnrs/records/syntactic-6
         "thread-chan-sem-defs.rkt"
         "stack.rkt"
         "secd-dbg-print-it.rkt")
  
;;(list-data? 5)
;;(list-data?  '(6 7 8 9))
;;(list-data?  '(6))
;;(cons?  '(6))
;;lang/htdp-advanced
  

(provide make-binding
         binding
         binding?
         make-binding
         binding-variable
         binding-value
         the-empty-environment
         extend-environment
         remove-environment-binding
         lookup-environment
         lookup-act-environment
         make-frame
         frame?
         frame-stack
         frame-fun-stack 
         frame-environment
         frame-code
         term
         abstraction?
         application?
         secd
         make-secd
         secd-stack
         secd-fun-stack
         secd-environment
         secd-code
         secd-dump
         secd-heap
         make-closure
         closure-environment
         closure-variable
         closure-code
         closure?
         base?
         make-ap
         ap ap?
         make-tailap
         tailap tailap?
         make-prim
         prim? prim
         prim-operator
         prim-arity
         op
         make-op
         op?
         op-operation
         op-code
         op-params
         op-stack-arity
         op-stack-out
         op-parm-arity
         apply-primitive         
         primitive?
         primitive-application?
         abst
         make-abst
         abst abst?
         abst-variable
         abst-code
         op-code-syms
         ast
         make-ast ast?
         ast-stack
         ast-code
         ast-next 
         ast-env  
         ast-dump
         op-code?
         instruction
         smart-first
         smart-rest
         definition?
         define-def
         define-def?
         define-def-bind
         define-def-value   
         make-define-def
         eval-param
         fun-application?
         app-fun
         app-fun?
         make-app-fun
         var-symbol?       
         stack-element
         make-stack-element
         stack-element?
         stack-element-type
         stack-element-value
         new-stack-element
         where
         where-condition?
         make-where
         where?
         where-condition
         where-if-branch
         where-else-branch
         heap-assignment?
         heap-set-at!
         make-heap-set-at!
         heap-set-at!?
         heap-cell
         heap-cell?
         make-heap-cell
         heap-cell-variable
         heap-cell-init-value
         make-heap-get-at
         heap-allocator?
         heap-alloc
         heap-alloc?
         heap-free-fun?
         make-heap-alloc
         heap-free
         heap-free?
         make-heap-free
         heap-get-at
         heap-get-at?
         heap-getter?
         heap
         heap?
         make-heap
         heap-storage
         extend-heap-stor
         lookup-heap-stor
         free-heap-stor-cell
         is-where?
         is?
         single-cond
         make-single-cond
         single-cond?
         single-cond-what
         single-cond-code
         conditions
         make-conditions
         conditions?
         conditions-conds
         begin-it?
         code-block
         make-code-block
         code-block?
         code-block-code
         break
         the-break?
         break?
         make-break
         rest-or-empty
         debug-step
         debug-info
         make-debug-info
         debug-info?
         debug-info-break-criteria-list
         set-debug-info-break-criteria-list!
         debug-info-continue-end
         set-debug-info-continue-end! 
         debug-channel
         debug-on
         debug-inf
         set-debug-on!
         send-dbg-cmd
         native-scheme?
         make-native-scheme
         native-scheme-cmd?
         native-scheme-code)

;;Hier der "Prozessor" Befehlssatz - als Idee
(define op-code-syms '(  push pop peek create-const
                              load-const store-const alloc free svc-open svc-read
                              svc-write svc-close
                              read write
                              store load lambda let branch apply eq?
                              where add mul div sub mod  sqrt exp))


;; Für spätere Benutzung

(define op-code?
  (lambda (term)
    (or (equal? 'push  term)
        (equal? 'pop term)
        (equal? 'peek term)
        (equal? 'store term)
        (equal? 'load term)
        )))

;; Hir kommt die Sektion der "Primitiven Applikationen" das sind Operationen
;; welche direkkt in die Sprache eingebettet sind




(define primitive?
  (lambda (term)
    (or (equal? 'add  term)
        (equal? 'sub term)
        (equal? 'mul term)
        (equal? 'div term)
        (equal? 'square term)
        (condition-primitive? term)
        (bitwise-primitive? term)
        (type-predicate-primitive? term)
        )
    ))
       


(define condition-primitive?
  (lambda (term)
    (or (equal? 'is-eq? term)
        (equal? 'gt? term)
        (equal? 'lt? term)
        (equal? 'gt-eq? term)
        (equal? 'lt-eq? term)
        (equal? '! term)
        (equal? '&& term)
        (equal? '|| term)
        (equal? 'true? term)
        (equal? 'false? term)
        )
    ))

(define bitwise-primitive?
  (lambda (term)
    (or (equal? 'bit-rshift  term)
        (equal? 'bit-lshift  term)
        (equal? 'bit-and term)
        (equal? 'bit-ior  term)
        (equal? 'bit-xor term)
        (equal? 'bit-not term)
        )
    ))

(define type-predicate-primitive?
  (lambda (term)
    (or (equal? 'boolean? term)
        (equal? 'integer? term)
        (equal? 'number? term)
        (equal? 'string? term)
        (equal? 'char? term)
        (equal? 'byte? term)
        (equal? 'list? term)
        (equal? 'cons? term)
        (equal? 'procedure? term)
        )
    ))
        
    
    
           
;; Ende der Definitionen für Primitive Application


;; die instructionen sind die Befehle / Definitionen die die "VM" kennt 





; Applikations-Instruktion
(define-record-type 
  (ap make-ap ap?)
  )

; Eine endrekursive Applikations-Instruktion ist ein Wert
;   (make-tailap)
(define-record-type 
  (tailap make-tailap tailap?)
  )

; Eine Abstraktions-Instruktion hat folgende Eigenschaften:
; - Parameter (eine Variable)
; - Code für den Rumpf
(define-record-type
  (abst make-abst abst?)  
  (fields
   (immutable variable abst-variable)
   (immutable code abst-code)))

(define eval-param (lambda (term)
                     (if (symbol? (smart-first term))
                         (smart-first term)
                         '++_no_parm)))


(define rest-or-empty
  (lambda (token)
    (if (and (cons? token
                    )(cons? (rest token)))
        (rest token)
        (list)
        )))

; Eine Deefinitions-Instruktion hat folgende Eigenschaften:
; -  Ein Bezeichner
; -  ein Symbol ein Base eine Abstraction eine Applikation... 
(define-record-type
  (define-def make-define-def define-def?)
  (fields
   (immutable bind define-def-bind )
   (immutable value define-def-value)))


;; Hier die Definition der Umgebung
; Eine Bindung besteht aus:
; - Variable
; - Wer(define stack
(define-record-type
  (binding make-binding binding?)
  (fields
   (immutable variable binding-variable)
   (immutable value binding-value)))

;; dieser record is für die später komplett typisierte Speicherung von Werten auf dem Stack (noch nicht
;; vollendet)
(define-record-type
  (stack-element make-stack-element stack-element?)
  (fields
   (immutable type stack-element-type)
   (immutable datatype stack-element-datatype)
   (immutable value stack-element-value)))

;; hier kommt die internen Definitionen einer Zuweisung

;; Definition einer Zuweisung
; Eine Zuweisungs-Instruktion ist ein Wert
; diese Definitionen habe ich der Einfacheit halber teilweise von Mike Sperber übernommen

;; Definition wie ie Zuweisung im Code aussieht







(define heap-allocator?
  (lambda (term)
    (and (cons? term)
         (equal? 'heap-alloc (first term)))))




(define heap-free-fun?
  (lambda (term)
    (and (cons? term)
         (equal? 'heap-free (first term)))))




(define heap-assignment?
  (lambda (term)
    (and (cons? term)
         (equal? 'heap-set-at! (first term)))))


(define heap-getter?
  (lambda (term)
    (and (cons? term)
         (equal? 'heap-get-at (first term)))))



;; Rekord für eine heap Zuweisung // Getter / Allocator / Free
(define-record-type
  (heap-alloc make-heap-alloc heap-alloc?)
  )


(define-record-type
  (heap-free make-heap-free heap-free?)
  )

(define-record-type
  (heap-set-at! make-heap-set-at! heap-set-at!?)
  )

(define-record-type
  (heap-get-at make-heap-get-at heap-get-at?)
  )

;; Definition um Scheme native aufzurufen
(define-record-type
  (native-scheme make-native-scheme native-scheme?)
  (fields
   (immutable code native-scheme-code)))

(define native-scheme-cmd?
  (lambda (term)
    (and (cons? term)
         (equal? 'native-scheme (first term)))))


;; Die für den Heap notwendigen Definitionen eines

;; Definition einer Zelle die auf dem Heap liegt
(define-record-type
  (heap-cell make-heap-cell heap-cell?)
  (fields
   (immutable variable heap-cell-variable)
   (immutable init-value heap-cell-init-value) ;; change to typed
   ))

;; Der Heap an sich


(define-record-type
  (heap make-heap heap?)
  (fields
   (immutable storage heap-storage)))

;; Heap Alloc Signatur


;; Definition eines heap Element

; Ein Frame besteht aus:
; - Stack
;; Stack für Abstractionen / Closures
; - Umgebung
; - Code
(define-record-type
  (frame make-frame frame?)
  (fields
   (immutable stack frame-stack)
   (immutable fun-stack frame-fun-stack)
   (immutable environment frame-environment)
   (immutable code frame-code)))



; Eine Closure besteht aus:
; - Variable
; - Code
; - Umgebung
(define-record-type
  (closure make-closure closure?)
  (fields
   (immutable variable closure-variable)
   (immutable code closure-code)
   (immutable environment closure-environment)))

; Ein SECD-Zustand besteht aus:
; - Stack
;; Stack für Abstractionen / Closures
; - Umgebung
; - Code
; - Dump
(define-record-type
  (secd make-secd secd?)
  (fields
   (immutable stack secd-stack)
   (immutable fun-stack secd-fun-stack)
   (immutable environment secd-environment)
   (immutable code secd-code)
   (immutable dump secd-dump)
   (immutable heap secd-heap)))
  

; Eine Instruktion für eine primitive Applikation hat folgende
; Eigenschaften:
; - Operator
; - Stelligkeit
(define-record-type
  (prim make-prim prim?)
  (fields
   (immutable operator prim-operator) 
   (immutable arity prim-arity)))



; Eine Instruktion für eine Funktions Applikation hat folgende
; Eigenschaften:

(define-record-type
  (app-fun make-app-fun app-fun?))
  
;; (app-fun-variable var-symbol)
;;(app-fun-code machine-code))

;; Diese Struktur dient zur Darstellung eines "Conditional Branch"
(define-record-type
  (where make-where where?)
  (fields
   (immutable condition where-condition) 
   (immutable if-branch where-if-branch)
   (immutable else-branch where-else-branch)))

;; Definition eines Begin Blocks - Instruktions-Sequenz

(define begin-it?
  (lambda (term)
    (and (cons? term)
         (equal? 'code-block (first term)))))

(define-record-type
  (code-block make-code-block code-block?)
  (fields
   (immutable code code-block-code)))


;; Definition eines  generellen Blocks von Bedingungen

;; Definition der instruction keywords für Bedingungen


(define is-where?
  (lambda (term)
    (and (cons? term)
         (equal? 'where-cond (first term)))))




(define is?
  (lambda (term)
    (and (cons? term)
         (equal? 'is? (first term)))))




(define the-break?
  (lambda (term)
    (and (cons? term)
         (equal? 'break (first term)))))





;; Definition für den Ausstieg aus where-cond
(define-record-type
  (break make-break break?))

;; Definition einer einzelnen enthltenen Bedingung
(define-record-type
  (single-cond make-single-cond single-cond?)
  (fields
   (immutable what single-cond-what)
   (immutable code single-cond-code)))

;; Definition des Bedingungs-Blocks
(define-record-type
  (conditions make-conditions conditions?)
  (fields
   (immutable conds conditions-conds)
   ))
;; define record for begin


; Eine Instruktion für generelle Anwendung
; Eigenschaften:
; - Operator
; - Stelligkeit - Stack
; - Anzahl der auf dem Stack ausgegebenen Ergebnis - Werte
; - Stelligkeit op parameter
(define-record-type
  (op make-op op?)
  (fields
   (immutable code op-code)
   (immutable operation op-operation)
   (immutable params op-params) 
   (immutable stack-arity op-stack-arity)
   (immutable stack-out op-stack-out)
   (immutable parm-arity op-parm-arity)))

; Ein Basiswert ist ein boolescher Wert oder eine Zahl
; Prädikat für Basiswerte

(define base?
  (lambda (term)
    (and (or (boolean? term)
             (char?  term)
             (byte?  term)
             (string? term)
             (const-secd-list? term)
             (procedure? term)
             (number? term)
             (integer? term))
         (not (abstraction? term))
         (not (application? term))
         (not (equal? term 'define))
         (not (equal? term 'lambda))
         (not (equal? term 'fn))
         (not (equal? term 'app-fun))
         (not (primitive? term))
         (not (equal? term 'cond-branch))
         (not (equal? term 'where-cond))
         (not (equal? term 'is?))
         (not (equal? term 'heap-alloc))
         (not (equal? term 'heap-free))
         (not (equal? term 'heap-get-at))
         (not (equal? term 'heap-set-at!)
         (not (equal? term 'native-scheme))
                
         ))))
        
        







;; Funktion zur realen Applikation von Primitives
(define apply-primitive
  (lambda (primitive args)
    (begin
 
      (cond
        ((equal? primitive  'add)
         (+ (first args) (first (rest args))))
        ((equal? primitive 'sub)
         (- (first args) (first (rest args))))
        ((equal? primitive 'mul)
         (* (first args) (first (rest args))))
        ((equal? primitive 'div)
         (/ (first args) (first (rest args))))
         ((equal? primitive 'square)
         (sqrt (first args)))
        ((equal? primitive 'is-eq?)
         (equal? (first args) (first (rest args))))
        ((equal? primitive 'gt?)
         (> (first args) (first (rest args))))
        ((equal? primitive 'lt?)
         (< (first args) (first (rest args))))
        ((equal? primitive 'gt-eq?)
         (>= (first args) (first (rest args))))
        ((equal? primitive 'lt-eq?)
         (<= (first args) (first (rest args))))
        ((equal? primitive '>>)
         (arithmetic-shift (first args)
                           (first (rest args))))
        ((equal? primitive '<<)
         (arithmetic-shift (first args)
                           (* (first (rest args)) -1)))
        ((equal? primitive 'bit-and)
         (bitwise-and (first args)
                      (first (rest args))))
        ((equal? primitive 'bit-ior)
         (bitwise-ior (first args)
                      (first (rest args))))       
        ((equal? primitive 'bit-not)
         (bitwise-not (first args)))
        ((equal? primitive 'bit-xor)
         (bitwise-xor (first args)
                      (first (rest args))))

        ((equal? primitive 'boolean?)
         (boolean? (first args)))
        ((equal? primitive 'integer?)
         (integer? (first args)))
        ((equal? primitive 'number?)
         (number? (first args)))
        ((equal? primitive 'string?)
         (string? (first args)))
        ((equal? primitive 'char?)
         (char? (first args)))
        ((equal? primitive 'byte?)
         (byte? (first args)))
        ((equal? primitive 'list?)
         (list? (first args)))
        ((equal? primitive 'cons?)
         (cons? (first args)))
        ((equal? primitive 'procedure?)
         (procedure? (first args)))
        ))))





; Prädikat für reguläre Applikationen

(define application?
  (lambda (term)
    (and (cons? term)         
         (not (equal? 'lambda (first term)))
         (not (equal? 'lfn (first term)))       
         (not (equal? 'define (first term)))
         (not (primitive? (first term)))        
         (not (fun-application? (first term)))
         (not (where-condition? (first term)))
         (not (is-where? (first term)))
         (not (is? (first term)))
         (not (var-symbol? (first term)))
         (not (base? (first term)))
         (not (the-break? (first term)))
         (not (heap-allocator? (first term)))
         (not (heap-free-fun? (first term)))
         (not (heap-assignment? (first term)))
         (not (heap-getter? (first term)))
         (not (native-scheme-cmd? (first term)))
      
         )))

;;hier muss geprüft werden ob man das tatsächlich braucht
;;======================================
(define smart-first
  (lambda (term)
    (cond
      ((cons? term)
       (first term))
      (else (first (list term)))
      )
    ))



(define smart-rest
  (lambda (term)
    (cond
      ((cons? term)
       (rest term))
      (else (rest (list term)))
      )
    ))    
;;======================================




; Prädikat für Abstraktionen

(define definition? (lambda (term)
                      (equal?  term 'define) ))



;; ======================= DATENTYP DEFINITIONEN (TYPED LANGUAGE) ===============

;; Hier die Definition des list Typs

(define-record-type
  (secd-list make-secd-list secd-list?)
  (fields
   (immutable internal-sign secd-list-internal-sign)
   (mutable length secd-list-length set-secd-list-length!) 
   (mutable data-field secd-list-data-field set-secd-list-data-field!)
   ))
  
(define list-internal-sign ':<lst>)


(define const-secd-list?
  (lambda (term)
    (and (cons? term)
         (equal? '<l> (first term))
         (equal? '<l> (last term)))))



(define extract-list-content
  (lambda (int-list)
    (let ([lst-content
           (take int-list (- (length int-list) 1))])
      lst-content)))

           
(define make-list-from-const
 (lambda (term)   
   (let* ([prefix (first term)]
         [int-list (rest term)]
         [content (extract-list-content int-list)])       
   (make-secd-list list-internal-sign (length content)
                   content))))


;; Typ Operationen

(define safe-first
  (lambda (lst)
    (if (and (list? lst) (not (empty? lst)))
        (first lst)
        lst)))

(define safe-rest
  (lambda (lst)
    (if (and (list? lst) (not (empty? lst)))
        (rest lst)
        lst)))

(define lst-first
  (lambda (term)
    (and (cons? term)
         (equal? 'first (first term)))))

(define lst-rest
  (lambda (term)
    (and (cons? term)
         (equal? 'rest (first term)))))

;; hier die Definitionen zur Typ-Deklaration statische Typisierung

(define-record-type
  (data-type-decl mak-data-type-decl data-type-decl?)
  (fields
   (immutable type-id data-type-decl-type-id)
   (immutable type-name data-type-decl-type-name)
   (immutable len-var data-type-decl-len-var)
   (immutable length data-type-decl-length)))
 



;; ======================= DATATYPE DEFINITIONS (TYPED LANG) ===============


; Prädikat für Abstraktionen

(define abstraction?
  (lambda (term)
    (and (cons? term)
         (equal? 'lambda (first term))
         )))






; Prädikat für primitive Applikationen

(define primitive-application?
  (lambda (term)
    (and (cons? term)
         (primitive? (first term))
         )))

; Prädikat für higher order Applikationen

(define fun-application?
  (lambda (term)
    (and (cons? term)
         (equal? (first term) 'apply-fun)
         )))


; Prädikat für where Bedingungen

(define where-condition?
  (lambda (term)
    (and (cons? term) 
         (equal? (first term) 'cond-branch)
         )))


;;Record für einen abstrakten Syntax-Baum für spätere Benutzung
(define-record-type
  (ast make-ast ast?)
  (fields
   (immutable stack ast-stack)
   (immutable code ast-code) 
   (immutable next ast-next)
   (immutable env ast-env)
   (immutable dump ast-dump)))
  

(define new-stack-element
  (lambda (type val)
    (make-stack-element type base? val)
    ))



(define the-empty-environment empty)

; eine Umgebung um eine Bindung erweitern


;; Bindung zur Umgebung zufügen
(define  extend-environment
  (lambda (env var-symbol var-value)   
    (cons (make-binding var-symbol var-value)
          
          (remove-environment-binding env var-symbol)  )))

;; Hilfsfunktion für obiges


(define remove-environment-binding
  (lambda (environment variable)
    (cond
      ((empty? environment) empty)
      ((cons? environment)
       (if (equal? variable (binding-variable (first environment)))
           (rest environment)
           (cons (first environment)
                 (remove-environment-binding (rest environment) variable)))))))

;; Lookup einr Bindung in der aktuellen Umgebung


(define lookup-act-environment
  (lambda (environment variable)
    (cond
      ((empty? environment) empty)
      ((cons? environment)
       (if (equal? variable (binding-variable (first environment)))
           (binding-value (first environment))
           (lookup-act-environment (rest environment) variable))))))

;;Lookup einer Bindung -> Reihenfolge
;;  - Aktuelle Umgebung
;;  - Den Generationen nach geordnet von innen nach außen werden die in den Frames (Einheit auf dem Dump)
;; abgespeicherten Umgebungen durclaufen bis wir fündig werden oder eben nicht
;; Somit stellen wir damit eine lexikale Art der Bindung dar 

(define lookup-environment
  (lambda (environment dump variable)
    (let* ([act-val (lookup-act-environment environment variable)])
      (cond ((not (empty? act-val)) act-val)
            ((not (empty? dump))  (env-lookup-helper dump variable))
            (else act-val)))))

;; Hilfsfunktion für Lookup
(define env-lookup-helper (lambda (dump variable)
                            (let* ([env (frame-environment (first dump))]
                                   [binding (lookup-act-environment env variable)])                            
                              (if (not (empty? binding))
                                  binding
                                  (if (empty? (rest dump))
                                      empty
                                      (env-lookup-helper (rest dump) variable)
                                      )                            
                                  ))))


;; Lookup einr Bindung in der aktuellen Umgebung


(define lookup-heap-stor
  (lambda (heap-instance-rec variable)
    (let ([heap-instance (heap-storage heap-instance-rec)])
      (cond
        ((empty? heap-instance) empty)
        ((cons? heap-instance)
         (if (equal? variable (heap-cell-variable (first heap-instance)))
             (heap-cell-init-value (first heap-instance))
             (lookup-heap-stor (rest heap-instance) variable)))))))

; eine Umgebung um eine Bindung erweitern


;; Bindung zur Umgebung zufügen
(define  extend-heap-stor
  (lambda (heap-inst-rec var-symbol var-value)
    (let ([heap-inst (heap-storage heap-inst-rec)])
      
      (make-heap (cons (make-heap-cell var-symbol var-value)      
                       (remove-heap-stor-cell heap-inst  var-symbol)))
      )))

;; Hilfsfunktion für obiges


(define remove-heap-stor-cell
  (lambda (heap-instance variable)
    (cond
      ((empty? heap-instance) empty)
      ((cons? heap-instance)
       (if (equal? variable (heap-cell-variable (first heap-instance)))
           (rest heap-instance)
           (cons (first heap-instance)
                 (remove-heap-stor-cell (rest heap-instance) variable)))))))



(define free-heap-stor-cell
  (lambda (heap-instance variable) ;;; write new
    (let ([the-new-stor (remove variable (heap-storage heap-instance)
                                (lambda (var cell)
                                  (equal? var (heap-cell-variable cell) )))])
      (make-heap the-new-stor))))
    
   


;; symbol? unter Ausschluss der "Schlüsselworte"

(define var-symbol?
  (lambda (term)
    (let ([token term])
      (and (symbol? token)
           (not (equal? token 'define))
           (not (equal? token 'lambda))
           (not (equal? token 'fn))
           (not (equal? token 'app-fun))
           (not (primitive? token))
           (not (equal? token 'cond-branch))
           (not (equal? token 'where-cond))
           (not (equal? token 'is?))
           (not (equal? token 'heap-alloc))
           (not (equal? token 'heap-free))
           (not (equal? token 'heap-get-at))
           (not (equal? token 'heap-set-at!))
           (not (equal?  token 'native-scheme))
           (not (base?  token)))
      )))


(define term
  (list
   var-symbol?
   definition?
   fun-application?
   where-condition?
   application?
   abstraction?
    
   primitive-application?
   is-where?
   is?
   heap-allocator?
   heap-getter?
   heap-assignment?
   heap-free-fun?
   native-scheme-cmd?
   base?))



(define instruction
  (list
   base?
   var-symbol?
   op?
   ap?
   tailap?
   prim?    
   define-def?
   app-fun?
   abst?
   where?
   single-cond?
   heap-set-at!?
   heap-get-at?
   heap-alloc?
   single-cond?
   conditions?
   native-scheme?
   ))


;; Debug Funktionalität Deefinitionen Funktionen

;; VM Seite

(define-record-type
  (debug-info make-debug-info debug-info?)
  (fields
   (mutable break-criteria-list debug-info-break-criteria-list set-debug-info-break-criteria-list!)
   (mutable continue-end debug-info-continue-end set-debug-info-continue-end! )
   ))

(define go-next-step
  (lambda (secd-rec)
    (put (list "next code: " (secd-code secd-rec)))
    ))

(define go-next-criteria
  (lambda (secd-rec)
    (put (list "next code: " (secd-code secd-rec)))
    ))

(define show-next-code
  (lambda (secd-rec)
    (put (list "next code: " (secd-code secd-rec)))
    ))

(define env->lists
  (lambda (env-list)
    (let the-loop
      ([intern-lst env-list]
       [result empty])
      (if (empty? intern-lst)
          result
          (the-loop (rest intern-lst)
                    (append result
                            (cons (list (binding-variable
                                         (first intern-lst)))
                                  (list (binding-value
                                         (first intern-lst)))
                                  )))))))
               

(define show-env
  (lambda (secd-rec)
    (display-env (env->lists (secd-environment secd-rec)))))
   
(define show-op-stack
  (lambda (secd-rec)
    (put (list "op stack is: " ((secd-stack secd-rec) 'print-stack)))
    ))

(define show-fun-stack
  (lambda (secd-rec)
    (print (list "fun stack is: " ((secd-fun-stack secd-rec) 'print-stack)))
    ))

(define show-heap
  (lambda (secd-rec)
    (put (list "heap content is: " (secd-heap secd-rec)))
    ))

(define show-dump
  (lambda (secd-rec)
    (put (secd-dump secd-rec))))

(define run-to-end
  (lambda (secd-rec)
    (begin
      (channel-put (debug-channel 'none) "run"))))

(define print-help
  (lambda (dummy)
    (put (string-append "step"             ":: step through code ::"))
    (put (string-append "continue"         ":: continue to next break ::"))
    (put (string-append "show-next-code"   ":: show he next code sequence to be executed ::"))
    (put (string-append "show-env"         ":: show content of environment ::"))
    (put (string-append "show-dump"        ":: show the conrtent of the dump ::"))
    (put (string-append "show-op-stack"    ":: show the operations stack ::"))
    (put (string-append "show-fun-stack"   ":: show the functions stack ::"))
    (put (string-append "show-heap"        ":: show the heap content ::"))
    (put (string-append "dbg-restart"      ":: restart thread of secd for fresh debug ::"))
    (put (string-append "run"              ":: run rest of application ::"))
    (put (string-append "quit"             ":: quit debug ::"))
    (put (string-append "show-help"        ":: show this help ::"))
    ))

(define debug-cmd-pairs
  (list
   (list "step" go-next-step)
   (list "continue" go-next-criteria)
   (list "show-next-code" show-next-code)
   (list "show-env" show-env)
   (list "show-dump" show-dump)
   (list "show-op-stack" show-op-stack)
   (list "show-fun-stack" show-fun-stack)
   (list "show-heap" show-heap)
   (list "run" run-to-end)
   (list "show-help" print-help)
   ))

(define debug-channel
  (lambda (cmd)
  (letrec ([res-channel (cond
    ((equal? cmd 'init)
             (set-dbg-channel! (make-channel))
             the-dbg-channel)
    (else the-dbg-channel))])
    res-channel)))
    



(define debug-on #f)

(define debug-inf (make-debug-info '() #f))


    

(define process-dbg-cmd
  (lambda (dbg-cmd secd-rec)
    (let until-cont ([command dbg-cmd])
      (let* ([cmd-fun-pair (assoc command debug-cmd-pairs)]
             [cmd-fun (if (not (eq? cmd-fun-pair #f))
                          (second cmd-fun-pair)
                          go-next-step)])
         (cmd-fun secd-rec)             
             secd-rec))))

(define check-if-in-crit
  (lambda (the-rec criteria-list)
    (let recur-it ([the-list criteria-list])

      (let ([cmd (if (not (empty? the-list))
                     (first the-list)
                     '())])
        (if (empty? cmd)
            #f
            (if (cmd the-rec)
                #t
                (recur-it (rest the-list))
                ))))))

(define debug-step
  (lambda (secd-rec)
     (if debug-on
    (let ([dbg-cmd (channel-get (debug-channel 'none))])
     
          (begin
            (cond
              ((not (empty? ( debug-info-break-criteria-list debug-inf)))
               (let ([criteria-list (( debug-info-break-criteria-list debug-inf))])
                 (if (check-if-in-crit (first (secd-code secd-rec)) criteria-list)                    
                     (begin
                       (putln "process debug command loop // conditional:")
                       (process-dbg-cmd dbg-cmd secd-rec)
                       (putln "END process loop ->")
                       secd-rec)
                     secd-rec)))                     
              (else  (begin
                       (putln "process debug command loop // stepping mode")
                       (process-dbg-cmd dbg-cmd secd-rec)
                       (putln "END process loop ->")
                       secd-rec
                       ))
                       )))            
          ;; (channel-put dbg-input-channel 'do-input))
          secd-rec
            )))


        
  
;; Debugger Seite

(define send-dbg-cmd
  (lambda (dbg-cmd)
    (channel-put (debug-channel 'none) dbg-cmd)
    ))

(define set-debug-on!
  (lambda (flag)
    (set! debug-on flag)))

