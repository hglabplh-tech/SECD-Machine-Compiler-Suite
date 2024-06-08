#lang racket

;; Diese Definitions Datei implementiert den Compiler zu  unserer SECD(H)
;; VM ... der Gedanke hinter diesen Definitionen ist es Code zu übersetzen
;;der als Basis für Compiler dient die auf dem Lambda Kalkül aufbauen.
;; Ein wesentlicher Aspekt ist zusätzlich, dass der Teil einer Sprache bei dem tatsächlich etwas
;; direkt in 'Machine/Byte" - Code übersetzt.... das ist desswegn attraktiv weil
;; damit bei einer neuen Hardware oder VM weniger Code bei gleichbleibender Schnittstelle
;; ausgtauscht werden muss...
;;

(require (only-in srfi/1 fold)
         "secd-vm-defs.rkt"
         "stack.rkt"
         "operations.rkt"
         "secd-vm-data-types.rkt"
         "debug-out.rkt"
         )
;; Hier die Definitionen, welche nach außen gereicht werden

(provide compile-secd read-analyze-compile)

(define debug-compile-term
  (lambda (code)
    (begin
      (write-object-nl "The code:")
      (if (cons? code)
          (begin
            (write-object-nl code)
          
            (write-object-nl (first code)))
          (write-object-nl code)))))

; Dfinition um die Elemente einer Liste von Listen aneinanderhängen


(define append-lists
  (lambda (the-list)
    (foldr append '() the-list)))


;; Hier wird ein Term in Maschinencode umgesetzt.
;; TODO : Weelche Rolle spielt die Art der Implementierung für die Erkennung
; einer Endrekursion


(define term->machine-code ;; spielt hier diee Reihenfolge der cases eine Rolle?
  (lambda (term)
    (begin
      (debug-compile-term term)
      (cond

        ;; Umsetzung einer 'top level' definition (define)
        ((definition? (smart-first term))
         (let ([identifier (first (rest term))]
               [expression (first (rest (rest term)))])
           (if (base? expression)
               (list (make-define-def identifier
                                      expression))
               (list (make-define-def identifier
                                      (term->machine-code
                                       expression)              
                                      )))))

        ;; Umsetzung des Aufrufs einer nicht anonymen Closure 
        ((fun-application? term)
         (append
          (append-lists
           (map term->machine-code/t (reverse (rest term))))
          (list (make-app-fun)
                )))

        ;; hier wird ein 'Codeblock' übersetzt // entspricht begin in Scheme
        ((begin-it?  term)
         (list (make-code-block 
                (term->machine-code
                 (rest term))))              
         )

        ;; Aufruf des nativen racket via eval
        ((native-scheme-cmd? term)
          (list (make-native-scheme (rest term))
                  (term->machine-code
                 (rest (rest term))))) 

        ;; Umsetzung eines 'if' -> Konditionale Verzweigung
        ((where-condition? term)       
         (list (make-where
                (term->machine-code
                 (first (rest term)))
                (term->machine-code
                 (first (rest (rest term))))
                (term->machine-code
                 (first (rest (rest (rest term)))
                        )))))

   
        ;; Umsetzung der Einleitung eines (cond also wie ein case / switch...
        ((is-where? term)
         (list (make-conditions
                (append-lists
                 (map term->machine-code/t (rest term))))))

        ;; Umsetzung einer Verzweigung innerhalb eines (cond 
        ((is? term)     
         (list(make-single-cond
               (term->machine-code
                (first (rest term)))
               (append
                (append-lists
                 (map term->machine-code/t (rest (rest term))
                      ))))))

        ((the-break? term)
         (append
          (append-lists
           (map term->machine-code (rest term)))
          (list (make-break))))
          

        ;; Umsetzung der Allokation (Instanzierung) einer heyp Variable#n
        ;;Dynamische Bindung
        ((heap-allocator? term)
         (append          
          (append-lists
           (map term->machine-code/t (rest term)))            
          (list (make-heap-alloc))))
            
        ;; Umsetzung der Freigabe einer Bindung auf dem Heap
        ((heap-free-fun? term)
         (append          
          (append-lists
           (map term->machine-code/t (rest term)))             
          (list (make-heap-free))))
      
        ;;Zuweisung (set! )  einer Heap-Variablen
        ((heap-assignment? term)
         (append           
          (append-lists
           (map term->machine-code/t (rest term)))
          (list (make-heap-set-at!))))
     

        ;; holen eines Wertes einer dynamisch gebunden Heap-Variablen
        ((heap-getter? term)
         (append          
          (append-lists
           (map term->machine-code/t (rest term)))            
          (list (make-heap-get-at))))

        ;; Hier wird der Wert eines Symbol auf den Stack gelegt...
        ;; Diese Symbol gilt als Identifier zu einer lexikal gebundenen Variablen
        ((var-symbol? term) (list (push! (list term) )))

        ;; Hier wird aus einer Abstraktion eine Closure instantiert und dann unmittelbar
        ;;  angesteuert
        ((application? term)
         (let ([result (append (term->machine-code (first term))
                               (append (term->machine-code (smart-first (smart-rest term)))
                                       (list (make-tailap))))])       
           result))      

        ;; definition einer Logik die später in einer Closure landet die Definition
        ;; einer Abstraktion ist statisch
        ((abstraction? term)
         (begin
           (write-newline)
           ;; (write-string (symbol->string term ))
           (let* ([params (smart-first(rest term))]
                  [params (reverse params)]
                  [abstract-code  (make-abst
                                   (eval-param params)                      ;; look for the a´bstraction params
                                   (abstraction-code-collect
                                    term->machine-code term))])
             (begin
               ;;    (write-string (symbol->string params ))
               (schoenfinkel-proc (rest-or-empty params)
                                  abstract-code (list abstract-code))))))
      
      
        ;; Hir wird ein 'konstanter' Wert auf den Stack geschoben
        ((base? term)  (list (push! (list term) ))) 

        ;; hier wird eine primitive Operation wie z.B.: mul sub add zur Ausführung
        ;; umgesetzt
        ((primitive-application? term)
         (append
          (append-lists
           (map term->machine-code/t (rest term)))
          (list (make-prim
                 (smart-first term)
                 (length (rest term))))))      
      
        ((and (not (empty? term)) (cons? term)
              (cons? (rest term)) (not (empty? (rest term))))
         (append (term->machine-code/t (first term))
                 (append (term->machine-code/t (first (rest term))))))
        (else (error (list "Error not a valid command" term)))
      
    
        ))))




; Term in Maschinencode übersetzen
;  in nicht-endrekursivem Kontext


(define term->machine-code/t
  (lambda (term)
    (begin
      (debug-compile-term term)
      (cond 

    
        ((definition? (smart-first term))
         (let ([identifier (first (rest term))]
               [expression (first (rest (rest term)))])
           (if (base? expression)
               (list (make-define-def identifier
                                      expression))
               (list (make-define-def identifier
                                      (term->machine-code/t
                                       expression)              
                                      )))))
      
        ;; Umsetzung des Aufrufs einer nicht anonymen Closure 
        ((fun-application? term)
         (append
          (append-lists
           (map term->machine-code/t (reverse (rest term))))
          (list (make-app-fun)
                )))

      
        ((begin-it?  term)
         (list (make-code-block 
                (term->machine-code/t
                 (rest term))))              
         )
      
        ((where-condition? term)       
         (list (make-where
                (term->machine-code/t
                 (first (rest term)))
                (term->machine-code/t
                 (first (rest (rest term))))
                (term->machine-code/t
                 (first (rest (rest (rest term)))
                        )))))

   

          
        ((is-where? term)
         (list (make-conditions  
                (append-lists
                 (map term->machine-code/t (rest term))))))
        
        ((is? term)     
         (list(make-single-cond
               (term->machine-code
                (first (rest term)))
               (append
                (append-lists
                 (map term->machine-code/t (rest (rest term))
                      ))))))

      
                     

        ((the-break? term)
         (append
          (append-lists
           (map term->machine-code/t (rest term)))
          (list (make-break))))

                           
           
        ((heap-allocator? term)
         (append           
          (append-lists
           (map term->machine-code/t (rest term)))            
          (list (make-heap-alloc))))

        ;; Umsetzung der Freigabe einer Bindung auf dem Heap
        ((heap-free-fun? term)
         (append          
          (append-lists
           (map term->machine-code/t (rest term)))             
          (list (make-heap-free))))
            
            
     
        ((heap-assignment? term)
         (append           
          (append-lists
           (map term->machine-code/t (rest term)))
          (list (make-heap-set-at!))))
     

        ((heap-getter? term)
         (append          
          (append-lists
           (map term->machine-code/t (rest term)))            
          (list (make-heap-get-at))))
      
       
        ((var-symbol? term)
         (list (push! (list term) )))
        
        ((application? term)
         (let ([result (append (term->machine-code/t (first term))
                               (append (term->machine-code/t (smart-first (smart-rest term)))
                                       (list (make-ap))))])       
           result))
     
       

      
             
        ((abstraction? term)
         (begin
           (write-newline)
           ;; (write-string (symbol->string term ))
           (let* ([params (smart-first (rest term))]
                  [params (reverse params)]
                  [abstract-code  (make-abst
                                   (eval-param params)                      ;; look for the a´bstraction params
                                   (abstraction-code-collect
                                    term->machine-code/t-t term))])
             (begin
               ;;    (write-string (symbol->string params ))
               (schoenfinkel-proc (rest-or-empty params)
                                  abstract-code (list abstract-code))))))
      
      
        ((base? term)
         (list (push! (list term) )))
    
        ((primitive-application? term)
         (append
          (append-lists
           (map term->machine-code/t (rest term)))
          (list (make-prim
                 (smart-first term)
                 (length (rest term))))))

        ((and (not (empty? term)) (cons? term)
              (cons? (rest term)) (not (empty? (rest term))))
         (append (term->machine-code/t-t (first term))
                 (append (term->machine-code/t-t (smart-first (smart-rest term))))))
      
        (else (error (list "Error not a valid command" term)))
       
        )))
  )



(define term->machine-code/t-t ;; spielt hier diee Reihenfolge der cases eine Rolle?
  (lambda (term)
    (begin
      (debug-compile-term term)
      (cond

    
        ((definition? (smart-first term))
         (let ([identifier (first (rest term))]
               [expression (first (rest (rest term)))])
           (if (base? expression)
               (list (make-define-def identifier
                                      expression))
               (list (make-define-def identifier
                                      (term->machine-code/t
                                       expression)              
                                      )))))
      
     
        ;; Umsetzung des Aufrufs einer nicht anonymen Closure 
        ((fun-application? term)
         (append
          (append-lists
           (map term->machine-code/t (reverse (rest term))))
          (list (make-app-fun)
                )))
        
        ((begin-it?  term)
         (list (make-code-block 
                (term->machine-code/t
                 (rest term))))              
         )

        ((where-condition? term)       
         (list (make-where
                (term->machine-code/t
                 (first (rest term)))
                (term->machine-code/t
                 (first (rest (rest term))))
                (term->machine-code/t
                 (first (rest (rest (rest term)))
                        )))))

        ((is-where? term)
         (list (make-conditions  
                (append-lists
                 (map term->machine-code/t (rest term))))))

                           
                           

      
        ((is? term)     
         (list (make-single-cond
                (term->machine-code
                 (first (rest term)))
                (append
                 (append-lists
                  (map term->machine-code/t (rest (rest term))
                       ))))))

        ((the-break? term)
         (append
          (append-lists
           (map term->machine-code/t (rest term)))
          (list (make-break))))

         
        ((heap-allocator? term)
         (append           
          (append-lists
           (map term->machine-code/t (rest term)))            
          (list (make-heap-alloc))))

        ;; Umsetzung der Freigabe einer Bindung auf dem Heap
        ((heap-free-fun? term)
         (append          
          (append-lists
           (map term->machine-code/t (rest term)))             
          (list (make-heap-free))))
            
            
     
        ((heap-assignment? term)
         (append           
          (append-lists
           (map term->machine-code/t (rest term)))
          (list (make-heap-set-at!))))
     

        ((heap-getter? term)
         (append          
          (append-lists
           (map term->machine-code/t (rest term)))            
          (list (make-heap-get-at))))
      
        ((var-symbol? term)
         (list (push! (list term) )))
       
        ((application? term)
         (let ([result (append (term->machine-code/t (first term))
                               (append (term->machine-code/t (smart-first (smart-rest term)))
                                       (list (make-tailap))))])       
           result))  
       

  

        ((abstraction? term)
         (begin
           (write-newline)
           ;; (write-string (symbol->string term ))
           (let* ([params (smart-first (rest term))]
                  [params (reverse params)]
                  [abstract-code  (make-abst
                                   (eval-param params)                      ;; look for the a´bstraction params
                                   (abstraction-code-collect term->machine-code/t-t term))])
             (begin
            
               (schoenfinkel-proc (rest-or-empty params)
                                  abstract-code (list abstract-code))))))
      
        ((base? term)  (list (push! (list term) )))
 
        ((primitive-application? term)
         (append
          (append-lists
           (map term->machine-code/t (rest term)))
          (list (make-prim
                 (smart-first term)
                 (length (rest term))))))
      
        ((and (not (empty? term)) (cons? term)
              (cons? (rest term)) (not (empty? (rest term))))
         (append (term->machine-code/t (first term))
                 (append (term->machine-code/t (first (rest term))))))
      
        (else (error (list "Error not a valid command" term)))
        ))))



;; Hier wird der Code einer Abstraktion aufgesammelt
(define abstraction-code-collect
  (lambda (term->code term)
    (term->code (smart-first ;;;; ????? prüfen
                 (smart-rest
                  (smart-rest term))))
    ))

;; Definition für eine Abstraktion die mehrere Parameter hat
;; die Parameter / Bindungen werden so heruntergebrochen, dass die
;; ineinander Vrerschachtelten Abstraktionen jeweil wieder nur einen Parameter haben
;; Beispiel:
;; (lambda (x y z) code)
;; wird zu ->
;; (lambda (x)
;;   (lambda (y)
;;     (lambda (z) code)))

(define schoenfinkel-proc
  (lambda (params first-abst result) 
    (if (and (empty? params))
        result
        (let* ([next-abst (make-abst
                           (eval-param params)
                           (append (list first-abst) (list (make-ap))))]
               [res (list next-abst)])  

          (append (schoenfinkel-proc (rest params)  next-abst  res))))))


; Aus Term SECD-Anfangszustand machen

(define inject-secd
  (lambda (term)
    (make-secd empty
               empty
               the-empty-environment          
               (append
                (append-lists
                 (map term->machine-code/t term)))
               empty
               (make-heap (list)))))

; bis zum Ende Zustandsübergänge berechne)


; Evaluationsfunktion zur SECD-Maschine berechnen

;;(check-expect (compile-secd '(+ 1 2)) 3)
;;(check-expect (compile-secd '(((lambda (x) (lambda (y) (+ x y))) 1) 2)) 3)
(define compile-stack (make-stack (list))) ;; Brauchen wir den noch
(define compile-secd
  (lambda (term)    
    (define value 
      (inject-secd term))  
    (begin
      (compile-stack 'push! value )
      (let ([compile-result (make-ast
                             compile-stack
                             (secd-code value)
                             empty
                             (secd-environment value)
                             empty)])
        (begin
          (print "Compiled code is:")
          (print-out-code (ast-code compile-result))
          compile-result
          )
        ))))

(define read-source-content
  (lambda (fname)
    (let* ([file-port (open-input-file fname) ]
           [buffer (read-syntax file-port)])
      (read-the-file  file-port  buffer))))

(define read-the-file
  (lambda (file-port content)    
    (let ([cont-read  (read-syntax file-port)])
      (if (eof-object?  cont-read)
          content
          (read-the-file file-port    
                         (append content
                                 (list cont-read))))
      )))

(define read-analyze-compile
  (lambda (source-name)
    (read-analyze-compile-intern
     (read-source-content source-name)
     (list))))
    

(define  read-analyze-compile-intern
  (lambda (sources result)
    (begin
      (let ([files-content (append-lists (map (lambda (element)                                               
                                                (syntax->datum
                                                 (read-source-content element)))
                                              sources))])
        (make-ast
         compile-stack
         (secd-code (inject-secd
                     files-content))
         empty
         the-empty-environment 
         empty)))))
                   
        
        

