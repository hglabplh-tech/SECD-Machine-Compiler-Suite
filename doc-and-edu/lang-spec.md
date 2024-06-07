# Definition und Spezifikation der Intermediate Language für ya-secd-machine (Eine PAL ähnliche Sprache zwischen Hochsprache und Byte / Maschinen Code 'Intermediate Compiler Code'

**ANMERKUNG:** Die Syntax ist LISP-ähnlich da spaen wir uns den Parser, da der 'Compiler' in einer Scheme ähnlichen Sprache verfasst ist.

 Die Sprachlemente:

- Abstraktion (lambda) Anonym, mit Identifier
- Application mit Namen , Anonym 
- Primitive Operationen
- Konditionale Verzweigung
- 'Switch / Case' Konstrukt
- Rekursion / Endrekursion


## Die Syntax:

Beispiel: 

```
((lambda (x)
	(lambda (y)
		(mul x y) ) 6) 9)
```

##### Hier ist einigesenthalten fangen wir mal mit der Abstrakion an:

`(lambda (x) ..... ) `

ist eine Abstraktion hier wird die Vorgehensweise bei Aufruf der Abstraktion als Closure (mit Environment...) festglegt.

`(mul x y )` ;; Multipliziere x mit y

ist eine Primitive Applikation... diese Art der Applikation liese sich auch über den Lambda Kalkulus darstellen und ist ein Zugeständnis an Lesbarkeit, Performanz....

Die 6) und 9) bedeuten die Applikation für eine anonyme Abstraktion. Hier wird die Abstraktion als Code genommen es wird eine Umgebung etabliert (-> Closure) und der Aufruf findet statt.

##### Nun zur konditionalen Verzweigung 

Wir befinden uns im Body einer Abstraktion.... 'x' ist eine lexikal gebundene Variable.

```
(cond-branch (== x 5) 
		(mul x x)
		(apply-fun sqrt x))
```

'apply-fun' ist der Aufruf einer Abstraktion über einen Identifier.
1. **(== x 5)** ist die Bedingung: Ergebnis immer TRUE/FALSE 
2. **(mul x x)** ist der 'positiv Zweig' die Bedingung hat das Ergebnis TRUE
3. **(apply-fun sqrt x)** 'negativ Zweig' die Bedingug hat das Egebnis FALSE

damit haben wir in 'if' gebaut und können nicht sequentielle Logik implementieren



##### Hier die Erklärung zu dem 'apply-fun - Namentlicher Aufruf

Zum namentlichen Aufruf müssen wir einem Objekt (Abstzraktion, String , Listze Number.... )
erst mal einen Namen geben können
Hierzu implementiert unsere Sprache das Schlüsselwrt 'define'

Das sieht dann so aus:

```
(define sqrt 
	(lambda (x)
		(mul x x)))
```

nun können wir wie im obigen Beispiel für das 'if' folgendes hinschreiben

`(apply-fun sqrt 5) `
-> damit wird die. Abdtraktion **sqrt** refrenziert uud aufgerufen als instantiierte Closure. Die 5 wird dabeii als Wert an den Identifier **'x**' im Lambda gebunden ... Lexikale Bindung.

##### Etwas ausgearbeitere Verzweigung

Etwas netter ist die mehrfache Verzweigung auf bestimmte Bedingungen:
Das ist dann ein 'switch / case artiges Konstrukt....

```
(where-cond  
       (is? (== x 40)
             (add 3 4))
       (is? (== x 80)
             (sub x  10))
       (is? (== x 11)
              (add (apply-fun allocator 9)
					(mul test-num-var 7))))
```

- Das **'where-cond'** ist die Einleitung zum 'Konditionsblock'. 
- Mit **'is?'** leiten wir eine Bedingung ein wie oben zum Beispiel **(== x 40)** daraufhin kommt der Code der dann ausgeführt werden soll in unserem Fall : **(add 3 4)**...
- Trifft dann eine Bedingung zu werdenn die nachfolgenden Bedingungsprüfungen  ( **'is?'** ) übersprungen (wie beim *break;* im *switch* / case in **C**)

Und fertig ist die mehrfache Verzweigung....


##### Der Heap im Gegensatz zur lexikalen Bindung

Im Gegensatz zur lexikalen Bindung bei drer die Bindung aus der Stellung der Variablen im Code hervorgeht und die eine spezielle Umgebung benötigt um rchtig zu funktionieren ist die Bindung auf dem Heap absolut dynamisch und im Gegensatz zur Umgebung für lexikale Bindungen ist der Heap (zu deutsch Haufen) einfach eine Ansammlng von dynamischen Bindungen die mit alloc angelegt und mit einm free freigegeben werden.

Umgebung für lexikale Bindung:

define -> top level Bsp.: 

`((blubber 5) (sanbber 6))`

lambda 1. Closure
 
`((x 4))`
  
im gesamten
 
`(((x 4)) ((blubber 5) (sanbber 6)))`

das erste Element der Gesamtliste wäre hier die aktuelle Umgebung fdas zweite Element wäre die Umgebung die wiedr gültig wird nachdem der Aufruf Closure beendet wird. (Nach Return)....
Folgt man dieser Logik und hätte noch ein lambda im lambda ->

```
.. Top level defines
(lambda (x) 
   (lambda (y) 
		..... ))    
```
würde das Environment innerhalb des 2. Lambda z.B. so aussehen:

`(((y 8)) ((x 4)) ((blubber 5) (sanbber 6))) `

wobei dan 

`((y 8))`

aktuell wäre und auf dem Rückweg dann erst wieder 

 
`((x 4)) `

und dann am Ende wieder

`((blubber 5) (sanbber 6))`

Beim Heap hingegen ist immer der komplette Heap aktuell
Bsp.: 

`((d 56) (k 7) (b 90))`


1. Die Funktionen für die Verwaltung der Heap Variablen Bindungen

- **heap-alloc** : Einfügen einer neuen Bindung auf dem Heap

- **heap-free** : Freigeben einer Bindung im Heap... danach ist sie nicht mehr erreichbar und gelöscht. 

- **heap-set-at!** : Setzen des Wertes einer Bindung auf dem Heap... das Element muss vorhandn sein

- **heap-get-at** : Holen des Wertes einer Bindung auf dem Heap


### Die Schlüsselworte:

- **define** : top level definition
- **lambda** : Abstraktiuonsdefinition
- **apply-fun** : Aufruf iner Closure mit Namen
- **cond-branch** : 'if' Variante
- **where-cond** : mehr als eine Kondition
- **is?** : eine Kondition unter where-cond
- **heap-alloc** : Erstellen einer Bindung auf dem Heap 
- **heap-free ** : Löschen / Freigeben einer Bindung auf dem Heap
- **heap-set-at!** : Setzen eines Wertes in einer bestehenden Bindung !!!!!Seiten-Effekt!!!! dieses set! lässt meine Implementierung nur für den Heap zu (Abgrenzung gegen Lexikale Bindung (rein funktional)
- **heap-get-at** : Holen eines Wertes einer bestimmten Bindung

###### Berechnungen

- **mul** : Multiplikation
- **add** : Addition
- **div** : Division
- **sub** : Subtraktion

###### Vergleichsoperatoren

- Die üblichen Kandidaten: **==, >= , <=, >, < **

###### Vordefinierte Library Funktionen

- **sqrt** : 2er Potenz

### Mehr als ein Argument bei einer Abstraktion

- ***Beispiel*** : 

-- Definition:

```
(define parm-test 
	(lambda (x y z) 
		(mul x (add y z))))

```
-- Aufruf

`(apply-fun arm-test 7 10 90)`

Da laut der Theorie einer SECD und des Lambda Kalkulus immer nur ein Argument da ist führt der 'Compiler' intern ein Currying durch zu deutsch er Schönfinkelt das ganze ... danach wird aus der Definition folgendes:

```
(define parm-test 
	(lambda (x)
		(lambda (y) 
			(lambda (z)
				  (mul x (add y z)) ))))
```

Und nun haben wir wie vorgeschrieben nur ein Argument pro Abstraktion und trotzdem mehrere Parameter für eine logische Abstraktion. 
		