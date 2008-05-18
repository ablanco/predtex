;;MANUEL GOMAR ACOSTA
;;(load "practica-3.lsp")
;(defparameter *arcos*   '((indicador bateria_ok) (bateria_ok puede_mover) (bateria_ok objeto_elevable)))

;(defparameter *nodos*  '(Indicador Bateria_ok Puede_mover Objeto_elevable))

;(defparameter *red* (red.aleatoria *nodos* *arcos*))

;;Probabilidad de que el objeto si el indicador de bateria dice si
;;tiene errorres
;(enumeracion '((objeto_elevable si) (indicador si)) *red*)

;;por terminar
;;(algoritmo.eliminacion.variables 'objeto_elevable '((indicardor si)) *red*)




;;; PRACTICA 4
;;; INTELIGENCIA ARTIFICIAL II - 2006/07
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;
;;; Una red bayesiana es un grafo dirigido en el que cada nodo esta
;;; comentado con informacion probabilistica cuantitativa. De modo que:
;;; 1. Los nodos de la red son un conjunto de variables aleatorias.
;;; 2. Un conjunto de enlaces dirigidos (arcos) conectan pares de nodos. Si
;;;    hay un arco desde X hasta Y diremos que X es un padre de Y.
;;; 3. Cada nodo X tiene una distribucion de probabilidad condicionada que
;;;    cuantifica el efecto de los padres sobre el nodo.
;;; 4. El grafo no tiene ciclos dirigidos.
;;;
;;; Determinaremos la topologia de una red mediante dos listas: Una
;;; lista *nodos* donde guardaremos los nombres de los vertices del
;;; grafo y una lista *arcos* de pares de nodos
;;; tomaremos como ejemplo *nodos1* y *arcos1*

(defparameter *nodos1* '(robo terremoto alarma juanllama mariallama))

(defparameter *arcos1* '((robo alarma) (terremoto alarma) (alarma juanllama)
			(alarma mariallama)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1:
;;; Dado un nodo determinar sus padres segun un conjunto de aristas dado
;;; Ejemplo:
;;;   > (padres 'alarma *arcos1*)
;;;     (ROBO TERREMOTO)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun padres (padre arco)
  (loop for arc in arco
        when (eq (second arc) padre)
        collect (first arc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2:
;;; Cada nodo X tiene asociada una tabla de probabilidad
;;; condicionada. Cada tabla la expresaremos de la forma
;;;
;;; ((((Padre_1 V_11) (Padre_2 V_12) ... (Padre_n V_1n)).Prob_1)
;;;  (((Padre_1 V_21) (Padre_2 V_22) ... (Padre_n V_2n)).Prob_2)
;;;          ...          ...                   ... 
;;;  (((Padre_1 V_m1) (Padre_2 V_m2) ... (Padre_n V_mn)).Prob_m))
;;;   
;;; que representa la probabilidad de que ocurra X considerando que
;;; sus padres toman los valores considerados.
;;; En este ejercicio consideraremos que las variables son booleanas y
;;; toman los valores si y no. Se pide definir la funcion (tabla X
;;; Aristas) donde X es un nodo y generaremos aleatoriamente las
;;; distintas probabilidades condicionadas tomando como padres los que
;;; determine el conjunto Aristas . Ejemplo de uso:
;;;
;;;> (tabla 'alarma *arcos1*)
;;;   (ALARMA (((ROBO SI) (TERREMOTO SI)) . 0.217) 
;;;           (((ROBO SI) (TERREMOTO NO)) . 0.77) 
;;;           (((ROBO NO) (TERREMOTO SI)) . 0.113)
;;;           (((ROBO NO) (TERREMOTO NO)) . 0.126))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tabla (nodo aristas)
  (cons nodo (pon-probabilidad (crea-permu (padres nodo aristas)))))

(defun crea-permu (padres)
  (cond
   ((null padres)
    (list nil))
   (t  
    (append
     (loop for p in (crea-permu (cdr padres))
           collect (cons (list (car padres) 'SI) p))
     (loop for p in (crea-permu (cdr padres))
           collect (cons (list (car padres) 'NO) p))))))

(defun pon-probabilidad (lista)
  (loop for elemento in lista
        collect
;        (cons elemento (random 1.0))))
        (cons elemento (/ (random 1000) 1000.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3:
;;; Creacion de una red con valores aleatorios *nodos*
;;; Dada una lista de vertices *nodos* y una lista de aristas, *arcos*
;;; la funcion red.aleatoria genera una red bayesiana aleatoria. La
;;; red es una lista de pares donde el primer elemento es el nombre
;;; del nodo y el segundo es una tabla de probabilidad
;;; aleatoria. Ejemplo de uso:
;;;
;;; > (red.aleatoria *nodos1* *arcos1*)
;;;   ((ROBO      (NIL . 0.556)) 
;;;    (TERREMOTO (NIL . 0.616))
;;;    (ALARMA    (((ROBO SI)  (TERREMOTO SI)) . 0.365)
;;;               (((ROBO SI)  (TERREMOTO NO)) . 0.5) 
;;;               (((ROBO NO)  (TERREMOTO SI)) . 0.009)
;;;               (((ROBO NO)  (TERREMOTO NO)) . 0.084))
;;;   (JUANLLAMA  (((ALARMA SI)) . 0.796) 
;;;               (((ALARMA NO)) . 0.13))
;;;   (MARIALLAMA (((ALARMA SI)) . 0.933) 
;;;               (((ALARMA NO)) . 0.225)))
;;;
;;; Notese que con esta representacion de la red tambien damos de
;;; manera implicita su topologia y podemos recuperar tanto la lista
;;; de nodos como la lista de aristas. Eso lo veremos en el siguiente
;;; ejercicio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun red.aleatoria (nodos arcos)
  (loop for nodo in nodos
        collect
        (tabla nodo arcos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 4:
;;; Definir las funciones nodos.red y arcos.red que reciban una red
;;; bayesiana con la representacion anterior y devuelvan la lista de
;;; nodos y la lista de arcos respectivamente.
;;;
;;; > (setf *red1* (red.aleatoria *nodos1* *arcos1*))
;;;      ...
;;; > (nodos.red *red1*)
;;;   (ROBO TERREMOTO ALARMA JUANLLAMA MARIALLAMA)
;;; > (arcos.red *red1*)
;;;   ((ROBO ALARMA) (TERREMOTO ALARMA) (ALARMA JUANLLAMA) (ALARMA MARIALLAMA))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nodos.red (red)
 (loop for x in red
       collect
       (first x)))

(defun arcos.red (red)
  (loop for x in red
        when (not (null (first (second x))))
        append
        (loop for n in (first (second x))
              collect
              (list (first n)(first x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 5:
;;; Calculo de las probabilidades segun la red bayesiana. Definir la
;;; funcion probabilidad que reciba una lista de pares (X B) donde X
;;; es una variable aleatoria y B es un valor booleano (SI o NO) junto
;;; con una red y devuelva la probabilidad asociada a la distribucion.
;;; Tomando el ejemplo de AIMA tenemos

(setf *red*
     '((ROBO      (NIL . 0.001)) 
       (TERREMOTO (NIL . 0.002))
       (ALARMA    (((ROBO SI)  (TERREMOTO SI)) . 0.95)
                  (((ROBO SI)  (TERREMOTO NO)) . 0.94) 
                  (((ROBO NO)  (TERREMOTO SI)) . 0.29)
                  (((ROBO NO)  (TERREMOTO NO)) . 0.001))
      (JUANLLAMA  (((ALARMA SI)) . 0.90) 
                  (((ALARMA NO)) . 0.05))  
      (MARIALLAMA (((ALARMA SI)) . 0.70) 
                  (((ALARMA NO)) . 0.01))))

(setf *lista1* '((terremoto no) (robo no) (alarma si) (juanllama si)
		 (mariallama si)))

;;; que codifican los datos del ejemplo. La respuesta es 
;;;
;;; > (probabilidad *lista1* *red*)
;;;    6.281113E-4
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun probabilidad (lista red)
 (apply '*
        (loop for x in lista collect
              (probabilidad.condicionada
               x
               (loop for y in (padres (first x) (arcos.red red))
                     collect (assoc y lista))
               red))))

;(setf *aa* (loop for y in (padres 'alarma (arcos.red *red*)) collect (assoc y *lista1*)))
;(probabilidad.condicionada '(ALARMA SI) *aa* *red*)

(defun probabilidad.condicionada (nodo padres red)
  (cond
   ((null padres) ;si no tiene padres nada
    (if (eq (second nodo) 'si)
        (rest (first (rest (assoc (first nodo) red))))
        (- 1 (rest (first (rest (assoc (first nodo) red)))))))
   ((eq (second nodo) 'si) ; si es positivo
    (first
     (loop for x in (rest (assoc (first nodo) red)) ;cogemos de la red la rama que buscamos
           when (equal (first x) padres) ;cuando sea el que buscamos
           collect
           (rest x)))) ;valor de la probabilidad
   (t
    (1-
     (first
      (loop for x in (rest (assoc (first nodo) red))
            when (equal (first x) padres)
            collect
            (rest x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 6:
;;; ALGORITMO DE INFERENCIA POR ENUMERACION
;;;
;;; Recordemos que
;;;
;;;  ---                 ---
;;;  >    P(X|e) = alfa  >   P(X,e,y)
;;;  ---                 ---
;;;                       y
;;;
;;; "Esencialmete se trata de una suma de productos de los elementos
;;; de las tablas de las distribuciones condicionales"
;;; Por ejemplo

(setf *lista2* '((robo si) (juanllama si)(mariallama si)))

(defun enumeracion (lista red)
  (let ((listas 
	 (crea-permu 
	  (remove-si (nodos.red red) 
		     (nodos.lista lista)))))
    (apply '+
	   (loop for l in
		 (loop for x in listas collect (append x lista))
		 collect
		 (probabilidad l red)))))

(defun remove-si (l1 l2)
  (loop for x in l1
    when (not (member x l2))
    collect x))
;(remove-si (nodos.red *red*) (nodos.lista *lista2*))

(defun nodos.lista (l)
  (loop for x in l collect (first x)))
;(nodos.lista *lista2*)


;;; > (enumeracion *lista2* *red*)
;;;     5.922426E-4
;;;
;;; El calculo de la probabilidad de que se haya producido un robo
;;; sabiendo que Juan y Maria han llamado se realiza sumando las
;;; probabilidades para todas los posibles valores para las variables
;;; alarma y terremoto
;;; Del mismo modo, el algoritmo de inferencia por enumeracion para
;;; una unica variable nos da la probabilidad que le asigne la Tabla
;;; de Probabilidad condicionada
;;;
;;; > (enumeracion '((terremoto no)) *red*)
;;;     0.99800026
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALGORITMO DE ELIMINACION DE VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; En el Algoritmo de Inferencia por Enumeracion hay calculos que se
;;; repiten. Podemos mejorar la eficiencia mediante el algoritmo de
;;; Eliminacion de Variables. Las ideas principales del algoritmo son:
;;; * Almacenar los calculos realizados para posteriores usos
;;; * En lugar de numeros multiplicamos tablas de probabilidades. Esas
;;;   tablas se denominan factores.
;;; El algoritmo es el siguiente:
;;;
;;; * Entrada: una v.a. X de consulta, un conjunto de valores observados e
;;; para la variables de evidencia y una red bayesiana.
;;; * Salida: P(X|e)
;;;
;;; 1. Sea RED_E el resultado de eliminar de RED las variables irrelevantes
;;;    para la consulta realizada
;;; 2. Sea FACTORES igual a vacio
;;; 3. Sea VARIABLES el conjunto de variables de RED_E
;;; 4. Sea VAR_ORD el conjunto de VARIABLES ordenado segun un orden de
;;;    eliminacion
;;; 5. PARA cada V en VAR_ORD HACER
;;;    5.1 Sea FACTOR el factor correspondiente a VAR (respecto de e)
;;;    5.2 Incluir FACTOR en FACTORES
;;;    5.3 Si VAR es una variable oculta hacer FACTORES igual
;;;        a AGRUPA(VAR,FACTORES)
;;; 6. Devolver NORMALIZA(MULTIPLICA(FACTORES))
;;;
;;; Vamos a implementar una variante simplificada del algoritmo. Para
;;; ello definiremos primero una serie de predicados auxiliares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun algoritmo.eliminacion.variables (X evidencias red)
(let* 
    ((RED_E (eliminar.variables.irrelevantes X evidencias red))
     (FACTORES nil)
     (VARIABLES (nodos.red RED_E))
     (VAR_ORD (ordenanar.variables VARIABLES RED_E)))
  (loop for V in VAR_ORD
	do
	(setf FACTORES
	      (agrupa
	       v 
	       (incluir.factor (crea.factor v evidencias RED_E)
			       FACTORES))))
  (normaliza(multiplica(FACTORES)))))
	
(defun incluir.factor (f1 f2)
  (if (null f2)
      f1
    (producto.punto f1 f2)))

(defun agrupa (variable factor)
  (suma.factor variable factor))


(defun ordenar.variables (variables red)
  (reverse (ordenar.variables.aux variables red)))
;(ordenar.variables (nodos.red *red*) *red*)

(defun ordenar.variables.aux (variables red)
  (if (null variables)
      nil
      (let ((vsp (variables.sin.padre variables red)))
;	(format t "variables ~a arbol ~a ~%" variables red)
	(if (null vsp)
	    variables ;hojas
	  (append 
	   vsp ;primero las que no tienen padres
	   (ordenar.variables.aux 
	    (remove-list vsp variables) ;las eliminamos de la lista
	    (elimina.variables.red vsp red ))))))) ; las eliminamos de
					; la red
;(ordenar.variables.aux (nodos.red *red*) *red*)

(defun variables.sin.padre (variables red)
  (loop for x in variables
	when (not (padres x (arcos.red red)))
	collect
	x))
;(variables.sin.padre (nodos.red *red*) *red*)

(defun remove-list (lista1 lista2)
  (loop for x in lista2
	when (not (member x lista1))
	collect x))
;(remove-list '(1 2 3) '(1 3 4 5))


(defun elimina.variables.red (variables red)
  (if (listp variables)
   (loop for x in (nodos.red red)
	  when (not (member x variables))
	  collect
	  (assoc x red))
      (loop for x in (nodos.red red)
	    when (not (eq variables x))
	    collect
	    (assoc x red))))
;(elimina.variables.red 'robo *red*)
;(elimina.variables.red '(robo alarma) *red*)

(defun eliminar.variables.irrelevantes (X evidencias red)
  (loop for e in
	(remove-duplicates
	 (append 
	  (antecesor X red) 
	  (loop for x in evidencias collect (first x))))
	collect
	(assoc e red)))
;(eliminar.variables.irrelevantes 'alarma '((robo si)) *red*)
      

(defun antecesor (variable red)
  (cons variable
	  (loop for x in (padres variable (arcos.red red))
		when (not (null x))
		append
		(antecesor x red))))
;(antecesor 'juanllama *red*)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; EJERCICIO 7:
;;; Definir la funcion (crear.factor variable evidencia red) que tome
;;; una variable aleatoria, una lista de variables aleatorias de la
;;; red que tomaremos como evidencia y una red, y devuelva el factor
;;; asociado. El factor depende de si la variable tomada pertenece a
;;; la lista evidencia o si es una variable oculta.
;;;
;;; > (crear.factor 'juanllama '(juanllama mariallama) *red*)
;;;    ((((ALARMA SI)) . 0.9) 
;;;     (((ALARMA NO)) . 0.05))
;;;
;;; > (crear.factor 'alarma '(juanllama mariallama) *red*)
;;;    ((((ALARMA SI) (ROBO SI) (TERREMOTO SI)) . 0.95)
;;;     (((ALARMA SI) (ROBO SI) (TERREMOTO NO)) . 0.94)
;;;     (((ALARMA SI) (ROBO NO) (TERREMOTO SI)) . 0.29)
;;;     (((ALARMA SI) (ROBO NO) (TERREMOTO NO)) . 0.001)
;;;     (((ALARMA NO) (ROBO SI) (TERREMOTO SI)) . 0.050000012)
;;;     (((ALARMA NO) (ROBO SI) (TERREMOTO NO)) . 0.060000002)
;;;     (((ALARMA NO) (ROBO NO) (TERREMOTO SI)) . 0.71000004)
;;;     (((ALARMA NO) (ROBO NO) (TERREMOTO NO)) . 0.999))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun crear.factor (variable evidencia red)
  (if (member variable evidencia)
      (crear.factor.simple variable red)
    (crear.factor.complejo variable red)))

(defun crear.factor.simple (variable red)
  (rest (assoc variable red)))
;(crear.factor.simple 'juanllama *red*))

(defun crear.factor.complejo (variable red)
  (append
   (loop for x in (rest (assoc variable red))
	 collect
	 (cons
	  (cons 
	   (list variable 'SI)
	   (first x))
	  (rest x)))
   (loop for x in (rest (assoc variable red))
	 collect
	 (cons
	  (cons 
	   (list variable 'NO)
	   (first x))
	  (- 1 (rest x))))))
;(crear.factor.complejo 'alarma *red*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; EJERCICIO 8:
;;; Definir la funcion (suma.factor variable factor) que devuelva el
;;; factor obtenido tras sumar el factor dado en la variable.
;;;
;;; > (setf *f1* (crear.factor 'alarma '(juanllama mariallama) *red*))
;;; ...
;;; > *f1*
;;;    ((((ALARMA SI) (ROBO SI) (TERREMOTO SI)) . 0.95)
;;;     (((ALARMA SI) (ROBO SI) (TERREMOTO NO)) . 0.94)
;;;     (((ALARMA SI) (ROBO NO) (TERREMOTO SI)) . 0.29)
;;;     (((ALARMA SI) (ROBO NO) (TERREMOTO NO)) . 0.001)
;;;     (((ALARMA NO) (ROBO SI) (TERREMOTO SI)) . 0.050000012)
;;;     (((ALARMA NO) (ROBO SI) (TERREMOTO NO)) . 0.060000002)
;;;     (((ALARMA NO) (ROBO NO) (TERREMOTO SI)) . 0.71000004)
;;;     (((ALARMA NO) (ROBO NO) (TERREMOTO NO)) . 0.999))
;;; > (suma.factor 'robo *f1*)
;;;    ((((ALARMA NO) (TERREMOTO NO)) . 1.059)
;;;     (((ALARMA NO) (TERREMOTO SI)) . 0.76000005)
;;;     (((ALARMA SI) (TERREMOTO NO)) . 0.941) 
;;;     (((ALARMA SI) (TERREMOTO SI)) . 1.24))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun suma.factor (variable factor)
  (loop for x in
	(crea-permu (remove variable (variables.factor factor)))
	collect
	(cons x 
	      (loop for y in factor
		    when (pertenece.lista x (first y))
		    summing
		    (rest y)))))

;;Nos dice si las todas las condiciones se cumplen en una lista
(defun pertenece.lista (l1 l2)
  (loop for x in l1
        always
        (member x l2 :test 'equal)))
;;;(pertenece.lista '((robo si) (terremoto no)) '((ROBO SI) (TERREMOTO SI) (COSA NO)))

(defun variables.factor (factor)
  (loop for x in (first (first factor))
	collect
	(first x)))
;(variables.factor *f1*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; EJERCICIO 9:
;;; Definir las funciones (producto.punto f1 f2) que devuelva el
;;; factor resultado de hacer el producto punto de los factores f1 y
;;; f2 y una vez definida la funcion, definir (producto.punto.varios
;;; factores) donde factores es una lista de varios factores y devuelva
;;; el resultado de multiplicarlos mediante el producto punto.

(setf *f-a*
         '((((A SI) (B SI)) . 0.3)
	  (((A SI) (B NO)) . 0.7)
	  (((A NO) (B SI)) . 0.9)
	  (((A NO) (B NO)) . 0.1)))

(setf *f-b*
         '((((B SI) (C SI)) . 0.2)
	  (((B SI) (C NO)) . 0.8)
	  (((B NO) (C SI)) . 0.6)
	  (((B NO) (C NO)) . 0.4)))

;; > (producto.punto *f-a* *f-b*)
;;     ((((A SI) (B SI) (C SI)) . 0.06) 
;;      (((A SI) (B SI) (C NO)) . 0.24)
;;      (((A SI) (B NO) (C SI)) . 0.42) 
;;      (((A SI) (B NO) (C NO)) . 0.28)
;;      (((A NO) (B SI) (C SI)) . 0.18) 
;;      (((A NO) (B SI) (C NO)) . 0.72)
;;      (((A NO) (B NO) (C SI)) . 0.06) 
;;      (((A NO) (B NO) (C NO)) . 0.04))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun producto.punto (f1 f2)
 (loop for x in
       (crea-permu (variables.factores f1 f2))
       collect
       (cons
	x
	(*
	 (first
	  (loop for y in f1
		when (pertenece.lista (first y) x)
		collect
		(rest y)))
	 (first
	  (loop for y in f2
		when (pertenece.lista (first y) x)
		collect
		(rest y)))))))
	   
(defun variables.factores (f1 f2)
  (remove-duplicates 
   (append
    (variables.factor f1)
    (variables.factor f2))))
;(crea-permu (variables.factores *f-a* *f-b*))
(loop for y in *f-a* collect(first y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; EJERCICIO 10:
;;; Vamos a implementar una version reducida del algoritmo de
;;; eliminacion de variables, para ello vamos a incluir como dato de
;;; entrada una lista de variables ordenadas donde ya hemos eliminado
;;; las variables irrelevantes.
;;;
;;; Se pide definir (eliminacion X evidencia red variables) donde
;;;
;;;  * X es la variable pregunta
;;;  * evidencia es la evidencia establecida
;;;  * red es la red bayesiana
;;;  * variables es la lista la variables de la red en un orden de eliminacion
;;;
;;; evidencia es una lista de la forma (VAR1 ... VARn)
;;; Consideraremos todas la evidencias positivas
;;;
;;; Ejemplo:
;;;  > (eliminacion 
;;;       'robo 
;;;       '(juanllama mariallama) 
;;;       *red* 
;;;       '(mariallama juanllama alarma terremoto robo))
;;;
;;;  ((((ROBO SI)) . 5.922426E-4) (((ROBO NO)) . 0.0014918577))
;;;
;;; Nota: En esta version simplificada el factor de salida no esta normalizado.
;;;
;;; TRAZA:
;;; 1. Creamos *fmaria* correspondiente a mariallama --> factores = (*fmaria*)
;;; 2. Creamos *fjuan* correspondiente a juanllama 
;;;     --> factores = (*fjuan* *fmaria*)
;;; 3a. Creamos *falarma* correspondiente a alarma
;;;     --> factores = (*falarma* *fjuan* *fmaria*)
;;; 3b. Puesto que alarma es una variable oculta entonces
;;;     multiplicamos los tres factores y obtenemos
;;;                *f-ajm*
;;; 3c. Sumamos en alarma y obtenemos el factor *suma-alarma* 
;;; 3d. La nueva lista de factores tiene solo un elemento
;;;                *nuevo-f*
;;; 4a. Creamos *fterremoto* correspondiente a terremoto y *nuevo-f*
;;;     pasa a ser (*fterremoto* *suma-alarma*)
;;; 4b. Puesto que terremoto es una variable oculta, multiplicamos
;;;     ambos factores y obtenemos *f-tsa*
;;; 4c. Sumamos en terremoto y obtenemos *suma-terremoto*
;;; 4d. La nueva lista de valores solo tiene un elemento: (*suma.terremoto*)
;;; 5. Creamos *frobo*
;;; 6. El resutado final es la multiplicacion de *frobo* y
;;;    *suma.terremoto*. Lo llamamos *final*
;;;
;;; > *final*
;;;   ((((ROBO SI)) . 5.922426E-4) (((ROBO NO)) . 0.0014918577))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;(defun eliminacion (X evidencias red variables_ord)
  


















