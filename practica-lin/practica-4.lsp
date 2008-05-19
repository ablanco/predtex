;;; ==============================================================
;;; Inteligencia Artificial II
;;; Practica 4: Clustering (algoritmo k-medias y EM)
;;; ==============================================================
;;(load "practica-4.lsp")

;;; Esta practica tiene tres partes:
;;; - En la primera parte vamos a implementar en Lisp el algoritmo de
;;;  clustering conocido como de K-medias
;;; - En la segunda parte experimentaremos la implementacion sobre un
;;;   conjunto de datos: el conocido como "iris", una base de datos en la
;;;   que cada instancia refleja una serie de medidas sobre la flor de la
;;;   planta del iris, junto con una clasificacion sobre el tipo de flor
;;;   (fichero iris.lsp) 
;;; - En la segunda parte usaremos el sistema WEKA para aplicar un 
;;;   refinamiento de k-medias basado en un esquema tipo EM
;;;   ("expectation-maximization"), tambien sobre el mismo conjunto de datos. 

;;; La teoria necesaria para hacer esta practica se encuentra en el tema
;;; 4 (transparencias 46 a 60).




;;; ***********************************
;;; Parte I: Implementacion de K-medias     
;;; ***********************************

;;; El algoritmo de K-medias es un algoritmo de clustering que
;;; sirve para clasificar en grupos o clusters una serie de ejemplos
;;; (vectores numericos) que constituyen el conjunto de
;;; datos de entrada. Ademas del conjuto de datos, recibe como
;;; entrada el numero K de clusters de la clasificacion que se pretende
;;; hacer. 

;;; Basicamente, comienza escogiendo K centros y asignando cada elemento
;;; a la clase representada por el centro mas cercano.  Una vez asignado
;;; un cluster a cada ejemplo, la media aritmetica de los ejemplos de
;;; cada cluster se toma como nuevo centro del cluster. Este proceso de
;;; asignacion de clusters y de recalculo de los centros se repite hasta
;;; que se cumple alguna condicion de terminacion (estabilizacion de los
;;; centros, por ejemplo).

;;; El pseudocodigo visto en el tema 4 es el siguiente:

;;; -------------------------------------------------------------------
;;; K-MEDIAS(K,DATOS,distancia)

;; 1. Inicializar m_i (i=1,...,k) (aleatoriamente o con algun criterio
;;    heuristico) 
;; 2. REPETIR (hasta que los m_i no cambien):
;;    2.1 PARA j=1,...,N, HACER: 
;;        Calcular el cluster correspondiente a x_j, escogiendo, de entre
;;        todos los m_i, el m_h tal que distancia(x_j,m_h) sea minima 
;;    2.2 PARA i=1,...,k HACER:
;;        Asignar a m_i la media aritmetica de los datos asignados al
;;        cluster i-esimo      
;; 3. Devolver m_1,...,m_n
;;; ---------------------------------------------------------------------


;;; Los siguientes ejercicios tienen como objetivo final la
;;; implementacion en Lisp del algoritmo de K-medias. 

;;; En nuestro caso, usaremos distancia euclidea, aunque podria usarse
;;; otra funcion de distancia. Esta es la definicion de la distancia
;;; euclidea que usaremos: 

(defun distancia (x y)
  (sqrt (loop for xi in x
	      for yi in y
	      summing
	      (expt (- xi yi) 2))))


;;; Ademas, en lugar de parar cuando se estabilizan los
;;; centros, haremos una version mas simple que realiza un numero
;;; prefijado de iteraciones. 

;;; Para hacer pruebas a medida que se van definiendo las funciones,
;;; usar el siguiente conjunto de datos, en el que aparecen datos dobre
;;; los pesos de una poblacion (en forma de vector unidimensional). Es
;;; de suponer que estos datos corresponden a dos grupos (hombres y
;;; mujeres), y en principio desconocemos a que grupo pertenece cada
;;; peso.

(defparameter *pesos-poblacion*
  '((51.0) (43.0) (62.0) (64.0) (45.0) (42.0) (46.0) (45.0) (45.0) (62.0) (47.0)
   (52.0) (64.0) (51.0) (65.0) (48.0) (49.0) (46.0) (64.0) (51.0) (52.0) (62.0)
   (49.0) (48.0) (62.0) (43.0) (40.0) (48.0) (64.0) (51.0) (63.0) (43.0) (65.0)
   (66.0) (65.0) (46.0) (39.0) (62.0) (64.0) (52.0) (63.0) (64.0) (48.0) (64.0)
   (48.0) (51.0) (48.0) (64.0) (42.0) (48.0) (41.0)))




;;; ---------------------------------------------------------------
;;; Ejercicio 1
;;; La clasificacion de cada ejemplo la vamos a almacenar mediante una
;;; lista de estructuras con dos campos: uno para el elemento en
;;; cuestion y otra para el numero de cluster que tiene
;;; asignado. Definir la estructura correspondiente.
;;; ---------------------------------------------------------------

(defstruct
  (ejemplo
   (:CONC-NAME nil))
  elemento 
  cluster)

;;; ---------------------------------------------------------------
;;; Ejercicio 2
;;; La asignacion de clusters a cada ejemplo durante cada
;;; iteracion, la almacenaremos en un array unidimensional cuyos
;;; elementos son estructuras del tipo anterior. A este array lo
;;; denominaremos "array de clasificacion".
;;; Para comenzar, definir una funcion 
;;; (CLASIFICACION-INICIAL-VACIA EJEMPLOS) 
;;; que recibe un conjunto de ejemplos y crea un array de estructuras
;;; del tipo anterior (con el campo clasificacion sin asignar)
;;;
;;; Ejemplo:
;;; > (setf CLAS (clasificacion-inicial-vacia *pesos-poblacion*))
;;; #(#S(EJEMPLO :ELEMENTO (51.0) :CLUSTER NIL)
;;;   #S(EJEMPLO :ELEMENTO (43.0) :CLUSTER NIL)
;;;   #S(EJEMPLO :ELEMENTO (62.0) :CLUSTER NIL)
;;;   #S(EJEMPLO :ELEMENTO (64.0) :CLUSTER NIL)
;;;   #S(EJEMPLO :ELEMENTO (45.0) :CLUSTER NIL)
;;;    ...)
;;; ---------------------------------------------------------------

(defun clasificacion-inicial-vacia (pesos)
  (make-array (length pesos)
              :initial-contents
              (loop for x in pesos
                    collect
                    (make-ejemplo :elemento x :cluster nil))))


;;; ---------------------------------------------------------------
;;; Ejercicio 3
;;; Los centros que se vayan calculando durante el algoritmo los vamos a
;;; almacenar en un array unidimensional de K componentes (al que
;;; llamaremos "array de centros").
;;; Para definir los centros iniciales, vamos a elegir aleatoriamente K
;;; ejemplos distintos de entre los datos de entrada. 
;;; Se pide definir una funcion (CENTROS-INICIALES EJEMPLOS NUM-CLUSTERS) 
;;; que recibiendo el conjunto de datos de entrada, y el numero total 
;;; de clusters, genera un array inicial de centros.

;;; Por ejemplo (el resultado puede variar):
;;; > (setf CENT (centros-iniciales *pesos-poblacion* 2))
;;; #((64.0) (39.0))
;;; ---------------------------------------------------------------

;;; Sugerencia: puede ser de utilidad la siguiente funcion
;;; (ESCOGE-ALEATORIAMENTE-INDICES K N) que escoge K numeros alatorios
;;; distintos entre 1 y N:

;;(defun escoge-aleatoriamente (k l)
;;  (if (= k 0)
;;      nil
;;    (let ((pos-escogida (random (length l))))
;;      (cons (nth pos-escogida l)
;;	    (escoge-aleatoriamente (- k 1) 
;;				   (append (subseq l 0 pos-escogida)
;;					   (subseq l (1+ pos-escogida))))))))


;;(defun escoge-aleatoriamente-indices (k n)
;;  (escoge-aleatoriamente k (loop for i from 0 to (1- n) collect i)))


;;(defun centros-iniciales (pesos n)
;;  (make-array
;;   n
;;   :initial-contents
;;   (loop for x in
;;         (escoge-aleatoriamente-indices n (length pesos)) ;;puede coger dos centros iguales
;;         collect
;;         (nth x pesos))))


(defun centros-iniciales (pesos n)
  (if (< (length pesos) n)
      (format t "Tamanyo insuficiente de pesos")
    (make-array
     n
     :initial-contents
     (let ((centros nil))
       (loop for i from 1 to n ;;queremos solo dos centros
	     do
	     (setf centros ;;insertamos un centro por iteracion
		   (inserta-centro (random (length pesos))
				   pesos centros)))
       centros))))

(defun inserta-centro (indice pesos lcentros)
;;(format t "centro : ~a lista centros: ~a" (nth indice pesos) lcentros)
  (if (member (nth indice pesos) lcentros :test #'equal)
      (inserta-centro (random (length pesos)) pesos lcentros) ;;si es
                                                              ;;igual volvemos a lazanr
    (cons (nth indice pesos) lcentros))) ;;insertamos el centro diferente

;;(inserta-centro 3 *pesos-poblacion* '((61.0)(64.0)))
;;; > (setf CENT (centros-iniciales *pesos-poblacion* 2))


  
;;; ---------------------------------------------------------------
;;; Ejercicio 4
;;; La siguiente funcion incompleta (CALCULA-CENTRO-MAS-CERCANO E CENTROS) 
;;; recibe como entrada un ejemplo E y un array de CENTROS y devuelve el 
;;; numero de cluster cuyo centro esta mas cercano al ejemplo. 
;;; Por ejemplo:

;;; > (calcula-centro-mas-cercano '(41.0) #((39.0) (45.0))) 
;;; 1
;;; > (calcula-centro-mas-cercano '(64.0) #((39.0) (45.0)))
;;; 2

;;; Completar los trozos de codigo que le faltan a la funcion:

;; (defun calcula-centro-mas-cercano (e centros)
;;   (let ((num-clusters ??????????)
;; 	(mas-cercano ????????)
;; 	(menor-distancia ?????????))
;;     (loop for i from ????? to ?????? do
;; 	  (let ((d (distancia e (aref centros i))))
;; 	    (when ?????????
;; 	      (setf mas-cercano ??????
;; 		    menor-distancia ?????????))))
;;     ???????????))
;;; -------------------------------------------------------------


(defun calcula-centro-mas-cercano (e centros)
   (let ((num-clusters (first (array-dimensions centros)))
 	(mas-cercano 1)
 	(menor-distancia (distancia e (aref centros 0))))
     (loop for i from 0 to (- num-clusters 1) do
 	  (let ((d (distancia e (aref centros i))))
 	    (when (< d menor-distancia)
 	      (setf mas-cercano (+ 1 i)
 		    menor-distancia d))))
     mas-cercano))

;;; ---------------------------------------------------------------
;;; Ejercicio 5
;;; Define la funcion 
;;; (ASIGNA-CLUSTER-A-CADA-EJEMPLO CLASIFICACION CENTROS)
;;; que recibiendo un array de clasificacion CLASIFICACION y un array
;;; de centros CENTROS, actualice destructivamente CLASIFICACION de
;;; manera que en cada ejemplo, el cluster asignado sea el del centro
;;; en CENTROS mas cercano al ejemplo.

;;; Por ejemplo (si CENT y CLAS son los obtenidos en ejemplos anteriores):
 
;; > (asigna-cluster-a-cada-ejemplo CLAS CENT)
;; NIL
;; > CLAS
;; #(#S(EJEMPLO :ELEMENTO (51.0) :CLUSTER 2)
;;   #S(EJEMPLO :ELEMENTO (43.0) :CLUSTER 2)
;;   #S(EJEMPLO :ELEMENTO (62.0) :CLUSTER 1)
;;   #S(EJEMPLO :ELEMENTO (64.0) :CLUSTER 1)
;;   #S(EJEMPLO :ELEMENTO (45.0) :CLUSTER 2)
;;   #S(EJEMPLO :ELEMENTO (42.0) :CLUSTER 2)
;;   #S(EJEMPLO :ELEMENTO (46.0) :CLUSTER 2)
;; ......)
;;; -----------------------------------------------------------------


(defun ASIGNA-CLUSTER-A-CADA-EJEMPLO (clas c)
  (loop for i from 0 to  (1- (first (array-dimensions clas)))
        do
        (setf
         (cluster (aref clas i))
         (calcula-centro-mas-cercano (elemento (aref clas i)) c)))
  clas)

;;; ---------------------------------------------------------------
;;; Ejercicio 6
;;; La siguiente funcion incompleta
;;; (RECALCULA-CENTROS CLASIFICACION CENTROS)
;;; recibiendo un array de clasificacion CLASIFICACION y un array
;;; de centros CENTROS, actualiza destructivamente CENTROS de
;;; manera que el nuevo centro de cada cluster es la media aritmetica
;;; de los elementos que segun la CLASIFICACION, estan en ese
;;; cluster. 

;;; Por ejemplo (si CLAS y CENT con los valores de los ejemplos
;;  anteriores): 
;; > (recalcula-centros CLAS CENT)
;; NIL
;; > CENT
;; #((62.045456) (46.275864))

;;; Se pide completar la funcion adecuadamente:

;; (defun recalcula-centros (clasificacion centros)
;;   (let* ((num-ejemplos ????)
;; 	    (num-clusters ????)
;; 	    (acum ????))  ; Array para acumular las sumas de elementos 
;;                        ; de cada cluster
;; 	    (card-clusters ????)) ; Array para acumular el numero de 
;;                                ; elementos de cada cluster
;;     (loop for j from 0 to (1- num-ejemplos) do
;;              ????????????))
;;     (loop for i from 1 to num-clusters do
;; 	  (setf (aref centros (1- i)) ???????)))
;;; -------------------------------------------------

;;; Las siguientes funciones de operaciones entre vectores pueden ser de
;;; utilidad:

;;; Vector 0, de una dimension DIM dada:
(defun vector-cero (dim)
  (if (= dim 0) nil (cons 0 (vector-cero (1- dim)))))

;;; Suma de vectores:
(defun suma (x y)
  (loop for xi in x
	for yi in y
	collect (+ xi yi)))

;;; Division de los elementos de un vector por un numero:
(defun div (x n)
  (loop for xi in x collect (/ xi n)))

(defun suma-centros (v c)
  (loop for i from 0 to (1- (first (array-dimensions v)))
	when (= (cluster (aref v i)) c)
	summing (first (elemento (aref v i)))))

(defun cuenta-centros (v c)
  (loop for i from 0 to (1- (first (array-dimensions v)))
	when (= (cluster (aref v i)) c)
	summing 1))

;;(suma-centros clas 2)
;;(cuenta-centros clas 1)

(defun recalcula-centros (clasificacion centros)
  (let* ((num-ejemplos (first (array-dimensions clasificacion)))
         (num-clusters (first (array-dimensions centros)))
         (acum (vector-cero num-clusters))  ; Array para acumular las sumas de elementos de cada cluster
         (card-clusters (vector-cero num-clusters))) ; Array para acumular el num de elementos de cada cluster
;;    (loop for j from 0 to (1- num-clusters) do
;;	          ????????????)
    (loop for i from 1 to num-clusters do
          (setf (aref centros (1- i))
		(list ;;TODO chapucen!!!
		(/ (suma-centros clasificacion i)
		   (cuenta-centros clasificacion i)))))
    centros))
;; > (recalcula-centros CLAS CENT)
;; > (asigna-cluster-a-cada-ejemplo CLAS CENT)

;;; ---------------------------------------------------------------
;;; Ejercicio 7
;;; Usando las funciones definidas anteriormente, definir la funcion 
;;; (K-MEDIAS DATOS &key NUM-CLUSTERS ITERACIONES)
;;; que implementa la siguiente version del algoritmo k-medias:
;;; -------------------------------------------------------------------
;;; K-MEDIAS(DATOS K ITERACIONES)

;; 1. Inicializar m_i (i=1,...,k)  
;; 2. PARA L=1,...,ITERACIONES:
;;    2.1 PARA j=1,...,N, HACER: 
;;        Calcular el cluster correspondiente a x_j, escogiendo, de entre
;;        todos los m_i, el m_h tal que distancia(x_j,m_h) sea minima 
;;    2.2 PARA i=1,...,k HACER:
;;        Asignar a m_i la media aritmetica de los datos asignados al
;;        cluster i-esimo      
;; 3. Devolver (m_1,...,m_n) y el cluster finalmente asignado a cada dato
;;; ---------------------------------------------------------------------

 
;;; El algoritmo debe devolver como salida una lista con el array de
;;; clasificacion y con el array de centros finalmente obtenidos.

;;; Por ejemplo: 

;;; > (k-medias *pesos-poblacion* :num-clusters 2 :iteraciones 5)
;; (#(#S(EJEMPLO :ELEMENTO (51.0) :CLUSTER 1)
;;    #S(EJEMPLO :ELEMENTO (43.0) :CLUSTER 1)
;;    #S(EJEMPLO :ELEMENTO (62.0) :CLUSTER 2)
;;    #S(EJEMPLO :ELEMENTO (64.0) :CLUSTER 2)
;;    #S(EJEMPLO :ELEMENTO (45.0) :CLUSTER 1)
;;    #S(EJEMPLO :ELEMENTO (42.0) :CLUSTER 1)
;;    .....)
;;  #((46.8125) (63.63158)))
;;; -------------------------------------------------------------

(defun k-medias (pesos &key num-clusters iteraciones)
  (let* ((centros (centros-iniciales pesos num-clusters))
	 (clasific (asigna-cluster-a-cada-ejemplo
		(clasificacion-inicial-vacia pesos) centros))
	 (return (make-array 2)))
    ;;variables inicializadas
    (loop for i from 1 to iteraciones
	  do
;;	  (format t "~&centros : ~a ~&clasificacion ~a" centros clasific)
	  (setf centros (recalcula-centros clasific centros))
	  (setf clasific 
		(asigna-cluster-a-cada-ejemplo clasific centros)))
    (setf (aref return 0) clasific)
    (setf (aref return 1) centros)
    return))
;;; > (k-medias *pesos-poblacion* :num-clusters 4 :iteraciones 2)

;;; **************************************************
;;; Parte II: experimentacion de k-medias sobre "iris"
;;; **************************************************



;;; ---------------------------------------------------------------
;;; Ejercicio 7
;;; En "iris.lsp" se encuentra uno de los conjuntos de datos mas
;;; conocido y usado como banco de pruebas en aprendizaje automatico. Se
;;; trata de una lista con 150 vectores de datos, cada uno de ellos con
;;; cuatro medidas numericas sobre longitud y anchura de sepalo y petalo
;;; de la flor de la planta iris. Cada vector tiene asignado una de las
;;; tres posibles clasificaciones: setosa, versicolor o virginica. 

;;; La siguiente funcion nos permite validar una clasificacion
;;; cualquiera de los 150 vectores numericos de "iris.lsp", comparandola
;;; con la clasificacion original que aparece en la base de datos.  
;;; Para cada una de las tres clasificaciones originales, cuenta cuantos
;;; ejemplos hay en cada uno de los clusters asignados en la
;;; clasificacion que se le pasa como argumento de entrada.

(defun validacion-iris (clasificacion)
  (let ((posibles-valores '(Iris-setosa Iris-versicolor Iris-virginica))
	(contadores (make-array '(3 3) :initial-element 0)))
    (loop for e in *iris*
	  for i from 0 to (1- (length *iris*)) do
	  (let ((clase-original
		 (position (first (last e)) posibles-valores))
		(clase-asignada (cluster (aref clasificacion i))))
	    (incf (aref contadores clase-original (1- clase-asignada)))))
    (loop for v in posibles-valores
	  for i in '(0 1 2) do
	  (format t "~& Valor original: ~a  ~%     " v)
	  (loop for j in '(0 1 2) do
		(format t "Cluster ~a: ~a " (1+ j) (aref contadores i j))))))

;;; Se pide:
;;; - Obtener, a partir de los datos de "iris.lsp" la lista de vectores
;;;   numericos, SIN el tipo de iris.
;;; - Aplicar el algoritmo de k-medias a esos datos
;;; - Validar la clasificacion obtenida respecto de la clasificacion
;;;   original

;;(load "iris.lsp")
(defun quita-tipo (iris)
  (loop for x in iris
	collect
	(reverse (rest (reverse x)))))
;;(quita-tipo *iris*)
(defun clasificacion-inicial-vacia-iris (iris)
  (clasificacion-inicial-vacia (quita-tipo iris)))

;; (validacion-iris (clasificacion-inicial-vacia-iris *iris*))
;;; > (k-medias (quita-tipo *iris*) :num-clusters 6 :iteraciones 2)




 



;;; ******************************************************************
;;; Parte III: algoritmo EM para clustering y experimentacion con WEKA
;;; ******************************************************************

;;; ---------------------------------------------------------------
;;; Ejercicio 8
;;; Un refinamiento posible de K-medias consiste en suponer que los datos
;;; siguen K distribuciones de media y desviacion tipica desconocida, y
;;; aplicar un algoritmo tipo EM para intentar encontrar una hipotesis
;;; ML sobre esos parametros. Finalmente, cada ejemplo se asigna al
;;; cluster al cual tiene mas probabilidad de pertenecer. En la ultima
;;; parte del tema 4 se describe este algoritmo con detalle.

;;; El algoritmo EM para clustering es uno de los muchos algoritmos de
;;; aprendizaje automatico implementado en el sistema WEKA.

;;; Se pide usar WEKA para aplicar EM sobre la base de datos de iris,
;;; comparando los resultados con los anteriormente obtenidos por
;;; k-medias   
;;; ------------------------------------------------------------------

