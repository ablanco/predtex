;; Algoritmos de texto predictivo
;; http://www.cs.us.es/cursos/ia2/trabajos/propuesta-2/propuesta-fjmm.html
;; Francisco Jesús Martín Mateos (fjesus@us.es)

(load "aux.lsp")

;; Comentarios sobre la implementación
;; 
;; * 08-04-2008: El trabajo ha de ser desarrollado en el lenguaje de programación Lisp. Las únicas variables globales han de ser las necesarias para almacenar la información probabilística extraida del corpus.

;; Criterios de evaluación
;; 
;; Los criterios de evaluación serán los siguientes:
;; 
;; * Calidad del corpus
;; * Calidad de los criterios probabilísticos utilizados
;; * Calidad de la estructura de datos utilizada para almacenar la información probabilística
;; * Corrección de la implementación
;; * Conocimiento y exposición del trabajo
;; * Claridad y buen estilo de programación Lisp
;; * Efectividad de la fase de evaluación
;; * Coherencia de la fase de aprendizaje con los criterios probabilísticos utilizados para analizar el *corpus* 
;; 
;; También se tendrán en cuenta otros factores como:
;; 
;; * Interfaz gráfica de entrada simulando el teclado de un móvil
;; * Generación del texto de salida de forma progresiva (tal y como ocurre en un teléfono móvil)
;; * Desarrollo interactivo del proceso de entrada/salida
;; * Otras funcionalidades relacionadas y no detalladas en este documento

;; Webs Útiles
;; http://en.wikipedia.org/wiki/Predictive_text



;; ESTRUCTURAS DE DATOS
;;;;;;;;;;;;;;;;;;;;;;;

;; Rutas de los ficheros
(defparameter *corpus-location* '"subcorpus.txt")
(defparameter *diccionario-location* '"diccionario.txt")

;; La key es el numero, value la lista de palabras
;; TODO. mejorar, almacena la lista con valor (hola . 0.95)
(defparameter *corpus* (make-hash-table))
;(defparameter *corpus-key* (make-hash-table))

(defparameter *teclado* nil)

;; Estructura *corpus* donde almacenamos la informacion de la palabra
;; (defstruct *corpus* numero-asociado probabilidad)

;;Inicializa la variable teclado con los valores correspondientes
(defun crea-teclado ()
  (setf teclado
    (list
      (cons (codifica-palabra-a-lista-numeros-consola 'aábc) 2)
      (cons (codifica-palabra-a-lista-numeros-consola 'deéf) 3)
      (cons (codifica-palabra-a-lista-numeros-consola 'ghií) 4)
      (cons (codifica-palabra-a-lista-numeros-consola 'jkl) 5)
      (cons (codifica-palabra-a-lista-numeros-consola 'mnoóñ) 6)
      (cons (codifica-palabra-a-lista-numeros-consola 'pqrs) 7)
      (cons (codifica-palabra-a-lista-numeros-consola 'tuúv) 8)
      (cons (codifica-palabra-a-lista-numeros-consola 'wxyz) 9))))

;; FUNCIONES DE MANEJO DE DATOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Lee el diccionario y lo inserta en la tabla corpus
(defun leer-diccionario()
 (with-open-file (s *diccionario-location*)
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Fin de Fichero.")
    ;(format t "~&Leida ~A~%" l)
	(inserta-palabra l)
;	(inserta-key-corpus-key l)
	)))

;; TODO Hacer XD
;; Carga un texto y lo codifica en memoria
;devuelve una lista de la forma
;(((palabra .1)(palabra . 2)) . 3)
(defun leer-texto (fichero)
(let ((lista '(nil . 0)))
 (with-open-file (s fichero)
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Fin de Fichero.")
      (setf lista (leer-texto-aux l lista))))
	;(setf lista (linea-a-lista-palabras l))))
	lista))

;(leer-texto "corpus.txt")
(defun leer-texto-aux (linea lista)
(cons 
	(if (assoc linea (first lista)) ;si ya pertenece a la lista
	(loop for x in (first lista)
		collect
		(if (equal (string linea) (first x))
		(cons linea (+ (rest x) 1));actualizo su valor
		x))
	(cons
		(cons linea 1) ; la creo
	(first lista)))
(+ 1 (rest lista)))) ;le sumo uno al tamaño


;; Devuelve la lista de palabras asociada a un numero
(defun get-palabras (numero)
(gethash numero *corpus*))

;; Devuelve la probabilidad de una palabra
(defun get-probabilidad (palabra)
	(car
	(loop for x in
		(get-palabras (codifica-palabra-consola palabra))
		when (equal (string-downcase palabra) (first x))
		collect (rest x))))

;;Inserta una palabra actualizando su probalidad y normalizando el resto
;;suma la probabilidad enterior a la nueva
(defun set-palabra (numero palabra probabilidad)
(let ((numero (codifica-palabra palabra)))
	(setf (gethash numero *corpus*)
;		palabra)))
		(normaliza-lista
			(inserta-palabra-en-lista palabra probabilidad 
			(get-palabras numero)))))

;; Ordena de mayor a menor una lista de palabras . probabilidades
(defun ordena-por-probabilidad (lista)
	(sort lista #'(lambda (x y) (< (rest x) (rest y)))))

;;TODO
(defun inserta-palabra-en-lista (palabra probabilidad lista)
;	(ordena-por-probabilidad
		(if (null (get-probabilidad palabra))
		(cons 
			(cons palabra probabilidad)
		lista)
	(loop for x in lista
		collect
		(if (equal (first x) (string palabra))
		(cons palabra probabilidad)
		x	))))

;; Inserta una palabra en la tabla con probabilidad 0
(defun inserta-palabra (palabra)
  (set-palabra (codifica-palabra palabra) palabra (/ 1000 (+ (random 999) 1))))

(defun inserta-key-corpus-key (palabra)
  (let ((numero (codifica-palabra palabra))
	 (lista (descompone-a-numero palabra)))
    ;(format t "~&numero ~a lista ~a"numero lista)
  (loop for x in lista
    do
    (if (member numero (gethash x *corpus-key*))
      nil
      (setf (gethash x *corpus-key*)
	(cons numero (gethash x *corpus-key*)))))))

;; FUNCIONES PROBABILISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun entrenamiento (fichero)
  (let ((lista (leer-texto fichero))
	(total nil))
    (setf total (rest lista))
    (loop for x in (first lista) do
      (set-palabra (first x) (/ (rest x) total)))))

;; Normaliza una lista de palabras . probabilidades
(defun normaliza-lista (lista)
  (let* ((suma (loop for x in lista summing (rest x)))
	  (alfa (/ 1 suma)))
    (loop for x in lista
      collect
      (cons (first x) (* alfa (rest x))))))

(defun prediccion (teclas)
  ;(get-palabras (teclas))
  )

;; FUNCIONES DE CODIFICACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Codifica la palabra a una lista de codigos ascii
;;NOTA. diferencia con la que no es ascii
;;palabra esta en upercase y no es una secuencia
(defun codifica-palabra-a-lista-numeros-consola (palabra)
  (loop for x across (string palabra) collect
    (char-code (char-downcase (character x)))))

;;Codifica una palabra a un numero de teclado
(defun codifica-palabra-consola (palabra)
	(palabra-a-numero-aux
	(reverse
	(loop for x in 
	(codifica-palabra-a-lista-numeros-consola palabra)
	append
   	(loop for tecla in teclado
		when (member x (first tecla))
		collect
		;;(list x tecla))))
		(rest tecla))))))

;;Codifica una palabra a un numero de teclado
;;NOTA. diferencia con la que es ascii
;;palabra esta tal y como la lee del archivo y es una secuencia
(defun codifica-palabra (palabra)
	(palabra-a-numero-aux
	(reverse
  	(loop for x in 
	(loop for x across palabra collect (char-code x))	
		append
   		(loop for tecla in teclado
			when (member x (first tecla))
			collect
			(rest tecla))))))

;;Pasa de una secuencia de codigos ascci a un numero
(defun palabra-a-numero-aux (lista)
	(let* ((tam (1-  (length lista))))
;		(format t "~&lista ~a"lista)
		(loop for i from 0 to tam
		summing
		(* (expt 10 i) (nth i lista)))))

(defun descompone-a-numero (palabra)
	(let ((lista (codifica-palabra-lista palabra)))
	(loop for i from (- (length lista) 3) downto 1 ;;tamaño minimo 3
	collect
	(palabra-a-numero-aux (subseq lista i)))))

(defun codifica-palabra-lista (palabra)
	(reverse
  	(loop for x in 
	(loop for x across palabra collect (char-code (char-downcase x)))	
		append
   		(loop for tecla in teclado
			when (member x (first tecla))
			collect (rest tecla)))))

;; FUNCIONES DE PRESENTACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lanza el programa mostrando los resultados por pantalla
(defun inicio ()
  (lanzador t))

;; Lanza el programa escribiendo los resultados en un fichero
(defun inicio-fichero (fichero)
;;   TODO Probablemente esta opcion no tenga sentido XD
  )

;; Prepara las variables y lanza el algoritmo
(defun lanzador (canal)
  (crea-teclado)
  (format canal "~&Carga del diccionario")
  (leer-diccionario)
  (format canal "~&Proceso de entrenamiento")
  (entrenamiento *corpus-location*)
  (main canal))

;; Bucle principal del algoritmo
(defun main (canal)
  (let ((terminado nil)
	(teclas '())
	(tecla nil))
    (loop while (not terminado) do
      (escribe-teclado canal)
      (format canal "~&~%Pulse un numero (o la letra q para salir): ")
      (setf tecla (read))
      (setf teclas (append teclas (list tecla)))
      (cond
	((eq tecla " ")	;; Nueva palabra
	      (setf teclas '()))
	((eq tecla 'q)	;; Salir
	  (setf terminado t))
	(t
	  (format canal "~&Cadena predicha: ~a~%" (prediccion teclas)))))))

;; Muestra un teclado por <<canal>>
(defun escribe-teclado (canal)
  (escribe-linea canal)
  (format canal "~&+  1  +  2  +  3  +")
  (format canal "~&+     + ABC + DEF +")
  (escribe-linea canal)
  (format canal "~&+  4  +  5  +  6  +")
  (format canal "~&+ GHI + JKL + MNO +")
  (escribe-linea canal)
  (format canal "~&+  7  +  8  +  9  +")
  (format canal "~&+ PQRS+ TUV + WXYZ+")
  (escribe-linea canal)
  (format canal "~&+  *  +  0  +  #  +")
  (format canal "~&+     +     +     +")
  (escribe-linea canal))

(defun escribe-linea (canal)
  (format canal "~&+-----+-----+-----+"))
