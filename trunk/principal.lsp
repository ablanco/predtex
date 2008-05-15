;; Algoritmos de texto predictivo
;; http://www.cs.us.es/cursos/ia2/trabajos/propuesta-2/propuesta-fjmm.html
;; Francisco Jesús Martín Mateos (fjesus@us.es)

(load "aux.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALUMNOS                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alejandro Blanco Escudero ;;
;; Manuel Gomar Acosta       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ESTRUCTURAS DE DATOS
;;;;;;;;;;;;;;;;;;;;;;;

;; Rutas de los ficheros
(defparameter *corpus-location* '"corpus.txt")	;; TODO Quitar el sub
(defparameter *diccionario-location* '"subdiccionario.txt")	;; TODO Quitar el sub

;; La key es el numero, value la lista de palabras
;; TODO. mejorar, almacena la lista con valor (hola . 0.95)
(defparameter *corpus* (make-hash-table))
(defparameter *corpus-key* (make-hash-table))

(defparameter *teclado* nil)
(defparameter *profundidad* 15)	;; TODO Quitar el sub

;; Numero de palabras reconocidas hasta el momento, contando tambien las apariciones repetidas
(defvar *palabras-totales* 0)

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
;      (format t "~&Leida ~A~%" l)
	(inserta-palabra l)
	(inserta-key-corpus-key l)
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
	  	(inserta-palabra l) ;;Se acrualizan las palabras al diccionario
		(inserta-key-corpus-key l) ;; Y al indice de keys
      (setf lista (leer-texto-aux l lista))))
	lista))

;(leer-texto "corpus.txt")
(defun leer-texto-aux (linea lista)
(cons 
	(if (assoc linea (first lista) :test #' string-equal) ;si ya pertenece a la lista
	(loop for x in (first lista)
		collect
		(if (string-equal linea (first x))
		(cons (string-downcase linea) (+ 1 (rest x)));actualizo su valor
		x))
	(cons
		(cons (string-downcase linea) 1) ; la creo
	(first lista)))
(+ 1 (rest lista)))) ;le sumo uno al tamaño


;; Devuelve la lista de palabras asociada a un numero
;;ordenadas por probabilidad y normalizadas :D
(defun get-palabras (numero)
(ordena-por-probabilidad
(normaliza-lista
  (gethash numero *corpus*))))

(defun get-indices-palabra (numero)
	(gethash numero *corpus-key*))

(defun get-palabras-relacionadas (numero)
(append
	(get-palabras numero)
	(get-n-palabras-relacionadas-aux
		(get-palabras-relacionadas-aux numero))))

(defun get-n-palabras-relacionadas-aux (lista)
	(loop for x in
		(loop for i from 0 to (min *profundidad* (length lista))
			collect
			(nth i lista))
	when (not (null x))
	collect	x))

(defun get-palabras-relacionadas-aux (numero)
	(ordena-por-probabilidad
	(loop for x in (get-indices-palabra numero)
	append
	(gethash x *corpus*))))

;; Devuelve la probabilidad de una palabra
(defun get-probabilidad (palabra)
	(rest
		(assoc palabra (get-palabras (codifica-palabra-consola palabra)) :test #' string-equal)))

;;Inserta una palabra actualizando su probalidad y normalizando el resto
;;suma la probabilidad enterior a la nueva
(defun set-palabra (palabra probabilidad)
(let ((numero (codifica-palabra palabra)))
	(setf (gethash numero *corpus*)
;		palabra)))
		;(normaliza-lista
			(inserta-palabra-en-lista palabra probabilidad 
			(gethash numero *corpus*)))))

;; Ordena de mayor a menor una lista de palabras . probabilidades
(defun ordena-por-probabilidad (lista)
	(sort lista #'(lambda (x y) (> (rest x) (rest y)))))

;;TODO
(defun inserta-palabra-en-lista (palabra probabilidad lista)
	;(ordena-por-probabilidad ;;esto es muy pesado
		(if (null (get-probabilidad palabra))
		(cons 
			(cons palabra probabilidad)
		lista)
	(loop for x in lista
		collect
		(if (equal (first x) (string palabra))
		(cons palabra probabilidad)
		x))))

;; Inserta una palabra en la tabla con probabilidad 0
(defun inserta-palabra (palabra)
  ;; 1/247051 = 4,0477472262812131908e-6
  ;(set-palabra palabra 0.0000040477472262812131908))	;; TODO - Actualizar si cambia el diccionario
	(set-palabra palabra 0))	;; TODO Lin cree que esto es lo mejor

(defun inserta-key-corpus-key (palabra)
  (let ((numero (codifica-palabra palabra))
	 (lista (descompone-a-numero-aux palabra)))
    ;(format t "~&numero ~a lista ~a"numero lista)
  (loop for x in lista
    do
    (if (member numero (gethash x *corpus-key*))
      nil
      (setf (gethash x *corpus-key*)
	(cons numero (gethash x *corpus-key*)))))))

;; FUNCIONES PROBABILISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Devuelve la probabilidad de una palabra, recibe el numero de
;; apariciones de esa palabra
(defun calcula-probabilidad (n)
  (/ n *palabras-totales*))

;; Lee el fichero que le pasan por parametro y cuenta las apariciones
;; de las palabras e inicia las probabilidades
(defun entrenamiento (fichero)
  (let* ((lista (leer-texto fichero))
	(total (rest lista)))
    (loop for x in (first lista) do
      (if (= total 0)
	(set-palabra (first x) 0)
	(set-palabra (first x) (/ (rest x) total))))))

(defun aprendizaje (palabra)
;;   TODO
  )

;; Normaliza una lista de (palabras . probabilidades)
(defun normaliza-lista (lista)
  (let* ((suma (loop for x in lista summing (rest x)))
	  (alfa (if (= 0 suma)
		  1
		  (/ 1 suma))))
    (loop for x in lista
      collect
      (cons (first x) (* alfa (rest x))))))

;; Consulta el corpus y devuelve la lista de palabras correspondientes
;; a esas pulsaciones de teclas, ordenadas por probabilidad
(defun prediccion (teclas)
  (get-palabras (palabra-a-numero-aux teclas)))

;; Devuelve las palabras que se pueden llegar a escribir con esas
;; pulsaciones de teclas, ordenadas por probabilidad
(defun prediccion-futura (teclas)
  (get-palabras-relacionadas (palabra-a-numero-aux teclas)))

;; FUNCIONES DE CODIFICACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Codifica la palabra a una lista de codigos ascii
;; NOTA. diferencia con la que no es ascii
;; palabra esta en upercase y no es una secuencia
(defun codifica-palabra-a-lista-numeros-consola (palabra)
  (loop for x across (string palabra) collect
    (char-code (char-downcase (character x)))))

;; Codifica una palabra a un numero de teclado
(defun codifica-palabra-consola (palabra)
	(palabra-a-numero-aux
	(loop for x in 
	(codifica-palabra-a-lista-numeros-consola palabra)
	append
   	(loop for tecla in teclado
		when (member x (first tecla))
		collect
		;;(list x tecla))))
		(rest tecla)))))

;; Codifica una palabra a un numero de teclado
;; NOTA. diferencia con la que es ascii
;; palabra esta tal y como la lee del archivo y es una secuencia
(defun codifica-palabra (palabra)
	(palabra-a-numero-aux
  	(loop for x in 
	(loop for x across palabra collect (char-code x))	
		append
   		(loop for tecla in teclado
			when (member x (first tecla))
			collect
			(rest tecla)))))

;; Pasa una lista de numeros '(1 2 3 4 5) a un literal '54321
(defun palabra-a-numero-aux (lista)
  (let ((tam (1- (length lista)))
	(l (reverse lista)))
    ;(format t "~&lista ~a"lista)
    (loop for i from 0 to tam
      summing
      (* (expt 10 i) (nth i l)))))

(defun descompone-a-numero-aux (palabra)
	(let ((lista (codifica-palabra-lista palabra))) ;obtenemos la lista de numeros
	(loop for i from (- (length lista) 3 ) downto 1 ;;tamaño minimo 3
	collect
	(palabra-a-numero-aux ;;pasamos a numero la lista
		(reverse (subseq (reverse lista) i))))))  ;;obtenemos una lista cada vez mas grande :D

(defun codifica-palabra-lista (palabra)
  	(loop for x in 
	(loop for x across palabra collect (char-code (char-downcase x)))	
		append
   		(loop for tecla in teclado
			when (member x (first tecla))
			collect (rest tecla))))

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
  (format canal "~&Proceso de entrenamiento~%~%")
  (entrenamiento *corpus-location*)
  (main canal))

;; TODO - Poder meter palabras que no estan
;; Bucle principal del algoritmo
(defun main (canal)
  (let ((terminado nil)
	(teclas '())
	(tecla nil)
	(pred nil)
	(palabra nil)
	(frase '())
	(indice 0))
    (loop while (not terminado) do
      (escribe-teclado canal)
      (format canal "~&~%Opciones:~%-Pulse un numero del teclado")
      (format canal "~%-Pulse la letra e para un espacio en blanco")
      (format canal "~%-Pulse la letra b para borrar la ultima pulsacion")
      (format canal "~%-Pulse la letra q para salir")
      (format canal "~%-Pulse la letra n para la proxima palabra predicha")
      (format canal "~%~%Su eleccion: ")
      (setf tecla (read))
      (cond
	((eq tecla 'q) ;; ---------------------------------------------------- Salir
	  (setf terminado t))
	((eq tecla 'e) ;; ---------------------------------------------------- Espacio en blanco
	  (aprendizaje palabra)
	  (setf teclas '())
	  (setf frase (append frase (list palabra)))
	  (print-prediccion canal teclas palabra pred frase))
	((eq tecla 'b) ;; ---------------------------------------------------- Borrar ultima pulsacion
	  (if (null teclas)
	    (format canal "~&~%No hay nada que borrar")
	    (setf teclas (reverse (rest (reverse teclas)))))
	  (setf indice 0)
	  (setf pred (prediccion-futura teclas))
	  (setf palabra (first (nth indice pred)))
	  (print-prediccion canal teclas palabra pred frase))
	((and (eq tecla 'n) (not (null pred)) (not (null palabra))) ;; ------- Siguiente palabra
	  (setf indice (+ 1 indice))
	  (setf palabra (first (nth indice pred)))
	  (print-prediccion canal teclas palabra pred frase))
	((not (eq tecla 'n)) ;; ---------------------------------------------- Pulsar tecla
	  (setf teclas (append teclas (list tecla)))
	  (setf indice 0)
	  (setf pred (prediccion-futura teclas))
	  (setf palabra (first (nth indice pred)))
	  (print-prediccion canal teclas palabra pred frase))))))

;; Funcion auxiliar
(defun print-prediccion (canal teclas palabra pred frase)
  (format canal "~&~%Palabra predicha: ~a~%" palabra)
  (format canal "~&Palabras posibles: ~a~%" (prediccion-a-lista-amigable pred))
  (format canal "~&Frase hasta ahora: ~a~%" frase)
  (format canal "~&Teclas pulsadas: ~a~%~%" teclas))

;; Funcion auxiliar
(defun prediccion-a-lista-amigable (pred)
  (loop for x in pred collect
    (first x)))

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

;; Funcion auxiliar
(defun escribe-linea (canal)
  (format canal "~&+-----+-----+-----+"))
