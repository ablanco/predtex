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
;; * Coherencia de la fase de aprendizaje con los criterios probabilísticos utilizados para analizar el corpus 
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

;; Direccion del fichero corpus
(defparameter *corpus-location* '"subcorpus.txt")

;; La key es el numero, value la lista de palabras
;; TODO. mejorar, almacena la lista con valor (hola . 0.95)
(defparameter *corpus* (make-hash-table))
(defparameter *corpus-key* (make-hash-table))

(defparameter *teclado* nil)

;; Estructura corpus donde almacenamos la informacion de la palabra
;; (defstruct *corpus* numero-asociado probabilidad)

;; FUNCIONES DE ESTRUCTURAS DE DATOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Crea un teclado, y crea la tabla hash con el vocabulario
(defun inicio ()
	(crea-teclado)
	(leer-archivo))

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


;; Inserta una palabra en la tabla con probabilidad 0
(defun inserta-palabra (palabra)
	(let 
		((numero (codifica-palabra palabra)))
	(setf (gethash numero corpus)
;		palabra)))
		(cons 
			(cons palabra 0) ;;probabilidad inicial 0
			(get-palabras numero)))))

;; FUNCIONES DE EXTRACCIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Lee el vocabulario y lo insterta en la tabla corpus
(defun leer-archivo()
 (with-open-file (s corpus-location)
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Fin de Fichero.")
    ;(format t "~&Leida ~A~%" l)
	(inserta-palabra l)
	(inserta-key-corpus-key l))))

;; Devuelve la lista de palabras asociada a un numero
(defun get-palabras (numero)
(gethash numero corpus))

;; Devuelve la probabilidad de una palabra
(defun get-probabilidad (palabra)
	(car
	(loop for x in
		(get-palabras (codifica-palabra-consola palabra))
		when (equal (string-downcase palabra) (first x))
		collect (rest x))))

;;TODO
;;Asigna una nueva probabilidad a una palabra y la inserta ordenadamente
;;NOTA:
;;Inicialmente todas estaban a 0, luego asumimos que esta previamente ordenado
;(defun set-probabilidad (palabra probabilidad)
;(append
;(loop for x in (get-probabilidad palabra)
;when (not (equal (string-downcase palabra) (first x)))
;until (< probabilidad (rest x))
;collect x)
;(cons palabra probabilidad)
;(loop for x in (get-probabilidad palabra)
;when (not (equal (string-downcase palabra) (first x)))
;until (> probabilidad (rest x))
;collec x)))


;; FUNCIONES DE PROBABILISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO
;;Hay que hacer las funciones probabilisticas

;; FUNCIONES DE CODIFICACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Codifica la palabra a una lista de codigos ascii
;;NOTA. diferencia con la que no es ascii
;;palabra esta en upercase y no es una secuencia
(defun codifica-palabra-a-lista-numeros-consola (palabra)
(loop for x across (string palabra) collect (char-code (char-downcase (character x)))))

;;Codifica una palabra a un numero de teclado
(defun codifica-palabra-consola (palabra)
	(palabra-a-numero 
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
	(palabra-a-numero
	(reverse
  	(loop for x in 
	(loop for x across palabra collect (char-code x))	
		append
   		(loop for tecla in teclado
			when (member x (first tecla))
			collect
			(rest tecla))))))

;;Pasa de una secuencia de codigos ascci a un numero
(defun palabra-a-numero (lista)
	(let* ((tam (1-  (length lista))))
;		(format t "~&lista ~a"lista)
		(loop for i from 0 to tam
		summing
		(* (expt 10 i) (nth i lista)))))

(defun inserta-key-corpus-key (palabra)
(let ((numero (codifica-palabra palabra))
	(lista (descompone-a-numero palabra)))
;(format t "~&numero ~a lista ~a"numero lista)
(loop for x in lista
	do
	(if (member numero (gethash x corpus-key))
	nil
	(setf (gethash x corpus-key)
		(cons numero (gethash x corpus-key)))))))

(defun descompone-a-numero (palabra)
	(let ((lista (codifica-palabra-lista palabra)))
	(loop for i from (- (length lista) 3) downto 1 ;;tamaño minimo 3
	collect
	(palabra-a-numero (subseq lista i)))))

(defun codifica-palabra-lista (palabra)
	(reverse
  	(loop for x in 
	(loop for x across palabra collect (char-code (char-downcase x)))	
		append
   		(loop for tecla in teclado
			when (member x (first tecla))
			collect
			(rest tecla)))))
	

;; FUNCIONES DE PRESENTACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun escribe-teclado (canal)
  (escribe-linea canal)
  (format canal "~&+ 1 -     + 2 - ABC + 3 - DEF +")
  (escribe-linea canal)
  (format canal "~&+ 4 - GHI + 5 - JKL + 6 - MNO +")
  (escribe-linea canal)
  (format canal "~&+ 7 - PQRS+ 8 - TUV + 9 - WXYZ+")
  (escribe-linea canal)
  (format canal "~&+ *       + 0       + #       +")
  (escribe-linea canal))

(defun escribe-linea (canal)
  (format canal "~&+---------+---------+---------+"))
