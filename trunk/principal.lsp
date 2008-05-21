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
(defparameter *corpus-location* '"corpus.txt")
(defparameter *diccionario-location* '"subdiccionario.txt")	;; TODO Quitar el sub

;; La key es el numero, value la lista de palabras
(defparameter *corpus* (make-hash-table))
(defparameter *corpus-key* (make-hash-table))


(defparameter *teclado* nil)
(defparameter *profundidad* 15)	

(defvar *palabras-totales* 0);; Numero total de palabras reconocidas hasta el momento.


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
	(set-palabra l (codifica-palabra l))
	(set-key l)
	)))

;; Devuelve la lista de palabras asociada a un numero
;;ordenadas por probabilidad y normalizadas :D
(defun get-palabras (numero)
(ordena-por-probabilidad
;;(normaliza-lista
(calcula-probabilidad
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

;;Inserta una palabra en el corpus actualizando sus repeticiones
(defun set-palabra (palabra numero)
(setf *palabras-totales* (1+ *palabras-totales*))
	(setf (gethash numero *corpus*)
		(set-palabra-aux palabra (gethash numero *corpus*))))

;; Inserta una palabra en una lista, si esta le suma 1 si no esta le da valor 1
(defun set-palabra-aux (palabra lista)
	;(ordena-por-probabilidad ;;esto es muy pesado
(if (assoc palabra lista :test #'equal) ;;si esta en la lista
	(loop for x in lista
		collect
		(if (equal (first x) (string palabra))
		(cons palabra (1+ (rest x))) ;;suma una aparicion
		x))	
	(cons 
		(cons palabra 1) ;;aparicion inicial
		lista)))

(defun set-palabra-compuesta (anterior palabra)
;(format t "insertada : ~a ," (string-concat (string anterior) '" " (string palabra)))
(if (or (null anterior) (string> 'a anterior))
	nil ;;esto es para cortar cuando se termina la frase
	(set-palabra 
		(string-concat (string anterior) '" " (string palabra))
		(codifica-palabra (string-concat (string anterior) (string palabra))))))

(defun set-key-compuesta (anterior palabra)
(if (or (null anterior) (not (stringp a anterior))) ;;TODO arreglar
	nil
	(set-key (string-concat (string anterior) (string palabra)))))

(defun set-key (palabra)
  (let ((numero (codifica-palabra palabra))
	 (lista (descompone-a-numero-aux palabra)))
    ;(format t "~&numero ~a lista ~a"numero lista)
  (loop for x in lista
    do
    (if (member numero (gethash x *corpus-key*))
      nil
      (setf (gethash x *corpus-key*)
	(cons numero (gethash x *corpus-key*)))))))

;; Ordena de mayor a menor una lista de palabras . probabilidades
(defun ordena-por-probabilidad (lista)
	(sort lista #'(lambda (x y) (> (rest x) (rest y)))))

;; FUNCIONES PROBABILISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Devuelve la probabilidad de una palabra, recibe el numero de
;; apariciones de esa palabra

;;Calcula la probabilidad total de cada elemento de la lista 
(defun calcula-probabilidad (lista)
	(loop for x in lista
	collect
	(cons (first x)
		(/ (rest x) *palabras-totales*)))) 

;; Lee el fichero que le pasan por parametro y cuenta las apariciones
;; de las palabras e inicia las probabilidades
(defun entrenamiento (fichero)
(let ((anterior nil))
 (with-open-file (s fichero)
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Fin de Fichero.")
	  	(set-palabra (string-downcase l) (codifica-palabra l)) ;;Se acrualizan las palabras al diccionario
		(set-palabra-compuesta (string-downcase anterior) (string-downcase l))
		(setf anterior (string-downcase l))
		(set-key l) ;; Y al indice de keys
		(set-key-compuesta anterior l)))))

;; Incrementa el numero de apariciones totales, y el de apariciones de la palabra
;; Si la palabra no estaba en el *corpus* la incluye
(defun aprendizaje (palabra)
  (set-palabra palabra (codifica-palabra palabra)))

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
 ; (leer-diccionario) ;;TODO aligera los test
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
      (format canal "~%-Pulse la letra o para escoger otra palabra predicha")
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
	((and (eq tecla 'o) (not (null pred)) (not (null palabra))) ;; ------- Siguiente palabra
	  (format canal "~&~%Introduzca el indice de la palabra deseada: ")
	  (setf indice (1- (read)))
	  (setf palabra (first (nth indice pred)))
	  (print-prediccion canal teclas palabra pred frase))
	((not (eq tecla 'o)) ;; ---------------------------------------------- Pulsar tecla
	  (setf teclas (append teclas (list tecla)))
	  (setf indice 0)
	  (setf pred (prediccion-futura teclas))
	  (setf palabra (first (nth indice pred)))
	  (print-prediccion canal teclas palabra pred frase))))))

;; Funcion auxiliar
(defun print-prediccion (canal teclas palabra pred frase)
  (format canal "~&~%Palabra predicha: ~a~%" palabra)
  (format canal "~&Palabras posibles: ")
  (print-prediccion-aux canal pred)
  (format canal "~&Frase hasta ahora: ~a~%" frase)
  (format canal "~&Teclas pulsadas: ~a~%~%" teclas))

;; Funcion auxiliar
(defun print-prediccion-aux (canal pred)
  (let ((prednorm (normaliza-lista pred)))
	 (loop for x in prednorm
               for i from 1 to (length prednorm) do
	   (format canal "~a.-'~a'(~a) | " i (first x) (rest x)))))

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
