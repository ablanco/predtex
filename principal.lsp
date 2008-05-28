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
;; (defparameter *corpus-location* '"corpus/reservoir-dogs.txt")
(defparameter *corpus-location* '"corpus_sin_tratar/reservoir_dogs-sintildes")
(defparameter *diccionario-location* '"subdiccionario.txt")	;; TODO Quitar el sub

;; La key es el numero, value la lista de palabras
(defparameter *corpus* (make-hash-table))
(defparameter *corpus-compuesto* (make-hash-table))
(defparameter *corpus-key* (make-hash-table))


(defparameter *teclado* nil)
(defparameter *profundidad* 15)	

(defvar *palabras-totales* 0);; Numero total de palabras reconocidas hasta el momento.
(defvar *palabras-compuestas-totales* 0);; Numero total de palabras reconocidas hasta el momento.


;;Inicializa la variable teclado con los valores correspondientes
(defun crea-teclado ()
  (setf teclado
    (list
    (cons (codifica-palabra-ascii "'¿?¡()/!)#0,:;-") 0) ;;TODO limpiar el corpus de estos sibolos
    (cons (codifica-palabra-ascii ". 1") 1)
      (cons (codifica-palabra-ascii 'aábc2) 2)
      (cons (codifica-palabra-ascii 'deéf3) 3)
      (cons (codifica-palabra-ascii 'ghií4) 4)
      (cons (codifica-palabra-ascii 'jkl5) 5)
      (cons (codifica-palabra-ascii 'mnoóñ6) 6)
      (cons (codifica-palabra-ascii 'pqrs7) 7)
      (cons (codifica-palabra-ascii 'tuúvü8) 8)
      (cons (codifica-palabra-ascii 'wxyz9) 9))))

;; FUNCIONES DE MANEJO DE DATOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Lee el diccionario y lo inserta en la tabla corpus
(defun leer-diccionario()
 (with-open-file (s *diccionario-location*)
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Fin de Fichero.")
;      (format t "~&Leida ~A~%" l)
	(set-palabra l (codifica-palabra l))
	)))

;; Devuelve la lista de palabras asociada a un numero
;;ordenadas por probabilidad
(defun get-palabras (numero)
(ordena-por-probabilidad
;;(normaliza-lista
(calcula-probabilidad
  (gethash numero *corpus*))))

;;Nos devuelve una lista de palabras y probabilidades ordenadas por probalididad
;;NOTA, las palabras del mismo tamaño que el numero tienen prioridad
(defun get-palabras-relacionadas (numero)
(let ((lista (get-palabras-relacionadas-aux numero)))
(append
	(get-palabras numero)
	(subseq ;;tomamos solo una lista de tamaño máximo *profundidad*
	lista
	0 (min *profundidad* (length lista))))))
;;> (get-palabras-relacionadas-aux 223)
;; (("cada" . 4) ("acerca" . 3) ("cafe," . 3) ("cadaver" . 2) ("acercan" . 2) ....

(defun get-palabras-relacionadas-aux (numero)
(let ((lista (gethash numero *corpus-key*))) ;; Indices del corpus-key
	(ordena-por-probabilidad
		(append
			(loop for x in lista
			append
			(gethash x *corpus*)) ;;Palabras del corpus
 			(loop for x in lista
 			append
			(gethash x *corpus-compuesto*)))))) ;;Palabras del corpus-compuesto


;; Devuelve la probabilidad de una palabra
(defun get-probabilidad (palabra)
(rest (assoc palabra (get-palabras (codifica-palabra palabra)) :test #' string-equal)))

;;Inserta una palabra en el corpus actualizando sus repeticiones
(defun set-palabra (palabra numero)
(cond 	((not (string= " " palabra)) ;;La palabra no es un espacio en blanco
	(set-key palabra numero)
	(setf *palabras-totales* (1+ *palabras-totales*))
		(setf (gethash numero *corpus*)
			(set-palabra-aux palabra (gethash numero *corpus*))))
	(t
	nil)))

;; Inserta una palabra en una lista, si esta le suma 1 si no esta le da valor 1
(defun set-palabra-aux (palabra lista)
(if (assoc palabra lista :test #'equal) ;;si esta en la lista
	(loop for x in lista 
		collect
		(if (equal (first x) (string palabra)) ;;si es
		(cons palabra (1+ (rest x))) ;;suma una aparicion
		x)) ;; no es
	(cons 
		(cons palabra 1) ;;aparicion inicial
		lista)))
;;> (set-palabra-aux "hola" '(("hola" . 1) ("uno" . 1) ("dos" . 2) ("tres". 3)))
;; (("hola" . 2) ("uno" . 1) ("dos" . 2) ("tres" . 3))

;;Inserta una palabra compuesta en el corpus actualizando sus repeticiones
(defun set-palabra-compuesta (anterior palabra)
(if (and 	
	(< 0 (length palabra))(< 0 (length anterior))) ;;no son nulas o blancos ("")
	(let* ;;e.c.o.c
		((palabra-compuesta (string-concat anterior '" " palabra)) ;;palabra-compuesta
		(numero (codifica-palabra palabra-compuesta))) ;;numero de la palabra-compuesta
	;(format t "insertada-palabra : '~a' |" (string-concat (string anterior) '" " (string palabra)))
	(set-key ;;Llamamos a setkey
		palabra-compuesta (codifica-palabra palabra-compuesta))
	(setf *palabras-totales* (1+ *palabras-compuestas-totales*)) ;;inc contador palarbas-compuestas
	(setf (gethash numero *corpus-compuesto*) ;;Actualizamos la referencia
		(set-palabra-aux palabra-compuesta (gethash numero *corpus-compuesto*))))
	nil));;no se hace nada

(defun set-key (palabra numero)
    ;(format t "~&numero ~a lista ~a"numero lista)
  (loop for x in (lista-indices-relacionados palabra)
    do
    (if (member numero (gethash x *corpus-key*))
      nil
      (setf (gethash x *corpus-key*)
	(cons numero (gethash x *corpus-key*))))))

;; Ordena de mayor a menor una lista de palabras . probabilidades
(defun ordena-por-probabilidad (lista)
	(sort lista #'(lambda (x y) (> (rest x) (rest y)))))

;; FUNCIONES PROBABILISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         (format t "~& leido ~a"l)
        (loop for x in (parser l)
        do
	  	(set-palabra ;;Se acrualizan las palabras al diccionario
			x (codifica-palabra x))
		(set-palabra-compuesta anterior x)
		(setf anterior x))))))

;; Incrementa el numero de apariciones totales, y el de apariciones de la palabra
;; Si la palabra no estaba en el *corpus* la incluye
(defun aprendizaje (palabra)
  (set-palabra (string-downcase palabra) (codifica-palabra palabra)))

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
  (get-palabras (lista-a-numero-aux teclas)))

;; Devuelve las palabras que se pueden llegar a escribir con esas
;; pulsaciones de teclas, ordenadas por probabilidad
(defun prediccion-futura (teclas)
  (get-palabras-relacionadas (lista-a-numero-aux teclas)))

;; (funcion-de-evaluacion "254674866 33 83986 77334284861")
(defun funcion-de-evaluacion (cadena)
  (parser-inversa
    (loop for x in (parser cadena) collect
      (first (first (get-palabras-relacionadas (string-to-integer x)))))))

;; (funcion-de-aprendizaje "algoritmo de texto predictivo.")
(defun funcion-de-aprendizaje (cadena)
  (loop for x in (parser cadena) do
    (aprendizaje x)))

;; FUNCIONES DE CODIFICACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Funcion que pasa de una cadena a una lista de cadenas (palabras)
(defun parser (cadena)
(parser-aux
(let ((lista (append (loop for x across cadena collect x) (list (character " ")))) ;;Insertamos espacio al final
		(ind -1)) ;;Indice
	(loop for i from 0 to (length lista)
		when (string= (nth i lista) '#\Space) ;;Si hay un espacio
		collect
		(string-downcase (subseq cadena (1+ ind) (setf ind i)))))))
;;> (parser "hola a   todos soy una   cadena ")
;; ("hola" "a" "todos" "soy" "una" "cadena" "")
(defun parser-aux (cadena)
(if (null cadena)
nil
(loop for x in cadena
when (< 0 (length x))
collect x)))


;; Funcion inversa a parser lista-cadena
(defun parser-inversa (lista)
(apply #'string-concat ;;concateno todas las listas
	(reverse
		(rest ;;le quito el ultimo espacio
			(reverse
				(loop for x in lista
				append
				(list (string x) " "))))))) ;;meto palabra y espacio
;;> (parser-inversa '("hola" "a" "" "" "todos" "soy" "una" "" "" "cadena" ""))
;; "hola a   todos soy una   cadena "

;;Pasa de un string "2222" a un numero 2222
(defun string-to-integer (cadena)
(lista-a-numero-aux
(loop for x across cadena
	collect
(cond
	((equal x '#\0)
	0)
	((equal x '#\1)
	1)
	((equal x '#\2)
	2)
	((equal x '#\3)
	3)
	((equal x '#\4)
	4)
	((equal x '#\5)
	5)
	((equal x '#\6)
	6)
	((equal x '#\7)
	7)
	((equal x '#\8)
	8)
	(t
	9)))))
;;> (string-to-integer "22222")
;; 22222

;; Codifica la palabra a una lista de codigos ascii
(defun codifica-palabra-ascii (palabra)
  (loop for x across (string-downcase palabra) collect
    (char-code (character x))))
;;> (codifica-palabra-ascii 'hola)
;; (104 111 108 97)



;; Codifica una palabra a un numero de teclado
(defun codifica-palabra (palabra)
(lista-a-numero-aux
	(codifica-palabra-lista (string-downcase palabra))))
;;> (codifica-palabra 'hola)
;; 4652


;; Pasa una lista de numeros '(1 2 3 4 5) a un literal '54321
(defun lista-a-numero-aux (lista)
  (let ((tam (1- (length lista)))
	(l (reverse lista)))
    ;(format t "~&lista ~a"lista)
    (loop for i from 0 to tam
      summing
      (* (expt 10 i) (nth i l)))))
;;> (lista-a-numero-aux '(1 2 3 4 5))
;; 12345

;; Nos da una lista de indices relacionados con nuestra palabra
(defun lista-indices-relacionados (palabra)
	(let ((lista (codifica-palabra-lista palabra))) ;obtenemos la lista de numeros
	(loop for i from (- (length lista) 3 ) downto 1 ;;tamaño minimo 3
	collect
	(lista-a-numero-aux ;;pasamos a numero la lista
		(reverse (subseq (reverse lista) i))))))  ;;obtenemos una lista cada vez mas grande :D
;;> (lista-indices-relacionados "hola amigo")
;; (465 4652 46521 465212 4652126 46521264 465212644)

;;Codifica una palabra a una lista de numeros del teclado
(defun codifica-palabra-lista (palabra)
  	(loop for x across (string-downcase palabra)
;;	when (not (= 0 (rest (assoc (char-code x) teclado :test #'member))))
	collect 
	(rest
	(assoc (char-code x) teclado :test #'member))))
;;> (codifica-palabra-lista 'hola)
;; (4 6 5 2)

;; FUNCIONES DE PRESENTACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lanza el programa mostrando los resultados por pantalla
(defun inicio ()
  (carga-datos t)
  (main t))

;; Lanza el programa escribiendo los resultados en un fichero
(defun inicio-fichero (fichero)
;;   TODO Probablemente esta opcion no tenga sentido XD
  )

;; Carga los datos necesarios para ejecutar las funciones
(defun carga-datos (canal)
  (crea-teclado)
  (format canal "~&Carga del diccionario")
  ; (leer-diccionario) ;;TODO aligera los test
  (format canal "~&Proceso de entrenamiento~%~%")
  (entrenamiento *corpus-location*))

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
      (format canal "~%-Pulse la letra o para escoger otra palabra predicha")
      (format canal "~%-Pulse la letra n para incluir una palabra nueva")
      (format canal "~%-Pulse la letra q para salir")
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
        ((eq tecla 'n) ;; ---------------------------------------------------- Nueva palabra
          (format canal "~&~%Escriba la palabra: ")
          (setf palabra (string-downcase (read)))
          (aprendizaje palabra)
          (setf teclas '())
	  (setf frase (append frase (list palabra)))
	  (print-prediccion canal teclas palabra pred frase))
	((and (eq tecla 'o) (not (null pred)) (not (null palabra))) ;; ------- Siguiente palabra
	  (format canal "~&~%Introduzca el indice de la palabra deseada: ")
	  (setf indice (1- (read)))
	  (setf palabra (first (nth indice pred)))
	  (print-prediccion canal teclas palabra pred frase))
	((member tecla '(1 2 3 4 5 6 7 8 9)) ;; ------------------------------ Pulsar tecla
	  (setf teclas (append teclas (list tecla)))
	  (setf indice 0)
	  (setf pred (prediccion-futura teclas))
	  (setf palabra (first (nth indice pred)))
	  (print-prediccion canal teclas palabra pred frase))
        (t
	  (format canal "~&~%Opcion invalida. Escoja otra vez.~%"))))))

;; Funcion auxiliar
(defun print-prediccion (canal teclas palabra pred frase)
  (format canal "~&~%Palabra predicha: ~a~%" palabra)
  (format canal "~&Palabras posibles: ")
  (print-prediccion-aux canal pred)
  (format canal "~&Frase hasta ahora: ~a~%" frase) ;;TODO no la almacena :S
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
  (format canal "~&+  .  + ABC + DEF +")
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
