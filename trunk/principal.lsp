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
(defparameter *corpus-location* '"corpus_sin_tratar/reservoir_dogs-sintildes")
(defparameter *diccionario-location* '"diccionario.txt")

;; La key es el numero, value la lista de palabras
(defparameter *corpus* (make-hash-table))
(defparameter *corpus-compuesto* (make-hash-table))
(defparameter *corpus-key* (make-hash-table))


(defparameter *teclado* nil)
(defparameter *profundidad* 15)	

(defvar *palabras-totales* 0);; Numero total de palabras reconocidas hasta el momento.

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
  (entrenamiento *diccionario-location*))

;; Devuelve la lista de palabras asociada a un numero, ordenadas por probabilidad
(defun get-lista-palabras (numero)
  (ordena-por-probabilidad
   (calcula-probabilidad
    (gethash numero *corpus*))))
;; TODO Poner ejemplo

;; Devuelve una lista de palabras y probabilidades, ordenadas por probalididad

(defun get-lista-palabras-relacionadas (numero)
(let ((lista (get-lista-palabras-relacionadas-aux numero)))
(subseq ;; Tomamos solo una lista de tam maximo *profundidad*
(ordena-por-probabilidad
   (calcula-probabilidad lista))
0 (min *profundidad* (length lista))))) ;; Palabras del corpus-compuesto
;; (get-lista-palabras-relacionadas 771)
;; (("sr." . 705) ("sr. blanco" . 229) ("sr. rosa" . 180) ("sr. rubio" . 118) ("sr. naranja" . 94) ("sr. marrón" . 14) ("sr. azul" . 13))

(defun get-lista-palabras-relacionadas-aux (numero)
 (append
	(gethash numero *corpus*)
	(loop for x in (gethash numero *corpus-key*)
		append
		(get-lista-palabras-relacionadas-aux x))))
;; (get-lista-palabras-relacionadas 783)
;; (("que" . 551/15738) ("puedo" . 2/2623) ("puede" . 11/15738) ("pues" . 3/5246) ("queda" . 1/2623) .....

;; Devuelve la probabilidad de una palabra
(defun get-probabilidad (palabra)
  (let ((palabras (parser palabra)))
    (if (< 1 (length palabras))
	(get-bi-probabilidad (first palabras) palabra)
      (/
       (rest (assoc palabra (gethash (codifica-palabra palabra) *corpus*) :test #' string-equal))
       *palabras-totales*))))
;; TODO Poner ejemplo

;; Devuelve la probabilidad de una palabra compuesta en el modelo bigram
(defun get-bi-probabilidad (palabra bipalabra)
(* (get-probabilidad palabra)
  (/ 
;;    (rest (assoc bipalabra (gethash (codifica-palabra bipalabra) *corpus-compuesto*) :test #' string-equal))
    (rest (assoc bipalabra (gethash (codifica-palabra bipalabra) *corpus*) :test #' string-equal))
   (rest (assoc palabra (gethash (codifica-palabra palabra) *corpus*)
		:test #' string-equal)))))
;; TODO Poner ejemplo
			
;;Inserta una palabra en el corpus actualizando sus repeticiones
(defun add-palabra (palabra)
;; (format t "~& ~t~t~t~tDEBUG add-palabra '~a'" palabra)
(let*
	((palabras (parser palabra))
	(numero (codifica-palabra (first palabras)))
	(numero-compuesto nil))
;; 	 (format t " numero '~a' numero-compuesto '~a'"numero (codifica-palabra palabra))
	(setf *palabras-totales* (1+ *palabras-totales*))
	(setf (gethash numero *corpus*)
		(add-palabra-aux (first palabras) (gethash numero *corpus*)))
	(cond
		((< 1 (length palabras)) ;;Palabra compuesta
			(set-key (first palabras) (second palabras))
;; 			(setf palabra-compuesta (string-concat (first palabras) " " (second palabras)))
			(setf numero-compuesto (codifica-palabra palabra))
			(setf (gethash numero-compuesto *corpus*)
				(add-palabra-aux palabra (gethash numero-compuesto *corpus*))))
		(t
		(set-key (first palabras) nil))))) ;;Palabra simple



;; Inserta una palabra en una lista, si esta le suma 1 a sus apariciones si no esta le da valor 1
(defun add-palabra-aux (palabra lista)
(if (assoc palabra lista :test #'equal) ;; Se comprueba si esta en la lista
	(loop for x in lista 
		collect
		(if (equal (first x) (string palabra)) ;; Si es la buscada
		(cons palabra (1+ (rest x))) ;; Suma una aparicion
		x)) ;; No lo es
	(cons 
		(cons palabra 1) ;; No esta en la lista
		lista)))
;;> (add-palabra-aux "hola" '(("hola" . 1) ("uno" . 1) ("dos" . 2) ("tres". 3)))
;; (("hola" . 2) ("uno" . 1) ("dos" . 2) ("tres" . 3))


;; Incluye en corpus key todas las posibles palabras que se pueden llegar a escribir a partir de la dada
(defun set-key (palabra1 palabra2)
(let ((numero (codifica-palabra palabra1)))
;; (format t "~&DeBUG setkey: '~a' (ind: ~a) (cod: ~a)" palabra1 indice (codifica-palabra palabra1))
;; (if (not (null palabra2)) ;;Si no es una palabra compuesta
;; 		(format t "-> '~a'  (cod: ~a)" (string-concat palabra1 " " palabra2) (codifica-palabra (string-concat palabra1 " " palabra2))))
	(set-key-aux
		palabra1
		(lista-a-numero-aux (subseq (codifica-palabra-lista palabra1) 0 (1- (length palabra1))))
		numero)
	(if (null palabra2) ;;Si no es una palabra compuesta
	nil ;;No se hace nada
	(set-key-aux ;;Para añadir una palabra compuesta
		(string-concat palabra1 " " palabra2)
		numero
		(codifica-palabra (string-concat palabra1 " " palabra2))))))

(defun set-key-aux (palabra indice numero)
(if (member numero (gethash indice *corpus-key*))
      nil
      (setf (gethash indice *corpus-key*)
	(cons numero (gethash indice *corpus-key*)))))



;; Ordena de mayor a menor una lista de (palabras . probabilidades)
(defun ordena-por-probabilidad (lista)
	(sort lista #'(lambda (x y) (> (rest x) (rest y)))))

;; FUNCIONES PROBABILISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calcula la probabilidad total de cada elemento de la lista
(defun calcula-probabilidad (lista)
	(loop for x in lista
	collect
	(cons (first x)
	(get-probabilidad (first x)))))

;; Lee el fichero que le pasan por parametro y cuenta las apariciones
;; de las palabras e inicia las probabilidades
(defun entrenamiento (fichero)
 (with-open-file (s fichero)
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Fin de Fichero.")
;;             (format t "~&DEBUG entrenamieto: '~a' como '~a'"l (separa-en-bipalabras (parser l)))
        (loop for x in (separa-en-bipalabras (parser l))
        do
	(if (< 0 (length (first x)))
        (if (null (second x))
	  	(add-palabra (first x))
	  	(add-palabra (string-concat (first x) " "(second x)))))))))

(defun separa-en-bipalabras (lista)
(loop for i from 0 to (1- (length lista))
collect
(list
(nth i lista)
(nth (1+ i) lista ))))
;; (separa-en-bipalabras '("hola" "amigo" "mio"))
;; (("hola" "amigo") ("amigo" "mio") ("mio" NIL))


;; Incrementa el numero de apariciones totales, y el de apariciones de la palabra
;; Si la palabra no estaba en el *corpus* la incluye y la incluye en el diccionario
(defun aprendizaje (palabra)
  (add-palabra (string-downcase palabra))
(with-open-file (s *diccionario-location* :direction :output :if-exists :append)
(write-line palabra s)))

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
  (get-lista-palabras (lista-a-numero-aux teclas)))

;; Devuelve las palabras que se pueden llegar a escribir con esas
;; pulsaciones de teclas, ordenadas por probabilidad
(defun prediccion-futura (teclas)
  (get-lista-palabras-relacionadas (lista-a-numero-aux teclas)))

;; (funcion-de-evaluacion "254674866 33 83986 77334284861")
(defun funcion-de-evaluacion (cadena)
  (parser-inversa
    (loop for x in (parser cadena) collect
      (first (first (get-lista-palabras-relacionadas (string-to-integer x)))))))

;; (funcion-de-aprendizaje "algoritmo de texto predictivo.")
(defun funcion-de-aprendizaje (cadena)
  (loop for x in (parser cadena) do
    (aprendizaje x)))

;; FUNCIONES DE CODIFICACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Funcion que pasa de una cadena a una lista de cadenas (palabras)
(defun parser (cadena)
(loop for x in
(let ((lista (append (loop for x across cadena collect x) (list (character " ")))) ;;Insertamos espacio al final
		(ind -1)) ;;Indice
	(loop for i from 0 to (length lista)
		when (string= (nth i lista) '#\Space) ;;Si hay un espacio
		collect
		(string-downcase (subseq cadena (1+ ind) (setf ind i)))))
when (< 0 (length x))
collect x))
;;> (parser "hola a   todos soy una   cadena ")
;; ("hola" "a" "todos" "soy" "una" "cadena" "")

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



;;Codifica una palabra a una lista de numeros del teclado
(defun codifica-palabra-lista (palabra)
  	(loop for x across (string-downcase palabra)
	when (assoc (char-code x) teclado :test #'member)
	collect 
	(rest
	(assoc (char-code x) teclado :test #'member))))
;;> (codifica-palabra-lista 'hola)
;; (4 6 5 2)

;; FUNCIONES DE PRESENTACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lanza el programa mostrando los resultados por pantalla
(defun inicio ()
  (configuracion t)
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
  (leer-diccionario)
  (format canal "~&Proceso de entrenamiento~%~%")
  (entrenamiento *corpus-location*))

;; Opciones del programa
(defun configuracion (canal)
  (escoge-corpus canal)
  (format canal "~&Número de palabras predichas (por ejemplo 15): ")
  (setf *profundidad* (read)))

;; Permite escoger el corpus a utilizar
(defun escoge-corpus (canal)
  (let ((lista (directory 'corpus/*)))
    (format canal "~&Escoge el corpus que quieres usar: ")
    (loop for x in lista
	  for i from 1 to (length lista)
	  do
	    (format canal "~&~a.- ~a" i x))
    (format canal "~&Corpus a utilizar: ")
    (setf *corpus-location* (nth (1- (read)) lista))))

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
