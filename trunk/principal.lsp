;; Algoritmos de texto predictivo
;; http://www.cs.us.es/cursos/ia2/trabajos/propuesta-2/propuesta-fjmm.html
;; Francisco Jesús Martín Mateos (fjesus@us.es)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALUMNOS                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alejandro Blanco Escudero ;;
;; Manuel Gomar Acosta       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ESTRUCTURAS DE DATOS
;;;;;;;;;;;;;;;;;;;;;;;

;; Rutas de los ficheros
(defparameter *corpus-location* nil)
(defparameter *diccionario-location* '"aprendizaje.txt")

;; La key es el numero, value la lista de palabras
(defparameter *corpus* (make-hash-table))
(defparameter *corpus-dobles* (make-hash-table :test 'equal))
(defparameter *corpus-key* (make-hash-table))

(defparameter *teclado* nil)
(defparameter *profundidad* 15)	

(defvar *palabras-totales* 0);; Numero total de palabras reconocidas hasta el momento.

;;Inicializa la variable teclado con los valores correspondientes
(defun crea-teclado ()
  (setf *teclado*
	(list
	 (cons (codifica-palabra-ascii "'¿?¡()/!)#0,:;-") 0)
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

;; Devuelve una lista de palabras que es llegar posible escribir, que empiezan por las
;; pulsaciones dadas, ordenadas por probalididad
(defun get-lista-palabras-relacionadas (numero &optional (palabra-anterior nil) (lista-numeros nil))
  (let ((lista (get-lista-palabras-relacionadas-aux numero)))
    (subseq ;; Tomamos solo una lista de tam maximo *profundidad*
     (ordena-por-probabilidad 
      (calcula-probabilidad 
	(append 
	 lista 
	 (get-posibles-palabras ;;Palabras que puedes llegar a escribir teniendo..
	  palabra-anterior ; palabra-anterior ...
	  lista-numeros)) ;y habiendo pulsado los numeros
      palabra-anterior)) ;; Probabilidad bipalabra
     0
     (min *profundidad* (length lista))))) ;; Palabras del corpus-compuesto
;; (get-lista-palabras-relacionadas 771)
;; ("sr. naranja," . 1/3668) ("sr. naranja." . 11/47684) ("sr. blanco." . 5/23842) ("sr. azul" . 2/11921) ("sr. blanco," . 2/11921) ("sr. rubio." . 1/6812))

;; Devuelve una lista de palabras relacionadas con la palabra anterior y que contengan la lista de numeros
(defun get-posibles-palabras (palabra-anterior lista-numeros)
(loop for palabra in (gethash palabra-anterior *corpus-dobles*)
when (member 
      lista-numeros 
      (list (subseq (codifica-palabra-lista (first palabra)) 0 (min (length lista-numeros) (length (first palabra))))) :test #'equal)
collect palabra))
;; (get-posibles-palabras (gethash "una" *corpus-dobles*) '(7 2))
;; (("palabra" . 1) ("rama" . 1) ("rata" . 1) ("pasada" . 1) ("sanitaria" . 1)
;;  ("pancita" . 1) ("panza" . 4) ("pareja" . 1) ("pata" . 1) ("pasta" . 1))

;; Funcion auxiliar
(defun get-lista-palabras-relacionadas-aux (numero)
  (append
   (gethash numero *corpus*) ;; Las que se pueden escribir con esas pulsaciones
   (loop for x in (gethash numero *corpus-key*) append ;; Las que se pueden llegar a escribir con esas pulsaciones
	 (gethash x *corpus*))))

;;Inserta una palabra en el corpus actualizando sus repeticiones
(defun add-palabra (p1 &optional (p2 nil))
  (let* ((palabra (sin-punto p1))
	 (palabra-anterior (sin-punto p2))
	 (numero (codifica-palabra palabra)))
    (setf *palabras-totales* (1+ *palabras-totales*))
    (setf (gethash numero *corpus*)
	  (add-palabra-aux palabra (gethash numero *corpus*)))
    (cond
     ((not (null palabra-anterior)) ;; Palabra doble
      (setf (gethash palabra-anterior *corpus-dobles*)
	    (add-palabra-aux palabra (gethash palabra-anterior *corpus-dobles*)))))
    (set-key palabra)))

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
(defun set-key (palabra1)
  (let ((numero (codifica-palabra palabra1)))
    (set-key-aux
     palabra1
     (lista-a-numero-aux (subseq (codifica-palabra-lista palabra1) 0 (1- (length palabra1))))
     numero)))

(defun set-key-aux (palabra indice numero)
  (if (member numero (gethash indice *corpus-key*))
      nil
    (setf (gethash indice *corpus-key*)
	  (cons numero (gethash indice *corpus-key*)))))

;; Devuelve la probabilidad de una palabra
(defun get-probabilidad (palabra &optional (palabra-anterior nil))
  (if (null palabra-anterior)
      (/ ;; Unigram
       (rest (assoc palabra (gethash (codifica-palabra palabra) *corpus*) :test #' string-equal))
       *palabras-totales*)
    (if (null (rest (assoc palabra (gethash palabra-anterior *corpus-dobles*) :test #' string-equal)))
	0 ;; Caso en el q palabra no aparece nunca detras de palabra-anterior
      (/ ;; Bigram
       (rest (assoc palabra (gethash palabra-anterior *corpus-dobles*) :test #' string-equal))
       (rest (assoc palabra-anterior (gethash (codifica-palabra palabra-anterior) *corpus*) :test #' string-equal))))))

;; FUNCIONES PROBABILISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calcula la probabilidad total de cada elemento de la lista
(defun calcula-probabilidad (lista &optional (palabra-anterior nil))
  (loop for x in lista collect
	(cons (first x) (get-probabilidad (first x) palabra-anterior))))

;; Ordena de mayor a menor una lista de (palabras . probabilidades)
(defun ordena-por-probabilidad (lista)
  (sort lista #'(lambda (x y) (> (rest x) (rest y)))))
 
;; Lee el fichero que le pasan por parametro y cuenta las apariciones
;; de las palabras e inicia las probabilidades
(defun entrenamiento (fichero)
  (let ((anterior nil)
	(x nil))
    (with-open-file (s fichero)
		    (do ((l (read-line s) (read-line s nil 'eof)))
			((eq l 'eof) "Fin de Fichero.")
			;(format t "~%DEBUG leido : '~a' " (parser l))
			(loop for elem in (parser l) do
			      (setf x (limpieza elem))
			      (cond
				((< 0 (length x))
				  (add-palabra x anterior)
				  (if (punto-al-final x)
				    (setf anterior nil)
				    (setf anterior x)))))))))

;; Incrementa el numero de apariciones totales, y el de apariciones de la palabra
;; Si la palabra no estaba en el *corpus* la incluye y la incluye en el diccionario
(defun aprendizaje (palabra)
  (add-palabra (string-downcase palabra)))

;; Normaliza una lista de (palabras . probabilidades)
(defun normaliza-lista (lista)
  (let* ((suma (loop for x in lista summing (rest x)))
	 (alfa (if (= 0 suma)
		   1
		 (/ 1 suma))))
    (loop for x in lista
	  collect
	  (cons (first x) (* alfa (rest x))))))

;; Devuelve las palabras que se pueden llegar a escribir con esas
;; pulsaciones de teclas, ordenadas por probabilidad
(defun prediccion (teclas &optional (palabra-anterior nil))
  (get-lista-palabras-relacionadas (lista-a-numero-aux teclas) palabra-anterior teclas))

;; (funcion-de-evaluacion "254674866 33 83986 77334284861")
(defun funcion-de-evaluacion (cadena)
  (parser-inversa
   (loop for x in (parser cadena) collect
	 (first (first (get-lista-palabras-relacionadas (string-to-integer x)))))))

;; (funcion-de-aprendizaje "algoritmo de texto predictivo.")
(defun funcion-de-aprendizaje (cadena)
  (loop for x in (parser cadena) do
	(aprendizaje x)))

;; FUNCIONES DE CODIFICACION
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

;;Codifica una palabra a una lista de numeros del teclado
(defun codifica-palabra-lista (palabra)
  (loop for x across (string-downcase palabra)
	when (assoc (char-code x) *teclado* :test #'member)
	collect 
	(rest
	 (assoc (char-code x) *teclado* :test #'member))))
;;> (codifica-palabra-lista 'hola)
;; (4 6 5 2)

;; Pasa una lista de numeros '(1 2 3 4 5) a un literal '12345
(defun lista-a-numero-aux (lista)
  (let ((tam (1- (length lista)))
	(l (reverse lista)))
    (loop for i from 0 to tam
	  summing
	  (* (expt 10 i) (nth i l)))))
;;> (lista-a-numero-aux '(1 2 3 4 5))
;; 12345

;; Devuelve cierto si el último caracter de la palabra es un '.'
(defun punto-al-final (palabra)
  (equal (aref palabra (1- (length palabra))) '#\.))

;; Le quita el punto del final a la palabra, si es que lo tenia
(defun sin-punto (palabra)
  (if (null palabra)
      nil
    (if (punto-al-final palabra)
	(subseq palabra 0 (1- (length palabra)))
      palabra)))

;; Limpia una cadena de los siguintes simbolos especiales "'¿?¡()/!#,:;-
;; Los de cierre ? y ! se convierten a puntos .
(defun limpieza (palabra)
  (lista-char-a-string
  (loop for x across (string-downcase palabra) collect
    (cond
      ((or (equal x '#\') (equal x '#\¿) (equal x '#\¡) (equal x '#\()
	   (equal x '#\)) (equal x '#\/) (equal x '#\#) (equal x '#\,)
	   (equal x '#\:) (equal x '#\;) (equal x '#\-) (equal x '#\"))
	nil)
      ((or (equal x '#\?) (equal x '#\!))
       '#\.)
      (t
       x)))))

;; Pasa una lista de char a un string, saltandose aquellos que sean nil
(defun lista-char-a-string (l)
  (let ((cadena '""))
    (loop for x in l do
      (if (null x)
	nil
	(setf cadena (string-concat cadena (string x)))))
    cadena))

;; FUNCIONES DE PRESENTACION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lanza el programa mostrando los resultados por pantalla
(defun inicio ()
  (compile-file "principal.lsp")
  (load "principal")
  (format t "~%~%~%")
  (crea-teclado)
  (carga-datos t)
  (main t))

;; Carga los datos necesarios para ejecutar las funciones
(defun carga-datos (canal)
  (configuracion canal)
  (format canal "~&~%Carga del diccionario~%~%")
  (entrenamiento *diccionario-location*))

;; Opciones del programa
(defun configuracion (canal)
  (escoge-corpus canal)
  (format canal "~&~%Número de palabras predichas (por ejemplo 9): ")
  (setf *profundidad* (read)))

;; Permite escoger el corpus a utilizar
(defun escoge-corpus (canal)
  (let ((lista (directory 'corpus/*))
	(opcion 1)
	(terminado nil))
    (loop while (not terminado) do
	  (format canal "~&Escoge el corpus que quieres procesar: ")
	  (loop for x in lista
		for i from 1 to (length lista)
		do (format canal "~&~a.- ~a" i x))
	  (format canal "~&~a.- Continuar" (1+ (length lista)))
	  (format canal "~&Corpus a utilizar: ")
	  (setf opcion (read))
	  (cond
	   ((= opcion (1+ (length lista)))
	    (setf terminado t))
	   (t
	    (setf *corpus-location* (nth (1- opcion) lista))
	    (format canal "~&Proceso de entrenamiento~%~%")
	    (entrenamiento *corpus-location*))))))

;; Bucle principal del algoritmo
(defun main (canal)
  (let ((terminado nil)
	(teclas '())
	(tecla nil)
	(pred nil)
	(palabra nil)
	(frase '())
	(palabra-anterior nil)
	(indice 0))
    (loop while (not terminado) do
	  (escribe-teclado canal)
	  (format canal "~&~%Opciones:~%-Pulse un numero del teclado")
	  (format canal "~%-Pulse la letra e para un espacio en blanco")
	  (format canal "~%-Pulse la letra b para borrar la ultima pulsacion")
	  (format canal "~%-Pulse la letra o para escoger otra palabra predicha")
	  (format canal "~%-Pulse la letra n para incluir una palabra nueva")
	  (format canal "~%-Pulse la letra f para terminar una frase")
	  (format canal "~%-Pulse la letra q para salir")
	  (format canal "~%~%Su eleccion: ")
	  (setf tecla (read))
	  (cond
	   ((eq tecla 'q) ;; ---------------------------------------------------- Salir
	    (setf terminado t))
	   ((eq tecla 'e) ;; ---------------------------------------------------- Espacio en blanco
	    (aprendizaje palabra)
	    (setf teclas '())
	    (setf palabra-anterior palabra) ;; Palabra anterior
	    (setf frase (append frase (list palabra)))
	    (print-prediccion canal teclas palabra pred frase))
	   ((eq tecla 'b) ;; ---------------------------------------------------- Borrar ultima pulsacion
	    (if (null teclas)
		(format canal "~&~%No hay nada que borrar")
	      (setf teclas (reverse (rest (reverse teclas)))))
	    (setf indice 0)
	    (setf pred (prediccion teclas palabra-anterior))
	    (setf palabra (first (nth indice pred)))
	    (print-prediccion canal teclas palabra pred frase))
	   ((eq tecla 'n) ;; ---------------------------------------------------- Nueva palabra
	    (format canal "~&~%Escriba la palabra: ")
	    (setf palabra (string-downcase (read)))
	    (aprendizaje palabra)
	    (setf teclas '())
	    (setf frase (append frase (list palabra)))
	    (print-prediccion canal teclas palabra pred frase))
	   ((eq tecla 'f) ;; ---------------------------------------------------- Finalizar frase
	    (with-open-file (fich *diccionario-location* :direction :output :if-exists :append)
			    (write-line (parser-inversa frase) fich))
	    (setf palabra-anterior nil)
	    (setf teclas '())
	    (setf tecla nil)
	    (setf palabra nil)
	    (setf frase '())
	    (setf indice 0))
	   ((and (eq tecla 'o) (not (null pred)) (not (null palabra))) ;; ------- Siguiente palabra
	    (format canal "~&~%Introduzca el indice de la palabra deseada: ")
	    (setf indice (read))
	    (setf palabra (first (nth indice pred)))
	    (print-prediccion canal teclas palabra pred frase))
	   ((member tecla '(1 2 3 4 5 6 7 8 9)) ;; ------------------------------ Pulsar tecla
	    (setf teclas (append teclas (list tecla)))
	    (setf indice 0)	    
	    (setf pred (prediccion teclas palabra-anterior))
	    (setf palabra (first (nth indice pred)))
	    (print-prediccion canal teclas palabra pred frase))
	   (t
	    (format canal "~&~%Opcion invalida. Escoja otra vez.~%"))))))

;; Funcion auxiliar
(defun print-prediccion (canal teclas palabra pred frase)
  (format canal "~&~%Palabra predicha: ~a~%" palabra)
  (format canal "~&Palabras posibles: ~%")
  (print-prediccion-aux canal pred)
  (format canal "~&Frase hasta ahora: ~a~%" frase)
  (format canal "~&Teclas pulsadas: ~a~%~%" teclas))

;; Funcion auxiliar
(defun print-prediccion-aux (canal pred)
  (let ((prednorm (normaliza-lista pred)))
    (loop for i from 0 to (length prednorm)
	  when (= 0 (mod i 3))
	  do
	  (format canal "~&")
	  (print-palabra canal i (nth i prednorm))
	  (print-palabra canal (+ 1 i) (nth (+ 1 i) prednorm))
	  (print-palabra canal (+ 2 i) (nth (+ 2 i) prednorm)))))

(defun print-palabra (canal numero palabra)
  (if (and (not (null palabra)) (listp palabra))
      (format canal "~a...'~a'(~,3F)~t~t~t" numero (first palabra)(rest palabra))))

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
