(defun l ()
  (compile-file "principal.lsp")
  (load "principal")
  (format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%")
  (inicio))



;; ANTIGUO
;; ;; Incluye en corpus key todas las posibles palabras que se pueden llegar a escribir a partir de la dada
;; (defun set-key (palabra1 &optional (palabra2 nil))
;;   (let ((numero (codifica-palabra palabra1)))
;;     ;; (format t "~&DeBUG setkey: '~a' (ind: ~a) (cod: ~a)" palabra1 indice (codifica-palabra palabra1))
;; ;; (if (not (null palabra2)) ;;Si no es una palabra compuesta
;;     ;; 		(format t "-> '~a'  (cod: ~a)" (string-concat palabra1 " " palabra2) (codifica-palabra (string-concat palabra1 " " palabra2))))
;;     (set-key-aux
;;      palabra1
;;      (lista-a-numero-aux (subseq (codifica-palabra-lista palabra1) 0 (1- (length palabra1))))
;;      numero)
;;     (if (null palabra2) ;;Si no es una palabra compuesta
;; 	nil ;;No se hace nada
;;       (set-key-aux ;;Para añadir una palabra compuesta
;;        (string-concat palabra1 " " palabra2)
;;        numero
;;        (codifica-palabra (string-concat palabra1 " " palabra2))))))


;(defun linea-a-lista-palabras (l)
;(loop for x across l collect x ))
;(char-code (char-downcase x))))

;; (defun concatena (a b)
;; (string-concat (string a) (string b)))

;; Sin uso
;; (defun separa-en-bipalabras (lista)
;;   (loop for i from 0 to (1- (length lista))
;; 	collect
;; 	(list
;; 	 (nth i lista)
;; 	 (nth (1+ i) lista ))))
;; (separa-en-bipalabras '("hola" "amigo" "mio"))
;; (("hola" "amigo") ("amigo" "mio") ("mio" NIL))

;; entrenamiento
;; PALABRAS DOBLES
;; 			(loop for x in (separa-en-bipalabras (parser l))
;; 			    do
;; 			    (if (< 0 (length (first x)))
;; 				(if (null (second x))
;; 				    (add-palabra (first x))
;; 				  (add-palabra (string-concat (first x) " "(second x)))))))))

;; add-palabra antiguo
;;   (let*
;;       ((palabras (parser palabra))
;;        (numero (codifica-palabra (first palabras)))
;;        (numero-compuesto nil))
;;     ;; 	 (format t " numero '~a' numero-compuesto '~a'"numero (codifica-palabra palabra))
;;     (setf *palabras-totales* (1+ *palabras-totales*))
;;     (setf (gethash numero *corpus*)
;; 	  (add-palabra-aux (first palabras) (gethash numero *corpus*)))
;;     (cond
;;      ((< 1 (length palabras)) ;;Palabra compuesta
;;       (set-key (first palabras) (second palabras))
;;       ;; 			(setf palabra-compuesta (string-concat (first palabras) " " (second palabras)))
;;       (setf numero-compuesto (codifica-palabra palabra))
;;       (setf (gethash numero-compuesto *corpus*)
;; 	    (add-palabra-aux palabra (gethash numero-compuesto *corpus*))))
;;      (t
;;       (set-key (first palabras) nil))))) ;;Palabra simple
 
 
;; MAL
;;   (let ((palabras (parser palabra)))
;;     (if (< 1 (length palabras))
;; 	(get-bi-probabilidad (first palabras) palabra)
;;       (/
;;        (rest (assoc palabra (gethash (codifica-palabra palabra) *corpus*) :test #' string-equal))
;;        *palabras-totales*))))
;; TODO Arreglar y poner ejemplo

;;  MAL
;; Devuelve la probabilidad de una palabra compuesta en el modelo bigram
;; (defun get-bi-probabilidad (palabra bipalabra)
;;   (* (get-probabilidad palabra)
;;      (/ 
;;       ;;    (rest (assoc bipalabra (gethash (codifica-palabra bipalabra) *corpus-compuesto*) :test #' string-equal))
;;       (rest (assoc bipalabra (gethash (codifica-palabra bipalabra) *corpus*) :test #' string-equal))
;;       (rest (assoc palabra (gethash (codifica-palabra palabra) *corpus*)
;; 		   :test #' string-equal)))))
;; TODO Arreglar y poner ejemplo

;; SIN USO
;; ;; Devuelve la lista de palabras asociada a un numero, ordenadas por probabilidad
;; (defun get-lista-palabras (numero)
;;   (ordena-por-probabilidad
;;    (calcula-probabilidad
;;     (gethash numero *corpus*))))
;; ;; TODO Poner ejemplo
 
;; Nos da una lista de indices relacionados con nuestra palabra
;; (defun lista-indices-relacionados (palabra)
;; 	(let ((lista (codifica-palabra-lista palabra))) ;obtenemos la lista de numeros
;;  	(loop for i from (- (length lista) 3 ) downto 1 ;;tamaño minimo 3
;; ;;	(loop for i from (length lista) downto 1 ;;tamaño minimo 3
;; 	collect
;; 	(lista-a-numero-aux ;;pasamos a numero la lista
;; 		(reverse (subseq (reverse lista) i))))))  ;;obtenemos una lista cada vez mas grande :D
;; ;;> (lista-indices-relacionados "hola amigo")
;; ;; (465 4652 46521 465212 4652126 46521264 465212644)
#|
;; Inserta una palabra compuesta en el corpus compuesto actualizando sus repeticiones
(defun add-palabra-compuesta (anterior palabra)
(if (and 	
	(< 0 (length palabra))(< 0 (length anterior))) ;; No son nulas o blancos ("")
	(let* ;; e.o.c
		((palabra-compuesta (string-concat anterior '" " palabra)) ;; Concatena las palabras
		(numero (codifica-palabra palabra-compuesta))) ;; Numero de la palabra-compuesta
	(set-key
		palabra-compuesta (codifica-palabra palabra-compuesta))
;; 	(setf (gethash numero *corpus-compuesto*) ;; Actualizamos (si no esta se incluye) la palabra en el corpus compuesto
;;		(add-palabra-aux palabra-compuesta (gethash numero *corpus-compuesto*))))
 	(setf (gethash numero *corpus*) ;; Actualizamos (si no esta se incluye) la palabra en el corpus compuesto
		(add-palabra-aux palabra-compuesta (gethash numero *corpus*))))
	nil))|#
		
#|
(defun set-key (palabra numero)
  (loop for x in (lista-indices-relacionados palabra)
    do
    (if (member numero (gethash x *corpus-key*))
      nil
      (setf (gethash x *corpus-key*)
	(cons numero (gethash x *corpus-key*))))))
|#


;; (defun get-lista-palabras-relacionadas (numero)
;;   (let ((lista (gethash numero *corpus-key*))) ;; Indices del corpus-key
;;     (subseq ;; Tomamos solo una lista de tam maximo *profundidad*
;;      (append
;;       (get-lista-palabras numero) ;; Palabras que codifica el numero
;;                                   ;; Estan puestas fuera del ordena para que siempre aparezcan antes
;;       (ordena-por-probabilidad
;;        (append
;; 	(loop for x in lista
;; 	      append
;; 	      (gethash x *corpus*)) ;; Palabras del corpus
;; 	(loop for x in lista
;; 	      append
;; 	      (gethash x *corpus-compuesto*)))))
;;      0 (min *profundidad* (length lista))))) ;; Palabras del corpus-compuesto

#|
(defun leer-prueba()
 (with-open-file (s '"corpus_sin_tratar/reservoir_dogs-sintildes")
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Fin de Fichero.")
     (format t "~&Leida ~A~%" (parser (string l)))
;; 	(set-palabra l (codifica-palabra l))
	)))

(defvar *palabras-compuestas-totales* 0);; Numero total de palabras reconocidas hasta el momento.


(defun get-bi-palabras (numero)
(ordena-por-probabilidad
(calcula-probabilidad
  (gethash numero *corpus-compuesto*))))|#

;; ;; Separa una palabra compuesta e una lista de sus codificaciones
;; (defun get-bi-probabilidad-aux (palabra)
;; (let ((lista (codifica-palabra-lista (string-downcase palabra))))
;; (loop for x in
;; 		(append (loop for i from 0 to (1- (length lista))
;; 		when (= (nth i lista) 1)
;; 		collect i) (list (length lista)))
;; 	collect (lista-a-numero-aux (subseq lista 0 x)))))
;; ;; (get-bi-probabilidad-aux "hola amigo")
;; ;; (4652 4652126446)

;;TODO borrar
;; (defun entrenamiento (fichero)
;;   (let* ((lista (entrenamientofichero))
;; 	(total (rest lista)))
;;     (loop for x in (first lista) do
;; 	(set-palabra (first x) (codifica-palabra (first x))))))
;;       ;;(if (= total 0)
;; 	;;(set-palabra (first x)) 
;; 	;;(set-palabra (first x) (/ (rest x) total)))))) 

;;TODO esta funciona ya no vale pa na no?????
;(entrenamiento"corpus.txt")
;; (defun entrenamiento-aux (linea lista)
;; (cons 
;; 	(if (assoc linea (first lista) :test #' string-equal) ;si ya pertenece a la lista
;; 	(loop for x in (first lista)
;; 		collect
;; 		(if (string-equal linea (first x))
;; 		(cons (string-downcase linea) (+ 1 (rest x)));actualizo su valor
;; 		x))
;; 	(cons
;; 		(cons (string-downcase linea) 1) ; la creo
;; 	(first lista)))
;; (+ 1 (rest lista)))) ;le sumo uno al tamaño

		

;; (defun get-n-palabras-relacionadas-aux (lista)
;; 	(subseq lista 0 (min *profundidad* (length lista)))
;; 
;; 	(loop for x in
;; 		(loop for i from 0 to (min *profundidad* (length lista))
;; 			collect
;; 			(nth i lista))
;; 	when (not (null x))
;; 	collect	x))

;; (defun get-indices-palabra (numero)
;; 	(gethash numero *corpus-key*))
