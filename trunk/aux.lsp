(defun l ()
  (load "principal.lsp"))

;(defun linea-a-lista-palabras (l)
;(loop for x across l collect x ))
;(char-code (char-downcase x))))

(defun concatena (a b)
(string-concat (string a) (string b)))

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
