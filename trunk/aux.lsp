(defun l ()
  (load "principal.lsp"))

(defun inserta-key-corpus-key (palabra)
(let ((numero (codifica-palabra palabra))
	(lista (descompone-a-numero palabra)))
;(format t "~&numero ~a lista ~a"numero lista)
(loop for x in lista
	do
	(if (member numero (gethash x corpus-aux))
	nil
	(setf (gethash x corpus-aux)
		(cons numero (gethash x corpus-aux)))))))

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
