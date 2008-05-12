(defun l ()
  (load "principal.lsp"))


;(defun linea-a-lista-palabras (l)
;(loop for x across l collect x ))
;(char-code (char-downcase x))))

(defparameter alfabeto (list 'a'á 'b 'c 'd 'e 'é 'f 'g 'h 'i 'í 'j 'k 'l 'm 'n 'o 'ó 'ñ 'p 'q 'r 's 't 'u 'ú 'v 'w 'x 'y 'z))
	


(defun concatena (a b)
(string-concat (string a) (string b)))