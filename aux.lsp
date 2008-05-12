(defun l ()
  (load "principal.lsp"))

;(defun linea-a-lista-palabras (l)
;(loop for x across l collect x ))
;(char-code (char-downcase x))))

(defun concatena (a b)
(string-concat (string a) (string b)))