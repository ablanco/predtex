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

;; FUNCIONES DE AYUDA
;;;;;;;;;;;;;;;;;;;;;

(defun palabra-a-teclas (palabra)
  (loop for x in palabra collect
    (cond
      ((or (eq x 'a) (eq x 'b) (eq x 'c))
	2)
      ((or (eq x 'd) (eq x 'e) (eq x 'f))
	3)
      ((or (eq x 'g) (eq x 'h) (eq x 'i))
	4)
      ((or (eq x 'j) (eq x 'k) (eq x 'l))
	5)
      ((or (eq x 'm) (eq x 'n) (eq x 'o))
	6)
      ((or (eq x 'p) (eq x 'q) (eq x 'r) (eq x 's))
	7)
      ((or (eq x 't) (eq x 'u) (eq x 'v))
	8)
      ((or (eq x 'w) (eq x 'x) (eq x 'y) (eq x 'z))
	9))))
