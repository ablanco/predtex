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





;; ESTRUCTURAS DE DATOS
;;;;;;;;;;;;;;;;;;;;;;;

;; En corpus-lib se encuentra la informacion de todo el diccionario
;; la key es la palabra el value contiene una estructura corpus
(defparameter corpus-lib (make-hash-table))

;; En corpus lib almacenamos la lista de palabras asociadas a un numero
;; la key es el numero, value la lista de palabras
;; TODO. mejorar, almacena la lista con valor (hola . 095)
(defparameter corpus-numero (make-hash-table))

;; Estructura corpus donde almacenamos la informacion de la palabra
(defstruct corpus
palabra 
numero-asociado 
probabilidad)

;; FUNCIONES DE ESTRUCTURAS DE DATOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Crea una palabra con la estructura corpus
(defun crea-palabra-corpus (lista-letras lista-numeros)
(make-corpus :palabra lista-letras
:numero-asociado lista-numeros
:probabilidad 0))

;; Inserta una palabra en las dos tablas hash
(defun inserta-palabra (palabra)
  (insterta-palabra-corpus-numero palabra)
  (inserta-palabra-corpus-lib palabra))

(defun insterta-palabra-corpus-numero (palabra)
(let ((numero (palabra-a-numero palabra)))
(setf (gethash numero corpus-numero)
(cons palabra (gethash numero corpus-numero)))))

(defun inserta-palabra-corpus-lib (palabra)
  (setf (gethash palabra corpus-lib) 
  (crea-palabra-corpus (palabra-a-lista palabra) (palabra-a-lista-numeros palabra))))

;; Devuelve la lista de palabras asociada a un numero
(defun get-palabras-numero (numero)
(gethash numero corpus-numero))

;; Devuelve la estructura corpus de palabra
(defun get-palabras-lib (palabra)
(gethash palabra corpus-lib))

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

(defun palabra-a-lista-numeros (palabra)
  (loop for x across (string palabra) collect
    (cond
      ((or (eq x (character 'a)) (eq x (character 'b)) (eq x (character 'c)))
	2)
      ((or (eq x (character 'd)) (eq x (character 'e)) (eq x (character 'f)))
	3)
      ((or (eq x (character 'g)) (eq x (character 'h)) (eq x (character 'i)))
	4)
      ((or (eq x (character 'j)) (eq x (character 'k)) (eq x (character 'l)))
	5)
      ((or (eq x (character 'm)) (eq x (character 'n)) (eq x (character 'o)))
	6)
      ((or (eq x (character 'p)) (eq x (character 'q)) (eq x (character 'r)) (eq x (character 's)))
	7)
      ((or (eq x (character 't)) (eq x (character 'u)) (eq x (character 'v)))
	8)
      ((or (eq x (character 'w)) (eq x (character 'x)) (eq x (character 'y)) (eq x (character 'z)))
	9))))

;; FUNCIONES DE CODIFICACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
(defun palabra-a-numero (palabra)
 123)
(defun palabra-a-lista (palabra)
  '(a b c d))