(defparameter *world*
  '(
    (animal cordado)
    (cordado vertebrado)
    (vertebrado mamifero)
    (mamifero canino felino primata)
    (primata hominideo)
    (canino cachorro)
    (felino leao gato)
    (hominideo homem mulher)
    (gato fraj tom)
    (homem ale)
    ))

(defun is-the (value type)
  (tree-search-layer value (list type)))

(defun tree-search-layer (atom list)
  (cond ((null list) nil)
	((member atom list) t)
	((tree-search-layer atom (mappend #'elements list)))))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defun node (value)
  (srch-2 value *world*))

(defun elements (value)
  (rest (node value)))

(defun member-of (value)
  (srch value *world*))

(defun parent (value)
  (first (member-of value)))

(defun srch (value the-list)
  (cond ((null the-list) nil)
	((member value (first the-list)) (first the-list))
	(t (srch value (rest the-list)))))

(defun srch-2 (value the-list)
  (cond ((null the-list) nil)
	((eql value (first (first the-list))) (first the-list))
	(t (srch-2 value (rest the-list)))))