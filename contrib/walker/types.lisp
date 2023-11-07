(in-package :micros/walker)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun proper-list-p (x)
    (and (listp x)
         (null (cdr (last x))))))

(deftype proper-list (&optional (element-type '*))
  (declare (ignore element-type))
  '(and list (satisfies proper-list-p)))

(deftype variable-symbol ()
  '(and symbol (not keyword)))

(deftype non-list ()
  '(and (not null) (not list)))
