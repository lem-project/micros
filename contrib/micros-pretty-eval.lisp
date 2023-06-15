(defpackage :micros/contrib/pretty-eval
  (:use :cl :micros))
(in-package :micros/contrib/pretty-eval)

(defvar *null-value* (gensym))
(defvar *evaluated-values-table* (make-hash-table))
(defvar *evaluated-id-counter* 0)

(defun add (values)
  (let ((id (incf *evaluated-id-counter*)))
    (setf (gethash id *evaluated-values-table*)
          values)
    id))

(defun get-by-id (id)
  (let ((values (gethash id *evaluated-values-table* *null-value*)))
    values))

(defun remove-by-id (id)
  (remhash id *evaluated-values-table*))

(micros::defslimefun pretty-eval (string)
  (micros::with-buffer-syntax ()
    (let* ((values (multiple-value-list (eval (from-string string))))
           (id (add values)))
      (finish-output)
      (list :value (micros::format-values-for-echo-area values)
            :id id))))

(micros::defslimefun inspect-evaluation-value (id)
  (let ((values (get-by-id id)))
    (unless (eq values *null-value*)
      (micros::with-buffer-syntax ()
        (micros::with-retry-restart (:msg "Retry SLIME inspection request.")
          (micros::reset-inspector)
          (micros::inspect-object (if (= 1 (length values))
                                      (first values)
                                      values)))))))

(micros::defslimefun remove-evaluated-values (id)
  (remove-by-id id)
  (values))
