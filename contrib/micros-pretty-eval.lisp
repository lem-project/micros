(defpackage :micros/pretty-eval
  (:use :cl :micros))
(in-package :micros/pretty-eval)

(defvar *null-value* (gensym))
(defvar *evaluated-values-table* (make-hash-table))
(defvar *evaluated-id-counter* 0)
(defvar *mutex* (micros/backend:make-lock :name "pretty-eval"))

(defun add (values)
  (micros/backend:call-with-lock-held
   *mutex*
   (lambda ()
     (let ((id (incf *evaluated-id-counter*)))
       (setf (gethash id *evaluated-values-table*)
             values)
       id))))

(defun get-by-id (id)
  (micros/backend:call-with-lock-held
   *mutex*
   (lambda ()
     (let ((values (gethash id *evaluated-values-table* *null-value*)))
       values))))

(defun remove-by-id (id)
  (micros/backend:call-with-lock-held
   *mutex*
   (lambda ()
     (remhash id *evaluated-values-table*))))

(micros/swank-api:defslimefun pretty-eval (string)
  (micros::with-buffer-syntax ()
    (let* ((values (multiple-value-list (eval (from-string string))))
           (id (add values)))
      (finish-output)
      (list :value (micros::format-values-for-echo-area values)
            :id id))))

(micros/swank-api:defslimefun inspect-evaluation-value (id)
  (let ((values (get-by-id id)))
    (unless (eq values *null-value*)
      (micros::with-buffer-syntax ()
        (micros::with-retry-restart (:msg "Retry SLIME inspection request.")
          (micros::reset-inspector)
          (micros::inspect-object (if (= 1 (length values))
                                      (first values)
                                      values)))))))

(micros/swank-api:defslimefun get-evaluation-value (id)
  (let ((values (get-by-id id)))
    (unless (eq values *null-value*)
      (first values))))

(micros::defslimefun remove-evaluated-values (id)
  (remove-by-id id)
  (values))
