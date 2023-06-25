(defpackage :micros/trace
  (:use :cl))
(in-package :micros/trace)

(defvar *depth* 0)

(defun print-indentation ()
  (loop :repeat (* *depth* 2) :do (write-char #\space))
  (format t "~D: " *depth*))

(defun send-write-objects-event (objects)
  (loop :for first := t :then nil
        :for object :in objects
        :do (unless first
              (write-char #\space))
            (micros::send-write-object-event object)))

(micros/swank-api:defslimefun micros-trace (name)
  (flet ((before-hook (args)
           (print-indentation)
           (format t "(~A " name)
           (send-write-objects-event args)
           (format t ")~%")
           (incf *depth*))
         (after-hook (retlist)
           (decf *depth*)
           (print-indentation)
           (format t "~A returned " name)
           (send-write-objects-event retlist)
           (terpri)))
    ;; TODO: check already traced
    (micros/backend:wrap name 'micros-trace
                         :before #'before-hook
                         :after #'after-hook)))
