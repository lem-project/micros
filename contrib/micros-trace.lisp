(defpackage :micros/trace
  (:use :cl))
(in-package :micros/trace)

(defvar *depth* 0)

(micros/swank-api:defslimefun micros-trace (name)
  (flet ((before-hook (args)
           (write-string (make-string (* *depth* 2) :initial-element #\space))
           (prin1 (list* name :> args))
           (terpri)
           (incf *depth*))
         (after-hook (retlist)
           (decf *depth*)
           (write-string (make-string (* *depth* 2) :initial-element #\space))
           (prin1 (list* name :< retlist))
           (terpri)))
    ;; TODO: check already traced
    (micros/backend:wrap name 'micros-trace
                         :before #'before-hook
                         :after #'after-hook)))
