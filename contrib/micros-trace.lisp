(defpackage :micros/trace
  (:use :cl))
(in-package :micros/trace)

(defvar *traces* '())

(defun already-traced-p (name)
  (find name *traces*))

(defun remove-trace (name)
  (setf *traces* (remove name *traces*)))

(defun add-trace (name)
  (push name *traces*))

(defvar *depth* 0)

(defun print-indentation ()
  (loop :repeat (* *depth* 2) :do (write-char #\space))
  (format t "~D: " *depth*))

(defun send-write-objects-event (objects)
  (loop :for first := t :then nil
        :for object :in objects
        :do (write-char #\space)
            (micros::send-write-object-event object)))

(defun coerce-to-symbol (name)
  (etypecase name
    (symbol name)
    (string (micros:from-string name))))

(micros/swank-api:defslimefun micros-trace (name)
  (let ((name (coerce-to-symbol name)))
    (when (already-traced-p name)
      (warn "~a is apparently already traced! Untracing and retracing." name)
      (micros-untrace name))
    (flet ((before-hook (args)
             (print-indentation)
             (format t "(~A" name)
             (send-write-objects-event args)
             (format t ")~%")
             (incf *depth*))
           (after-hook (retlist)
             (decf *depth*)
             (print-indentation)
             (format t "~A returned" name)
             (send-write-objects-event (uiop:ensure-list retlist))
             (terpri)))
      (micros/backend:wrap name 'micros-trace
                           :before #'before-hook
                           :after #'after-hook)
      (add-trace name)
      (values))))

(micros/swank-api:defslimefun micros-untrace (name)
  (let ((name (coerce-to-symbol name)))
    (micros/backend:unwrap name 'micros-trace)
    (remove-trace name)
    (values)))

(micros/swank-api:defslimefun micros-trace-list ()
  (mapcar (lambda (name)
            (let ((*package* micros::*swank-io-package*))
              (prin1-to-string name)))
          *traces*))

(micros/swank-api:defslimefun toggle-trace (name)
  (let ((name (coerce-to-symbol name)))
    (if (already-traced-p name)
        (micros-untrace name)
        (micros-trace name))))
