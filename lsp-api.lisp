(defpackage :micros/lsp-api
  (:use :cl)
  (:export :hover-symbol))
(in-package :micros/lsp-api)

(defun describe-variable (symbol)
  (list "Variable"
        (with-output-to-string (stream)
          (when (boundp symbol)
            (sb-impl::describe-variable symbol stream)))))

(defun describe-function (symbol)
  (list "Function"
        (with-output-to-string (stream)
          (when (fboundp symbol)
            (let ((arglist (cons symbol (micros/backend:arglist symbol))))
              (write-line "```lisp" stream)
              (let ((*print-case* :downcase))
                (format stream "~(~A~)~%" arglist))
              (write-line "```" stream)
              (let ((doc (documentation symbol 'function)))
                (when doc
                  (write-line doc stream))))))))

(defun describe-class (symbol)
  (list "Class"
        (with-output-to-string (stream)
          (sb-impl::describe-class symbol nil stream))))

(defun describe-type (symbol)
  (list "Type"
        (with-output-to-string (stream)
          (sb-impl::describe-type symbol stream))))

(defun describe-declaration (symbol)
  (list "Declaration"
        (with-output-to-string (stream)
          (sb-impl::describe-declaration symbol stream))))

(defun describe-plist (symbol)
  (list "Symbol-plist:"
        (with-output-to-string (stream)
          (let ((plist (symbol-plist symbol)))
            (when plist
              (loop :for (k v) :on plist :by #'cddr
                    :do (format stream
                                "  ~@:_~A -> ~A~%"
                                (prin1-to-string k)
                                (prin1-to-string v))))))))

(defun hover-symbol (symbol-name)
  (micros::with-buffer-syntax ()
    (multiple-value-bind (symbol status)
        (micros::parse-symbol symbol-name)
      (when status
        (with-output-to-string (stream)
          (let ((contents
                  (remove ""
                          (list (describe-variable symbol)
                                (describe-function symbol)
                                (describe-class symbol)
                                (describe-type symbol)
                                (describe-declaration symbol)
                                (describe-plist symbol))
                          :key #'second
                          :test #'string=)))
            (loop :for (header body) :in contents
                  :for first := t :then nil
                  :do (unless first
                        (write-line "----------" stream))
                      (format stream "## ~A~%" header)
                      (write-line body stream))))))))
