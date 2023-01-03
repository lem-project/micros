(defpackage :lsp-backend/lsp-api
  (:use :cl)
  (:export :describe-symbol))
(in-package :lsp-backend/lsp-api)

(defun describe-symbol (symbol-name)
  (lsp-backend::with-buffer-syntax ()
    (multiple-value-bind (symbol status)
        (lsp-backend::parse-symbol symbol-name)
      (when status 
        (lsp-backend::describe-to-string symbol)))))
