
(defpackage :lsp-backend/contrib/snapshot
  (:use cl)
  (:export restore-snapshot save-snapshot background-save-snapshot)
  (:import-from swank defslimefun))
(in-package :lsp-backend/contrib/snapshot)

(defslimefun save-snapshot (image-file)
  (lsp-backend/backend:save-image image-file 
			    (let ((c lsp-backend::*emacs-connection*))
			      (lambda () (resurrect c))))
  (format nil "Dumped lisp to ~A" image-file))

(defslimefun restore-snapshot (image-file)
  (let* ((conn lsp-backend::*emacs-connection*)
	 (stream (lsp-backend::connection.socket-io conn))
	 (clone (lsp-backend/backend:dup (lsp-backend/backend:socket-fd stream)))
	 (style (lsp-backend::connection.communication-style conn))
	 (repl (if (lsp-backend::connection.user-io conn) t))
	 (args (list "--swank-fd" (format nil "~d" clone)
		     "--swank-style" (format nil "~s" style)
		     "--swank-repl" (format nil "~s" repl))))
    (lsp-backend::close-connection conn nil nil)
    (lsp-backend/backend:exec-image image-file args)))

(defslimefun background-save-snapshot (image-file)
  (let ((connection lsp-backend::*emacs-connection*))
    (flet ((complete (success)
	     (let ((lsp-backend::*emacs-connection* connection))
	       (lsp-backend::background-message
		"Dumping lisp image ~A ~:[failed!~;succeeded.~]" 
		image-file success)))
	   (awaken ()
	     (resurrect connection)))
      (lsp-backend/backend:background-save-image image-file
					   :restart-function #'awaken
					   :completion-function #'complete)
      (format nil "Started dumping lisp to ~A..." image-file))))

(in-package :lsp-backend)

(defun swank-snapshot::resurrect (old-connection)
  (setq *log-output* nil)
  (init-log-output)
  (clear-event-history)
  (setq *connections* (delete old-connection *connections*))
  (format *error-output* "args: ~s~%" (command-line-args))
  (let* ((fd (read-command-line-arg "--swank-fd"))
	 (style (read-command-line-arg "--swank-style"))
	 (repl (read-command-line-arg "--swank-repl"))
	 (* (format *error-output* "fd=~s style=~s~%" fd style))
	 (stream (make-fd-stream fd nil))
	 (connection (make-connection nil stream style)))
    (let ((*emacs-connection* connection))
      (when repl (lsp-backend/contrib/repl:create-repl nil))
      (background-message "~A" "Lisp image restored"))
    (serve-requests connection)
    (simple-repl)))

(defun read-command-line-arg (name)
  (let* ((args (command-line-args))
	 (pos (position name args :test #'equal)))
    (read-from-string (elt args (1+ pos)))))

(in-package :swank-snapshot)

(provide :swank-snapshot)
