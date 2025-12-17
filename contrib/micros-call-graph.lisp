;;; micros-call-graph.lisp --- Call graph analysis for Living Canvas
;;
;; License: MIT (same as Lem)

(in-package :micros)

;;; Configuration

(defparameter *excluded-definition-forms*
  '(defpackage in-package defsystem
    defvar defparameter defconstant
    defclass defstruct deftype define-condition
    deftest)
  "Definition forms to exclude from call graph analysis.")

;;; Utilities

(defun function-type (symbol)
  "Determine the type of a function symbol."
  (cond ((macro-function symbol) :macro)
        ((and (fboundp symbol)
              (typep (fdefinition symbol) 'generic-function))
         :generic-function)
        ((fboundp symbol) :function)
        (t nil)))

(defun format-arglist (symbol)
  "Format the argument list for display."
  #+sbcl
  (handler-case
      (let ((arglist (sb-introspect:function-lambda-list
                      (if (macro-function symbol)
                          (macro-function symbol)
                          (fdefinition symbol)))))
        (when arglist
          (format nil "(~{~(~A~)~^ ~})" arglist)))
    (error () nil))
  #-sbcl nil)

(defun get-source-location (symbol)
  "Get the source file and line number for a symbol's function definition."
  #+sbcl
  (handler-case
      (let ((source (sb-introspect:find-definition-source
                     (fdefinition symbol))))
        (when source
          (let ((pathname (sb-introspect:definition-source-pathname source)))
            (when pathname
              (cons (namestring pathname)
                    (or (find-definition-line pathname (symbol-name symbol))
                        1))))))
    (error () nil))
  #-sbcl nil)

(defun find-definition-line (pathname symbol-name)
  "Find the line number where a symbol is defined."
  (when (and pathname (probe-file pathname))
    (with-open-file (stream pathname :if-does-not-exist nil)
      (when stream
        (loop :for line-number :from 1
              :for line = (read-line stream nil nil)
              :while line
              :when (line-defines-symbol-p line symbol-name)
                :return line-number)))))

(defparameter *definition-prefixes*
  '("(defun " "(defmacro " "(defgeneric " "(defmethod "
    "(define-command " "(define-major-mode " "(define-minor-mode "
    ;; Package-qualified versions (lem:define-command etc.)
    "(lem:define-command " "(lem:define-major-mode " "(lem:define-minor-mode "
    "(lem:define-key " "(lem:define-attribute ")
  "Prefixes for definition forms used in line detection.")

(defun line-defines-symbol-p (line symbol-name)
  "Check if a line defines the given symbol."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (dolist (prefix *definition-prefixes*)
      (let ((prefix-len (length prefix)))
        (when (and (>= (length trimmed) prefix-len)
                   (string-equal prefix (subseq trimmed 0 prefix-len)))
          (let* ((rest (subseq trimmed prefix-len))
                 (rest-trimmed (string-left-trim '(#\Space #\Tab #\() rest))
                 (space-pos (position #\Space rest-trimmed))
                 (paren-pos (position #\( rest-trimmed))
                 (close-paren-pos (position #\) rest-trimmed))
                 (end-pos (or space-pos paren-pos close-paren-pos (length rest-trimmed)))
                 (name (subseq rest-trimmed 0 end-pos)))
            (when (string-equal name symbol-name)
              (return-from line-defines-symbol-p t))))))
    nil))

;;; Introspection Helpers

(defun function-to-symbol (fn)
  "Try to get the symbol name for a function object."
  #+sbcl
  (handler-case
      (let ((name (sb-kernel:%fun-name fn)))
        (cond
          ((symbolp name) name)
          ((and (consp name) (eq (car name) 'sb-pcl::fast-method))
           (cadr name))
          ((and (consp name) (member (car name) '(setf sb-pcl::slot-accessor)))
           nil)
          (t nil)))
    (error () nil))
  #-sbcl nil)

(defun get-callees (symbol package)
  "Get all functions called by SYMBOL within the given package."
  #+sbcl
  (handler-case
      (let ((fn (fdefinition symbol)))
        (when fn
          (let ((callees (sb-introspect:find-function-callees fn))
                (result '()))
            (dolist (callee callees)
              (let ((callee-sym (function-to-symbol callee)))
                (when (and callee-sym
                           (not (eq callee-sym symbol))
                           (eq (symbol-package callee-sym) package)
                           (fboundp callee-sym))
                  (pushnew callee-sym result))))
            result)))
    (error () nil))
  #-sbcl nil)

(defun get-callers (symbol package)
  "Get all functions that call SYMBOL within the given package."
  #+sbcl
  (handler-case
      (let ((xrefs (sb-introspect:who-calls symbol))
            (result '()))
        (dolist (xref xrefs)
          (let ((caller-name (car xref)))
            (when (and (symbolp caller-name)
                       (not (eq caller-name symbol))
                       (eq (symbol-package caller-name) package)
                       (fboundp caller-name))
              (pushnew caller-name result))))
        result)
    (error () nil))
  #-sbcl nil)

;;; Source Code Reading

(defun read-all-forms-from-file (pathname)
  "Read all forms from a file, handling in-package correctly."
  (handler-case
      (with-open-file (stream pathname :if-does-not-exist nil)
        (when stream
          (let ((*package* (find-package :cl-user))
                (*read-eval* nil)
                (forms '()))
            (loop :for form = (read stream nil :eof)
                  :until (eq form :eof)
                  :do (when (and (consp form)
                                 (eq (car form) 'in-package))
                        (let ((pkg (find-package (cadr form))))
                          (when pkg (setf *package* pkg))))
                      (push form forms))
            (nreverse forms))))
    (error () nil)))

(defun def-form-p (form-head)
  "Check if FORM-HEAD is a definition macro."
  (and (symbolp form-head)
       (let ((name (symbol-name form-head)))
         (or (and (>= (length name) 3)
                  (string-equal "DEF" name :end2 3))
             (and (>= (length name) 7)
                  (string-equal "DEFINE-" name :end2 7))))))

(defun extract-definition-name (form)
  "Extract the defined name from a definition form."
  (when (and (consp form) (consp (cdr form)))
    (let ((form-head (car form))
          (second-element (cadr form)))
      (cond
        ((member form-head '(define-command define-major-mode define-minor-mode
                             define-global-mode) :test #'string-equal)
         (if (consp second-element)
             (car second-element)
             second-element))
        ((symbolp second-element)
         second-element)
        (t nil)))))

(defun excluded-definition-form-p (form-head)
  "Check if FORM-HEAD should be excluded from call graph analysis."
  (member (symbol-name form-head)
          (mapcar #'symbol-name *excluded-definition-forms*)
          :test #'string-equal))

;;; Source-based Call Detection

(defun extract-called-symbols-from-form (form target-symbols)
  "Extract symbols from FORM that are in TARGET-SYMBOLS set."
  (let ((calls '()))
    (labels ((walk (f)
               (cond
                 ((and (symbolp f)
                       (gethash f target-symbols))
                  (pushnew f calls))
                 ((and (consp f)
                       (eq (car f) 'function)
                       (symbolp (cadr f))
                       (gethash (cadr f) target-symbols))
                  (pushnew (cadr f) calls))
                 ((consp f)
                  (walk (car f))
                  (walk (cdr f))))))
      (handler-case
          (walk form)
        (error () nil)))
    calls))

(defun get-definition-body (form)
  "Extract the body from a definition form."
  (when (and (consp form) (consp (cdr form)))
    (cdddr form)))

(defun extract-source-calls (pathname target-symbols)
  "Extract call relationships from source file PATHNAME."
  (let ((forms (read-all-forms-from-file pathname))
        (calls '()))
    (dolist (form forms)
      (when (and (consp form)
                 (def-form-p (car form))
                 (not (excluded-definition-form-p (car form))))
        (let ((name (extract-definition-name form))
              (body (get-definition-body form)))
          (when (and name (symbolp name) (gethash name target-symbols))
            (let ((callees (extract-called-symbols-from-form body target-symbols)))
              (dolist (callee callees)
                (unless (eq callee name)
                  (push (cons name callee) calls))))))))
    calls))

;;; Graph Construction

(defun make-node-id (package-name symbol-name)
  "Create a unique ID for a node."
  (format nil "~A:~A" package-name symbol-name))

(defun symbol-to-node-id (symbol)
  "Create a unique ID for a symbol node."
  (make-node-id (package-name (symbol-package symbol))
                (symbol-name symbol)))

(defun make-node-plist (symbol &optional form-head file-path)
  "Create a plist representing a graph node."
  (let* ((source-loc (get-source-location symbol))
         (type (cond
                 ((and form-head (string-equal (symbol-name form-head) "DEFINE-MAJOR-MODE"))
                  :major-mode)
                 ((and form-head (string-equal (symbol-name form-head) "DEFINE-MINOR-MODE"))
                  :minor-mode)
                 ((and form-head (string-equal (symbol-name form-head) "DEFINE-COMMAND"))
                  :command)
                 (t (function-type symbol)))))
    (list :id (symbol-to-node-id symbol)
          :name (symbol-name symbol)
          :package (package-name (symbol-package symbol))
          :type type
          :docstring (documentation symbol 'function)
          :arglist (format-arglist symbol)
          :source-file (or (car source-loc) file-path)
          :source-line (cdr source-loc))))

(defun make-edge-plist (source-symbol target-symbol)
  "Create a plist representing a graph edge."
  (list :source (symbol-to-node-id source-symbol)
        :target (symbol-to-node-id target-symbol)
        :call-type :direct))

(defun remove-duplicate-edges (edges)
  "Remove duplicate edges from the list."
  (remove-duplicates edges
                     :test (lambda (a b)
                             (and (equal (getf a :source) (getf b :source))
                                  (equal (getf a :target) (getf b :target))))))

;;; Analysis Functions

(defslimefun call-graph-analyze-package (package-designator)
  "Analyze a package and return call graph as plist.
Returns nil if package not found."
  (let ((package (find-package package-designator)))
    (unless package
      (return-from call-graph-analyze-package nil))
    (let ((nodes '())
          (edges '())
          (symbols '()))
      ;; Collect all function symbols
      (do-symbols (sym package)
        (when (and (eq (symbol-package sym) package)
                   (fboundp sym)
                   (function-type sym))
          (push sym symbols)
          (push (make-node-plist sym) nodes)))
      ;; Create edges using who-calls and find-function-callees
      (dolist (sym symbols)
        ;; Callers
        (let ((callers (get-callers sym package)))
          (dolist (caller callers)
            (push (make-edge-plist caller sym) edges)))
        ;; Callees
        (let ((callees (get-callees sym package)))
          (dolist (callee callees)
            (push (make-edge-plist sym callee) edges))))
      ;; Remove duplicates and return
      (list :nodes (nreverse nodes)
            :edges (remove-duplicate-edges edges)))))

(defun extract-definitions-from-file (pathname)
  "Extract all function/macro definitions from a file."
  (let ((forms (read-all-forms-from-file pathname))
        (definitions '()))
    (dolist (form forms)
      (when (and (consp form)
                 (def-form-p (car form))
                 (not (excluded-definition-form-p (car form))))
        (let ((name (extract-definition-name form)))
          (when (and name (symbolp name) (fboundp name))
            (push (cons name (car form)) definitions)))))
    (nreverse definitions)))

(defslimefun call-graph-analyze-file (pathname)
  "Analyze a file and return call graph as plist."
  (let ((definitions (extract-definitions-from-file pathname))
        (nodes '())
        (edges '())
        (symbols '())
        (symbol-set (make-hash-table :test 'eq))
        (file-path (namestring pathname)))
    (when definitions
      (let ((package (symbol-package (car (first definitions)))))
        ;; Create nodes
        (dolist (def definitions)
          (let* ((sym (car def))
                 (form-head (cdr def)))
            (push sym symbols)
            (setf (gethash sym symbol-set) t)
            (push (make-node-plist sym form-head file-path) nodes)))
        ;; Create edges using sb-introspect
        (dolist (sym symbols)
          (let ((callers (get-callers sym package)))
            (dolist (caller callers)
              (when (gethash caller symbol-set)
                (push (make-edge-plist caller sym) edges))))
          (let ((callees (get-callees sym package)))
            (dolist (callee callees)
              (when (gethash callee symbol-set)
                (push (make-edge-plist sym callee) edges)))))
        ;; Source-based call detection
        (let ((source-calls (extract-source-calls pathname symbol-set)))
          (dolist (call source-calls)
            (push (make-edge-plist (car call) (cdr call)) edges)))))
    ;; Remove duplicates and return
    (list :nodes (nreverse nodes)
          :edges (remove-duplicate-edges edges))))

(defun get-system-source-files (system-designator)
  "Get all Lisp source files from an ASDF system.
Returns nil if system not found."
  (let ((system (asdf:find-system system-designator nil)))
    (unless system
      (return-from get-system-source-files nil))
    (let ((files '()))
      (labels ((collect-files (component)
                 (typecase component
                   (asdf:cl-source-file
                    (let ((path (asdf:component-pathname component)))
                      (when (and path (probe-file path))
                        (push (namestring path) files))))
                   (asdf:module
                    (dolist (child (asdf:component-children component))
                      (collect-files child)))
                   (asdf:system
                    (dolist (child (asdf:component-children component))
                      (collect-files child)))
                   (otherwise nil))))
        (collect-files system))
      (nreverse files))))

(defslimefun call-graph-analyze-system (system-designator)
  "Analyze an ASDF system and return call graph as plist.
Returns nil if system not found or has no source files."
  (let ((files (get-system-source-files system-designator)))
    (unless files
      (return-from call-graph-analyze-system nil))
    (let ((nodes '())
          (edges '())
          (all-symbols '())
          (symbol-set (make-hash-table :test 'eq))
          (node-ids (make-hash-table :test 'equal)))
      ;; Collect all definitions from all files
      (dolist (file files)
        (let ((definitions (extract-definitions-from-file file)))
          (dolist (def definitions)
            (let* ((sym (car def))
                   (form-head (cdr def))
                   (node-id (symbol-to-node-id sym)))
              (unless (gethash node-id node-ids)
                (push sym all-symbols)
                (setf (gethash sym symbol-set) t)
                (setf (gethash node-id node-ids) t)
                (push (make-node-plist sym form-head file) nodes))))))
      ;; Create edges using sb-introspect
      (dolist (sym all-symbols)
        (handler-case
            (let ((fn (fdefinition sym)))
              (when fn
                #+sbcl
                (let ((callees (sb-introspect:find-function-callees fn)))
                  (dolist (callee callees)
                    (let ((callee-sym (function-to-symbol callee)))
                      (when (and callee-sym
                                 (not (eq callee-sym sym))
                                 (gethash callee-sym symbol-set))
                        (push (make-edge-plist sym callee-sym) edges)))))))
          (error () nil)))
      ;; Source-based call detection
      (dolist (file files)
        (let ((source-calls (extract-source-calls file symbol-set)))
          (dolist (call source-calls)
            (push (make-edge-plist (car call) (cdr call)) edges))))
      ;; Remove duplicates and return
      (list :nodes (nreverse nodes)
            :edges (remove-duplicate-edges edges)))))
