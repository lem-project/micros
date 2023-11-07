(in-package :micros/walker)

(defclass defun-form (ast)
  ((name :initarg :name
         :type variable-symbol
         :reader ast-name)
   (lambda-list :initarg :lambda-list
                :reader ast-lambda-list)
   (body :initarg :body
         :type implict-progn-form
         :reader ast-body)))

(defmethod walk-form ((walker walker) (name (eql 'defun)) form env path)
  (with-walker-bindings (name lambda-list &body body) (rest form)
    (multiple-value-bind (lambda-list env)
        (walk-lambda-list walker lambda-list env (cons 2 path))
      ;; TODO: declare
      (make-instance 'defun-form
                     :name name
                     :lambda-list lambda-list
                     :body (make-instance 'implict-progn-form
                                          :forms (walk-forms walker body env path 3))
                     :path (cons 0 path)))))

(defmethod visit (visitor (ast defun-form))
  (visit visitor (ast-lambda-list ast))
  (visit visitor (ast-body ast)))
