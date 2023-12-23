(in-package :micros/walker)

(defclass defmethod-form (ast)
  ((block-form :initarg :block-form
               :reader ast-block-form
               :type block-name-form)
   (name :initarg :name
         :reader ast-name)
   (lambda-list :initarg :lambda-list
                :reader ast-lambda-list)
   (body :initarg :body
         :type implict-progn-form
         :reader ast-body)))

(defun take-method-qualifiers (args)
  (let ((method-qualifiers
          (loop :while (typep (first args) 'non-list)
                :collect (pop args))))
    (values method-qualifiers args)))

(defmethod walk-specialized-lambda-list ((walker walker) specialized-lambda-list env path)
  (let ((walked-lambda-list '())
        (initial-forms '()))
    (labels ((add (binding path)
               (setf env (extend-env env binding))
               (push (make-instance 'lambda-list-variable-form
                                    :binding binding
                                    :path path)
                     walked-lambda-list)))
      (loop :with state := nil
            :for n :from 0
            :for arg :in specialized-lambda-list
            :do (case arg
                  ((&aux &key &rest &body &optional)
                   (setf state arg))
                  (otherwise
                   (ecase state
                     ((&rest &body)
                      (assert-type arg 'variable-symbol)
                      ;; TODO: special variable
                      (add (make-instance 'lexical-variable-binding :name arg)
                           (cons n path)))
                     ((&key &optional &aux)
                      (let* ((var-value (uiop:ensure-list arg))
                             (var (first var-value))
                             (value (second var-value)))
                        (assert-type var 'variable-symbol)
                        (let ((initial-value
                                (when value
                                  (walk walker value env (list* 1 n path)))))
                          (when initial-value
                            (push initial-value initial-forms))
                          (add (make-instance 'lexical-variable-binding ; TODO: special variable
                                              :name var
                                              :value initial-value)
                               (if (consp arg)
                                   (list* 0 n path)
                                   (cons n path))))))
                     ((nil)
                      (with-walker-bindings (var specializer)
                          (if (consp arg) arg (list arg t))
                        (declare (ignore specializer))
                        (assert-type var 'variable-symbol)
                        ;; TODO: special variable
                        (add (make-instance 'lexical-variable-binding :name var)
                             (if (consp arg)
                                 (list* 0 n path)
                                 (cons n path))))))))))
    (values (make-instance 'lambda-list-form
                           :variables walked-lambda-list
                           :initial-forms initial-forms
                           :path path)
            env)))

(defmethod walk-form ((walker walker) (name (eql 'defmethod)) form env path)
  (with-walker-bindings (name &rest args) (rest form)
    (multiple-value-bind (method-qualifiers args)
        (take-method-qualifiers args)
      (with-walker-bindings (specialized-lambda-list &body body) args
        (multiple-value-bind (specialized-lambda-list env)
            (walk-specialized-lambda-list walker
                                          specialized-lambda-list
                                          env
                                          (cons (+ 2 (length method-qualifiers)) path))
          (let* ((block-binding (make-instance 'block-binding :name name))
                 (env (extend-env env block-binding)))
            ;; TODO: declare
            (make-instance 'defmethod-form
                           :block-form (make-instance 'block-name-form
                                                      :binding block-binding
                                                      :path (cons 1 path))
                           :name name
                           :path (cons 0 path)
                           :lambda-list specialized-lambda-list
                           :body (make-instance 'implict-progn-form
                                                :forms (walk-forms walker
                                                                   body
                                                                   env
                                                                   path
                                                                   (+ 3 (length method-qualifiers)))
                                                :path path))))))))

(defmethod visit (visitor (ast defmethod-form))
  (visit visitor (ast-block-form ast))
  (visit visitor (ast-lambda-list ast))
  (visit visitor (ast-body ast)))
