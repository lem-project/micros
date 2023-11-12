(in-package :micros/walker)

(defclass simple-loop-form (ast)
  ((body :initarg :body
         :type implict-progn-form
         :reader ast-body)))

(defclass loop-form (ast)
  ((named :initarg :named
          :type variable-symbol
          :reader loop-form-named)
   (with-clauses :initarg :with-clauses
     :type (proper-list with-clause)
     :reader loop-form-with-clauses)
   (initial-clauses :initarg :initial-clauses
                    :type (proper-list initial-clause)
                    :reader loop-form-initial-clauses)
   (fianl-clauses :initarg :final-clauses
                  :type (proper-list final-clause)
                  :reader loop-form-final-clauses)
   (doing-forms :initarg :doing-forms
                :type (proper-list ast)
                :reader loop-form-doing-forms)
   (return-forms :initarg :return-forms
                 :type (proper-list ast)
                 :reader loop-form-return-forms)))

(defclass d-var (ast <with-binding-form>)
  ((binding :initarg :binding
            :reader ast-binding)))

(defclass with-clause (ast)
  ((d-vars :initarg :d-vars
           :type (proper-list d-var)
           :reader ast-d-vars)
   (value :initarg :value
          :reader ast-value)))

(defclass initial-clause (ast)
  ((forms :initarg :forms
          :type (proper-list ast)
          :reader ast-forms)))

(defclass final-clause (ast)
  ((forms :initarg :forms
          :type (proper-list ast)
          :reader ast-forms)))

(defclass it-form (ast)
  ())

(defun walk-d-var-spec (walker d-var-spec env path)
  (cond ((null d-var-spec)
         '())
        ((atom d-var-spec)
         (assert-type d-var-spec 'variable-symbol)
         (list (make-instance 'd-var
                              :binding (make-instance 'lexical-variable-binding
                                                      :name d-var-spec)
                              :path path)))
        (t
         (loop :for (elt . rest) :on d-var-spec
               :for n :from 0
               :append (walk-d-var-spec walker elt env (cons n path)) :into acc
               :finally (if (null rest)
                            (return acc)
                            (return
                              (append acc
                                      (walk-d-var-spec walker
                                                       rest
                                                       env
                                                       ;; Represents the path of b in (a . b)
                                                       (cons (+ n 2) path)))))))))

(defmethod walk-complex-loop-form ((walker walker) form env path)
  (assert (and (proper-list-p form) (eq 'loop (first form))))
  (let ((pos 0)
        (exps (rest form))
        (named-binding nil)
        (with-clauses '())
        (initial-clauses '())
        (final-clauses '())
        (doing-forms '())
        (return-forms '()))
    (labels ((lookahead ()
               (first exps))
             (next ()
               (walker-assert (not (null exps)))
               (incf pos)
               (pop exps))
             (accept (name)
               (when (and (typep (lookahead) '(or symbol string))
                          (string= name (lookahead)))
                 (next)
                 t))
             (exact-var ()
               (let ((var (next)))
                 (assert-type var 'variable-symbol)
                 var))

             (d-var-spec (path)
               (let ((d-var-spec (next)))
                 (walk-d-var-spec walker d-var-spec env path)))

             (name-clause ()
               (when (accept :named)
                 (let ((named (next)))
                   (assert-type named 'variable-symbol)
                   (setf named-binding (make-instance 'block-binding :name named))
                   (extend-env env named-binding))))
             (variable-clause ()
               (when (or (with-clause)
                         (initial-final)
                         (for-as-clause))
                 t))
             (variable-clause* ()
               (loop :while (variable-clause)))
             (with-clause ()
               (when (accept :with)
                 (let ((clauses
                           (loop :for d-vars := (d-var-spec (cons (1+ pos) path))
                                 :do (type-spec)
                                 :collect (make-instance
                                           'with-clause
                                           :path nil ; TODO
                                           :d-vars d-vars
                                           :value (when (accept :=)
                                                    (let ((value (walk walker
                                                                       (next)
                                                                       env
                                                                       (cons pos path))))
                                                      value)))
                                 :while (accept :and))))
                   (setf env
                         (extend-env* env
                                      (mapcan (lambda (clause)
                                                (mapcar #'ast-binding (ast-d-vars clause)))
                                              clauses)))
                   (setf with-clauses (append with-clauses clauses)))))
             (main-clause ()
               (or (unconditional)
                   (accumulation)
                   (conditional)
                   (termination-test)
                   (initial-final)
                   ))
             (main-clause* ()
               (loop :while (main-clause)))
             (initial-final ()
               (cond ((accept :initially)
                      (push (make-instance 'initial-clause :forms (compound-forms))
                            initial-clauses))
                     ((accept :finally)
                      (push (make-instance 'final-clause :forms (compound-forms))
                            final-clauses))))
             (unconditional ()
               (cond ((or (accept :do)
                          (accept :doing))
                      (setf doing-forms (append doing-forms (compound-forms))))
                     ((accept :return)
                      (if (accept :it)
                          (push (make-instance 'it-form :path (cons pos path))
                                return-forms)
                          (let ((form (next)))
                            (push (walk walker form env (cons pos path))
                                  return-forms))))))
             (accumulation ()
               ;; TODO
               )
             (conditional ()
               ;; TODO
               )
             (termination-test ()
               ;; TODO
               )
             (for-as-clause ()
               (loop :while (or (accept :for) (accept :as))
                     :collect (for-as-subclause)))
             (for-as-subclause ()
               (let ((var (exact-var)))
                 (type-spec)
                 (let ((binding (make-instance 'lexical-variable-binding :name var)))
                   (declare (ignore binding))
                   (cond ((accept :in)
                          )
                         ((accept :on)
                          )
                         ((accept :=)
                          )
                         ((accept :across)
                          )
                         ((accept :being)
                          )
                         (t
                          ;; TODO: error
                          )))))
             (type-spec ()
               (cond ((member (lookahead) '(t nil fixnum float))
                      (next)
                      t)
                     ((accept :of-type)
                      (next)
                      t)
                     (t
                      nil)))
             (compound-forms ()
               (let ((exp (next)))
                 (walker-assert (consp exp))
                 (cons (walk walker exp env (cons pos path))
                       (loop :for exp := (lookahead)
                             :while (consp exp)
                             :collect (walk walker exp env (cons pos path))
                             :do (next))))))
      (name-clause)
      (variable-clause*)
      (main-clause*)
      (make-instance 'loop-form
                     :named named-binding
                     :with-clauses (nreverse with-clauses)
                     :initial-clauses (nreverse initial-clauses)
                     :final-clauses (nreverse final-clauses)
                     :doing-forms (nreverse doing-forms)
                     :return-forms (nreverse return-forms)))))

(defmethod walk-form ((walker walker) (name (eql 'loop)) form env path)
  (with-walker-bindings (&body forms) (rest form)
    (if (and forms (symbolp (first forms)))
        (walk-complex-loop-form walker form env path)
        (make-instance 'simple-loop-form
                       :body (make-instance 'implict-progn-form
                                            :path path
                                            :forms (walk-forms walker forms env path 1))))))

(defmethod visit (visitor (ast d-var))
  nil)

(defmethod visit (visitor (ast loop-form))
  (visit-foreach visitor (loop-form-with-clauses ast))
  (visit-foreach visitor (loop-form-initial-clauses ast))
  (visit-foreach visitor (loop-form-final-clauses ast))
  (visit-foreach visitor (loop-form-doing-forms ast))
  (visit-foreach visitor (loop-form-return-forms ast)))

(defmethod visit (visitor (ast with-clause))
  (visit-foreach visitor (ast-d-vars ast))
  (visit visitor (ast-value ast)))

(defmethod visit (visitor (ast initial-clause))
  (visit-foreach visitor (ast-forms ast)))

(defmethod visit (visitor (ast final-clause))
  (visit-foreach visitor (ast-forms ast)))

(defmethod visit (visitor (ast it-form))
  nil)

(defmethod visit (visitor (ast simple-loop-form))
  (visit visitor (ast-body ast)))
