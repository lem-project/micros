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
   (for-as-clauses :initarg :for-as-clauses
                   :reader loop-form-for-as-clauses)
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

(defclass <for-as-in-on-list-clause> (ast)
  ((d-vars :initarg :d-vars
           :reader ast-d-vars)
   (in-on :initarg :in-on
          :reader ast-in-on)
   (by :initarg :by
       :reader ast-by)))

(defclass for-as-in-list-clause (<for-as-in-on-list-clause>) ())
(defclass for-as-on-list-clause (<for-as-in-on-list-clause>) ())

(defclass for-as-equals-then-clause (ast)
  ((d-vars :initarg :d-vars
            :reader ast-d-vars)
   (equals :initarg :equals
           :reader ast-equals)
   (then :initarg :then
         :reader ast-then)))

(defclass for-as-across-clause (ast)
  ((d-vars :initarg :d-vars
           :reader ast-d-vars)
   (across :initarg :across
           :reader ast-across)))

(defclass for-as-hash-clause (ast)
  ((d-vars :initarg :d-vars
           :reader ast-d-vars)
   (hash-table :initarg :hash-table
               :reader ast-hash-table)))

(defclass for-as-package-clause (ast)
  ((d-vars :initarg :d-vars
           :reader ast-d-vars)
   (package :initarg :package
            :reader ast-package)))

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
  (let ((pos 1)
        (exps (rest form))
        (named-binding nil)
        (with-clauses '())
        (initial-clauses '())
        (final-clauses '())
        (for-as-clauses '())
        (doing-forms '())
        (return-forms '()))
    (labels ((lookahead ()
               (first exps))
             (next ()
               (walker-assert (not (null exps)))
               (incf pos)
               (pop exps))
             (match (names)
               (when (symbolp (lookahead))
                 (loop :for name :in names
                       :thereis (string= name (lookahead)))))
             (accept (&rest names)
               (when (match names)
                 (next)
                 t))
             (exact (&rest names)
               (walker-assert (match names))
               (next)
               t)
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
                         (loop :for d-vars := (d-var-spec (cons pos path))
                               :do (type-spec)
                               :collect (make-instance
                                         'with-clause
                                         :path nil ; TODO
                                         :d-vars d-vars
                                         :value (when (accept :=)
                                                  (let* ((pos pos)
                                                         (value (walk walker
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
                   (initial-final)))
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
               (cond ((accept :do :doing)
                      (setf doing-forms (append doing-forms (compound-forms))))
                     ((accept :return)
                      (let ((pos pos))
                        (if (accept :it)
                            (push (make-instance 'it-form :path (cons pos path))
                                  return-forms)
                            (let ((form (next)))
                              (push (walk walker form env (cons pos path))
                                    return-forms)))))))
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
               (loop :while (accept :for :as)
                     :collect (for-as-subclause)))
             (for-as-subclause ()
               (let ((for-pos (1- pos))
                     (d-vars (d-var-spec (cons pos path))))
                 (type-spec)
                 (cond ((accept :in)
                        (for-as-in-list for-pos d-vars))
                       ((accept :on)
                        (for-as-on-list for-pos d-vars))
                       ((accept :=)
                        (for-as-equals-then for-pos d-vars))
                       ((accept :across)
                        (for-as-across for-pos d-vars))
                       ((accept :being)
                        (setf d-vars (for-as-being for-pos d-vars)))
                       (t
                        ;; TODO: error
                        ;; TODO: from, to, downfrom, downto, above, by
                        ))
                 (setf env (extend-env* env (mapcar #'ast-binding d-vars)))))

             (for-as-in-on-list (ast-class for-pos d-vars)
               (let* ((in (walk walker (next) env (cons (+ for-pos 3) path)))
                      (by (when (accept :by)
                            (walk walker (next) env (cons (+ for-pos 5) path)))))
                 (push (make-instance ast-class
                                      :path (cons for-pos path)
                                      :d-vars d-vars
                                      :in-on in
                                      :by by)
                       for-as-clauses)))
             (for-as-in-list (for-pos d-vars)
               (for-as-in-on-list 'for-as-in-list-clause for-pos d-vars))
             (for-as-on-list (for-pos d-vars)
               (for-as-in-on-list 'for-as-on-list-clause for-pos d-vars))
             (for-as-equals-then (for-pos d-vars)
               (let* ((env (extend-env* env (mapcar #'ast-binding d-vars)))
                      (in (walk walker (next) env (cons (+ for-pos 3) path)))
                      (by (when (accept :then)
                            (walk walker (next) env (cons (+ for-pos 5) path)))))
                 (push (make-instance 'for-as-equals-then-clause
                                      :path (cons for-pos path)
                                      :d-vars d-vars
                                      :equals in
                                      :then by)
                       for-as-clauses)))
             (for-as-across (for-pos d-vars)
               (let ((across (walk walker (next) env (cons (+ for-pos 3) path))))
                 (push (make-instance 'for-as-across-clause
                                      :path (cons for-pos path)
                                      :d-vars d-vars
                                      :across across)
                       for-as-clauses)))

             (for-as-being-hash (for-pos d-vars other-var-keyword)
               (let* ((hash-table-pos pos)
                      (hash-table (walk walker (next) env (cons hash-table-pos path))))
                 (when (accept :using)
                   (let ((using-pos pos)
                         (using (next)))
                     (with-walker-bindings (hash-value other-var) using
                       (walker-assert (and (symbolp hash-value)
                                           (string= hash-value other-var-keyword)))
                       (assert-type other-var 'variable-symbol)
                       (push (make-instance
                              'd-var
                              :binding (make-instance 'lexical-variable-binding
                                                      :name other-var)
                              :path (list* 1 using-pos path))
                             d-vars))))
                 (push (make-instance 'for-as-hash-clause
                                      :path (cons for-pos path)
                                      :d-vars d-vars
                                      :hash-table hash-table)
                       for-as-clauses))
               d-vars)
             (for-as-package (for-pos d-vars)
               (push (make-instance 'for-as-package-clause
                                    :path (cons for-pos path)
                                    :d-vars d-vars
                                    :package (when (accept :in :of)
                                               (let ((package-pos pos))
                                                 (walk walker
                                                       (next)
                                                       env
                                                       (cons package-pos path)))))
                     for-as-clauses))
             (for-as-being (for-pos d-vars)
               (exact :each :the)
               (cond ((accept :hash-key :hash-keys)
                      (exact :in :of)
                      (for-as-being-hash for-pos d-vars :hash-value))
                     ((accept :hash-value :hash-values)
                      (exact :in :of)
                      (for-as-being-hash for-pos d-vars :hash-key))
                     (t
                      (exact :symbol :symbols :present-symbol :present-symbols
                             :external-symbol :external-symbols)
                      (for-as-package for-pos d-vars)
                      d-vars)))
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
               (let ((pos pos)
                     (exp (next)))
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
                     :for-as-clauses (nreverse for-as-clauses)
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
  (visit-foreach visitor (loop-form-for-as-clauses ast))
  (visit-foreach visitor (loop-form-doing-forms ast))
  (visit-foreach visitor (loop-form-return-forms ast)))

(defmethod visit (visitor (ast with-clause))
  (visit-foreach visitor (ast-d-vars ast))
  (when (ast-value ast)
    (visit visitor (ast-value ast))))

(defmethod visit (visitor (ast initial-clause))
  (visit-foreach visitor (ast-forms ast)))

(defmethod visit (visitor (ast final-clause))
  (visit-foreach visitor (ast-forms ast)))

(defmethod visit (visitor (ast it-form))
  nil)

(defmethod visit (visitor (ast <for-as-in-on-list-clause>))
  (visit-foreach visitor (ast-d-vars ast))
  (visit visitor (ast-in-on ast))
  (when (ast-by ast) (visit visitor (ast-by ast))))

(defmethod visit (visitor (ast for-as-equals-then-clause))
  (visit-foreach visitor (ast-d-vars ast))
  (visit visitor (ast-equals ast))
  (when (ast-then ast) (visit visitor (ast-then ast))))

(defmethod visit (visitor (ast for-as-across-clause))
  (visit-foreach visitor (ast-d-vars ast))
  (visit visitor (ast-across ast)))

(defmethod visit (visitor (ast for-as-hash-clause))
  (visit-foreach visitor (ast-d-vars ast))
  (visit visitor (ast-hash-table ast)))

(defmethod visit (visitor (ast for-as-package-clause))
  (visit-foreach visitor (ast-d-vars ast))
  (when (ast-package ast)
    (visit visitor (ast-package ast))))

(defmethod visit (visitor (ast simple-loop-form))
  (visit visitor (ast-body ast)))
