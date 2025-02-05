(in-package :micros/walker)

(define-condition unimplemented ()
  ((context :initarg :context
            :reader unimplemented-context)))

(defun unimplemented (context &key form path)
  (cerror "continue" 'unimplemented :context context)
  (make-instance 'unknown-form :form form :path path))

(defmacro walker-assert (predicate)
  `(assert ,predicate))

(defun assert-type (value expected-type)
  (walker-assert (typep value expected-type)))

(defun assert-length (value expected-length)
  (walker-assert (= expected-length (length value))))

(defun assert-length-between (value min-length &optional max-length)
  (if max-length
      (walker-assert (<= min-length (length value) max-length))
      (walker-assert (<= min-length (length value)))))

(defclass <printable> () ())

(defmethod print-object ((object <printable>) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (loop :for slot :in (micros/mop:class-slots (class-of object))
                 :for name := (micros/mop:slot-definition-name slot)
                 :when (slot-boundp object name)
                 :collect (make-keyword name) :and
                 :collect (slot-value object name))
           stream)))

;; env
(defclass binding (<printable>)
  ((name :initarg :name
         :reader binding-name)
   (value :initarg :value
          :reader binding-value
          :writer (setf binding-value))))

(defclass special-variable-binding (binding) ())
(defclass lexical-variable-binding (binding) ())
(defclass lexical-function-binding (binding) ())
(defclass tagbody-binding (binding) ())
(defclass block-binding (binding) ())
(defclass macrolet-binding (binding)
  ((lambda-list :initarg :lambda-list
                :reader macrolet-binding-lambda-list)))

(defun extend-env (env binding)
  (cons binding env))

(defun extend-env* (env bindings)
  (append bindings env))

(defun lookup-binding (env name &rest types)
  (loop :for binding :in env
        :when (and (some (lambda (type) (typep binding type))
                         types)
                   (eq name (binding-name binding)))
        :return binding))

(defun lookup-variable (env name)
  (lookup-binding env name 'lexical-variable-binding 'special-variable-binding))

(defun lookup-function-binding (env name)
  (lookup-binding env name 'lexical-function-binding))

(defun lookup-macrolet-binding (env name)
  (lookup-binding env name 'macrolet-binding))

(defun lookup-tagbody-binding (env name)
  (lookup-binding env name 'tagbody-binding))

(defun lookup-block-binding (env name)
  (lookup-binding env name 'block-binding))

(defclass <with-binding-form> () ())

;; ast
(defclass ast (<printable>)
  ((path :initarg :path
         :initform nil
         :reader ast-path)))

(defgeneric ast-equal (ast1 ast2))

(defmethod ast-equal (object1 object2)
  (equal object1 object2))

(defmethod ast-equal ((object1 list) (object2 list))
  (and (= (length object1) (length object2))
       (every #'ast-equal object1 object2)))

(defmethod ast-equal ((object1 ast) (object2 ast))
  (and (eq (type-of object1) (type-of object2))
       (loop :for slot :in (micros/mop:class-direct-slots (class-of object1))
             :always (ast-equal (slot-value object1 (micros/mop:slot-definition-name slot))
                                (slot-value object2 (micros/mop:slot-definition-name slot))))))

(defclass unknown-form (ast)
  ((form :initarg :form
         :reader ast-form)))

(defclass constant-form (ast)
  ((value :initarg :value
          :reader ast-value)))

(defclass load-time-value-form (ast)
  ((form :initarg :form
         :type ast
         :reader ast-form)
   (read-only-p :initarg :read-only-p
                :type ast
                :reader ast-read-only-p)))

(defclass eval-when-form (ast)
  ((situations :initarg :situations
               :reader ast-situations)
   (body :initarg :body
         :reader ast-body)))

(defclass lambda-list-variable-form (ast <with-binding-form>)
  ((binding :initarg :binding
            :type binding
            :reader ast-binding)))

(defclass lambda-list-form (ast)
  ((variables :initarg :variables
              :type (proper-list lambda-list-variable-form)
              :reader ast-variables)
   (initial-forms :initarg :initial-forms
                  :type (proper-list ast)
                  :reader ast-initial-forms)))

(defclass lambda-form (ast)
  ((documentation :initarg :documentation :reader ast-documentation)
   (lambda-list :initarg :lambda-list :reader ast-lambda-list)
   (body :initarg :body :reader ast-body)))

(defclass named-lambda-form (ast)
  ((name :initarg :name :reader ast-name)
   (documentation :initarg :documentation :reader ast-documentation)
   (lambda-list :initarg :lambda-list :reader ast-lambda-list)
   (body :initarg :body :reader ast-body)))

(defclass multiple-value-call-form (ast)
  ((function :initarg :function
             :reader ast-function)
   (arguments :initarg :arguments
              :reader ast-arguments)))

(defclass multiple-value-prog1-form (ast)
  ((values-form :initarg :values-form
                :reader ast-values-form)
   (forms :initarg :forms
          :reader ast-forms)))

(defclass progn-form (ast)
  ((body :initarg :body :reader ast-body)))

(defclass if-form (ast)
  ((test :initarg :test :reader ast-test)
   (then :initarg :then :reader ast-then)
   (else :initarg :else :reader ast-else)))

(defclass catch-form (ast)
  ((tag :initarg :tag :reader ast-tag)
   (body :initarg :body :reader ast-body)))

(defclass throw-form (ast)
  ((tag :initarg :tag :reader ast-tag)
   (value :initarg :value :reader ast-value)))

(defclass let-binding-form (ast <with-binding-form>)
  ((binding :initarg :binding
            :reader ast-binding)
   (value :initarg :value
          :reader ast-value)))

(defclass let-form (ast)
  ((bindings :initarg :bindings
             :type (proper-list let-binding-form)
             :reader ast-bindings)
   (body :initarg :body
         :reader ast-body)))

(defclass flet-binding-form (ast <with-binding-form>)
  ((binding :initarg :binding :reader ast-binding)
   (lambda-list :initarg :lambda-list :reader ast-lambda-list)
   (body :initarg :body :reader ast-body)))

(defclass flet-form (ast)
  ((bindings :initarg :bindings
             :type (proper-list flet-binding-form)
             :reader ast-bindings)
   (body :initarg :body
         :reader ast-body)))

(defclass setq-form (ast)
  ((arguments :initarg :arguments
              :reader ast-arguments)))

(defclass unwind-protect-form (ast)
  ((protected-form :initarg :protected-form
                   :reader ast-proctected-form)
   (cleanup-forms :initarg :cleanup-forms
                  :reader ast-cleanup-forms)))

(defclass block-name-form (ast <with-binding-form>)
  ((binding :initarg :binding
            :reader ast-binding)))

(defclass block-form (ast)
  ((name :initarg :name
         :type block-name-form
         :reader ast-name)
   (body :initarg :body
         :reader ast-body)))

(defclass return-from-form (ast)
  ((name :initarg :name
         :type block-name-form
         :reader ast-name)
   (value :initarg :value
          :reader ast-value)))

(defclass tagbody-form (ast)
  ((statements :type (proper-list (or ast tag))
               :initarg :statements
               :reader ast-statements)))

(defclass tag (ast <with-binding-form>)
  ((binding :type tagbody-binding
            :initarg :binding
            :reader ast-binding)))

(defclass go-form (ast)
  ((tag :type tag
        :initarg :tag
        :reader ast-tag)))

(defclass the-form (ast)
  ((value-type :initarg :value-type
               :reader ast-value-type)
   (form :initarg :form
         :reader ast-form)))

(defclass call-function-form (ast)
  ((operator :initarg :operator
             :reader ast-operator)
   (arguments :initarg :arguments
              :reader ast-arguments)))

(defclass call-local-function-form (ast <with-binding-form>)
  ((binding :initarg :binding
            :reader ast-binding)
   (arguments :initarg :arguments
              :reader ast-arguments)))

(defclass lambda-call-form (ast)
  ((lambda-form :initarg :lambda-form
                :type lambda-form
                :reader ast-lambda-form)
   (arguments :initarg :arguments
              :reader ast-arguments)))

(defclass special-variable (ast <with-binding-form>)
  ((binding :initarg :binding
            :type special-variable-binding
            :reader ast-binding)))

(defclass lexical-variable (ast <with-binding-form>)
  ((binding :initarg :binding
            :type lexical-variable-binding
            :reader ast-binding)))

(defclass local-function-form (ast <with-binding-form>)
  ((binding :initarg :binding
            :type lexical-function-binding
            :reader ast-binding)))

(defclass function-form (ast)
  ((name :initarg :name
         :type variable-symbol
         :reader ast-name)))

(defclass implict-progn-form (ast)
  ((forms :initarg :forms
          :type (proper-list ast)
          :reader ast-forms)))

(defclass macrolet-form (ast)
  ((body :initarg :body
         :type implict-progn-form
         :reader ast-body)))

(defclass with-single-binding-form (ast <with-binding-form>)
  ((binding :initarg :binding
            :reader ast-binding)
   (binding-args :initarg :binding-args
                 :type (proper-list ast)
                 :reader ast-binding-args)
   (body :initarg :body
         :type implict-progn-form
         :reader ast-body)))

;;
(defstruct declaration-spec
  specials)

(defun parse-declaration-specifiers (declare-forms)
  (let ((specials '()))
    (dolist (declare-form declare-forms)
      (assert (eq 'declare (first declare-form)))
      (dolist (specifier (rest declare-form))
        (assert-type specifier 'proper-list)
        (case (first specifier)
          ((special)
           (setf specials (append specials (rest specifier)))))))
    (make-declaration-spec :specials specials)))

;; walker
(defclass walker ()
  ((special-variable-table
    :initform (make-hash-table :test 'eq)
    :reader walker-special-variable-table)
   (walking-without-expanding-unknown-macros
    :initarg :walking-without-expanding-unknown-macros
    :initform t
    :reader walking-without-expanding-unknown-macros-p)))

(defmethod get-special-variable-binding ((walker walker) symbol)
  (or (gethash symbol (walker-special-variable-table walker))
      (setf (gethash symbol (walker-special-variable-table walker))
            (make-instance 'special-variable-binding :name symbol))))

(defun make-variable (walker declaration-spec name &optional value)
  (declare (ignore value))
  (if (and declaration-spec
           (member name (declaration-spec-specials declaration-spec)))
      (get-special-variable-binding walker name)
      (make-instance 'lexical-variable-binding :name name)))

(defgeneric walk-form (walker name form env path))

(defun walk-forms (walker forms env path offset)
  (loop :for subform :in forms
        :for n :from 0
        :collect (walk walker subform env (cons (+ offset n) path))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compute-required-arguments-of-lambda-list (lambda-list)
    (loop :with count := 0
          :for (ll . rest) :on lambda-list
          :do (case ll
                (&optional
                 (return (values count (+ count (length rest)))))
                ((&rest &body)
                 (return (values count nil)))
                (otherwise
                 (incf count)))
          :finally (return (values count count)))))

(defmacro with-walker-bindings (lambda-list form &body body)
  (let ((g-form (gensym "FORM")))
    (multiple-value-bind (min max) (compute-required-arguments-of-lambda-list lambda-list)
      `(let ((,g-form ,form))
         (assert-type ,g-form 'proper-list)
         ,(cond ((null max)
                 `(assert-length-between ,g-form ,min))
                ((= min max)
                 `(assert-length ,g-form ,min))
                (t
                 `(assert-length-between ,g-form ,min ,max)))
         (destructuring-bind ,lambda-list ,g-form ,@body)))))

(defmethod walk-form ((walker walker) (name (eql 'block)) form env path)
  (with-walker-bindings (name &body body) (rest form)
    (assert-type name 'symbol)
    (let* ((binding (make-instance 'block-binding :name name))
           (env (extend-env env binding)))
      (make-instance 'block-form
                     :name (make-instance 'block-name-form :binding binding :path (cons 1 path))
                     :body (walk-forms walker body env path 2)
                     :path (cons 0 path)))))

(defmethod walk-form ((walker walker) (name (eql 'return-from)) form env path)
  (with-walker-bindings (name &optional value) (rest form)
    (assert-type name 'symbol)
    (let ((binding (lookup-block-binding env name)))
      (walker-assert binding)
      (make-instance 'return-from-form
                     :name (make-instance 'block-name-form :binding binding :path (cons 1 path))
                     :value (walk walker value env (cons 2 path))
                     :path (cons 0 path)))))

(defmethod walk-form ((walker walker) (name (eql 'catch)) form env path)
  (with-walker-bindings (tag &body body) (rest form)
    (make-instance 'catch-form
                   :tag (walk walker tag env (cons 1 path))
                   :body (walk-forms walker body env path 2)
                   :path (cons 0 path))))

(defmethod walk-form ((walker walker) (name (eql 'throw)) form env path)
  (with-walker-bindings (tag value) (rest form)
    (make-instance 'throw-form
                   :tag (walk walker tag env (cons 1 path))
                   :value (walk walker value env (cons 2 path))
                   :path (cons 0 path))))

(defmethod walk-form ((walker walker) (name (eql 'eval-when)) form env path)
  (with-walker-bindings (situations &body body) (rest form)
    (make-instance 'eval-when-form
                   :situations situations
                   :body (walk-forms walker body env path 2)
                   :path (cons 0 path))))

(defmethod walk-form ((walker walker) (name (eql 'if)) form env path)
  (with-walker-bindings (test then &optional else) (rest form)
    (make-instance 'if-form
                   :test (walk walker test env (cons 1 path))
                   :then (walk walker then env (cons 2 path))
                   :else (walk walker else env (cons 3 path))
                   :path (cons 0 path))))

(defmethod walk-flet-binding ((walker walker) definition env path)
  (with-walker-bindings (name lambda-list &body body) definition
    (assert-type name 'variable-symbol)
    (multiple-value-bind (lambda-list env)
        (walk-lambda-list walker lambda-list env (cons 1 path))
      ;; TODO: declare
      (let ((body (walk-forms walker body env path 2)))
        (make-instance 'flet-binding-form
                       :binding (make-instance 'lexical-function-binding
                                               :name name
                                               :value body)
                       :lambda-list lambda-list
                       :body body
                       :path (cons 0 path))))))

(defmethod walk-form ((walker walker) (name (eql 'flet)) form env path)
  (with-walker-bindings (definitions &body body) (rest form)
    (let ((bindings (loop :for definition :in definitions
                          :for n :from 0
                          :collect (walk-flet-binding walker definition env (list* n 1 path)))))
      (make-instance 'flet-form
                     :bindings bindings
                     :body (walk-forms walker
                                       body
                                       (extend-env* env (mapcar #'ast-binding bindings))
                                       path
                                       2)
                     :path (cons 0 path)))))

(defmethod walk-labels-binding ((walker walker) definition env path binding)
  (with-walker-bindings (name lambda-list &body body) definition
    (assert-type name 'variable-symbol)
    (multiple-value-bind (lambda-list env)
        (walk-lambda-list walker lambda-list env (cons 1 path))
      ;; TODO: declare
      (let ((body (walk-forms walker body env path 2)))
        (setf (binding-value binding) body)
        (make-instance 'flet-binding-form
                       :binding binding
                       :lambda-list lambda-list
                       :body body
                       :path (cons 0 path))))))

(defmethod walk-form ((walker walker) (name (eql 'labels)) form env path)
  (with-walker-bindings (definitions &body body) (rest form)
    (let* ((bindings (loop :for name :in (mapcar #'first definitions)
                           :collect (make-instance 'lexical-function-binding
                                                   :name name)))
           (env (extend-env* env bindings)))
      (make-instance 'flet-form
                     :bindings (loop :for definition :in definitions
                                     :for binding :in bindings
                                     :for n :from 0
                                     :collect (walk-labels-binding walker
                                                                   definition
                                                                   env
                                                                   (list* n 1 path)
                                                                   binding))
                     :body (walk-forms walker body env path 2)
                     :path (cons 0 path)))))

(defmethod walk-lambda-list ((walker walker) lambda-list env path &key declaration-spec)
  (let ((walked-lambda-list '())
        (initial-forms '()))
    (flet ((add (binding path)
             (setf env (extend-env env binding))
             (push (make-instance 'lambda-list-variable-form
                                  :binding binding
                                  :path path)
                   walked-lambda-list)))
      (loop :with state := nil
            :for n :from 0
            :for arg :in lambda-list
            :do (case arg
                  ((&aux &key &rest &body &optional)
                   (setf state arg))
                  (otherwise
                   (ecase state
                     ((&rest &body)
                      (assert-type arg 'variable-symbol)
                      (add (make-variable walker declaration-spec arg)
                           (cons n path)))
                     ((&key &optional &aux)
                      (let* ((var-value (uiop:ensure-list arg))
                             (var (first var-value))
                             (value (second var-value)))
                        (when (and (eq state '&key)
                                   (consp var))
                          (setf var (second var)))
                        (assert-type var 'variable-symbol)
                        (let ((initial-value
                                (when value
                                  (walk walker value env (list* 1 n path)))))
                          (when initial-value
                            (push initial-value initial-forms))
                          (add (make-variable walker declaration-spec var initial-value)
                               (if (consp arg)
                                   (list* 0 n path)
                                   (cons n path))))))
                     ((nil)
                      (add (make-variable walker declaration-spec arg)
                           (cons n path))))))))
    (values (make-instance 'lambda-list-form
                           :variables (nreverse walked-lambda-list)
                           :initial-forms initial-forms
                           :path path)
            env)))

(defun walk-lambda-list-and-body (walker lambda-list body env path &optional name)
  (multiple-value-bind (body declare-forms documentation)
      (parse-body body :documentation t)
    (let ((declaration-spec (parse-declaration-specifiers declare-forms)))
      (multiple-value-bind (lambda-list env)
          (walk-lambda-list walker
                            lambda-list
                            env
                            (cons 1 path)
                            :declaration-spec declaration-spec)
        (cond (name
               (make-instance
                'named-lambda-form
                :name name
                :documentation documentation
                :lambda-list lambda-list
                :body (walk-forms walker
                                  body
                                  (extend-env env (make-instance 'block-binding :name name))
                                  path
                                  (+ 2 (length declare-forms)))
                :path (cons 0 path)))
              (t
               (make-instance
                'lambda-form
                :documentation documentation
                :lambda-list lambda-list
                :body (walk-forms walker body env path (+ 2 (length declare-forms)))
                :path (cons 0 path))))))))

(defmethod walk-lambda-form ((walker walker) form env path)
  (assert-type (first form) '(member lambda #+:sbcl  sb-int:named-lambda))
  (ecase (first form)
    ((lambda)
     (with-walker-bindings (lambda-list &body body) (rest form)
       (walk-lambda-list-and-body walker lambda-list body env path)))
    #+:sbcl ((sb-int:named-lambda)
     (with-walker-bindings (name lambda-list &body body) (rest form)
       (walk-lambda-list-and-body walker lambda-list body env path name)))))

(defmethod walk-form ((walker walker) (name (eql 'function)) form env path)
  (with-walker-bindings (thing) (rest form)
    (cond ((symbolp thing)
           (assert-type thing 'variable-symbol)
           (let ((binding (lookup-function-binding env thing)))
             (if binding
                 (make-instance 'local-function-form :binding binding :path path)
                 (make-instance 'function-form :name thing :path path))))
          (t
           (walk-lambda-form walker thing env (cons 1 path))))))

(defmethod walk-form ((walker walker) (name (eql 'lambda)) form env path)
  (walk-lambda-form walker form env path))

(defmethod walk-let-binding-form ((walker walker) binding-form env path)
  (with-walker-bindings (var value)
      (if (consp binding-form)
          binding-form
          (list binding-form nil))
    (assert-type var 'variable-symbol)
    (let ((value (walk walker value env (cons 1 path))))
      (make-instance 'let-binding-form
                     :path (if (consp binding-form) (cons 0 path) path)
                     :binding (make-instance 'lexical-variable-binding
                                             :name var
                                             :value value)
                     :value value))))

(defmethod walk-form ((walker walker) (name (eql 'let)) form env path)
  (with-walker-bindings (bindings-form &body body) (rest form)
    (assert-type bindings-form 'proper-list)
    (let ((bindings
            (loop :for binding-form :in bindings-form
                  :for n :from 0
                  :collect (walk-let-binding-form walker binding-form env (list* n 1 path)))))
      (multiple-value-bind (forms declare-form)
          (parse-body body)
        ;; TODO: declare
        (make-instance 'let-form
                       :path (cons 0 path)
                       :bindings bindings
                       :body (walk-forms walker
                                         forms
                                         (extend-env* env (mapcar #'ast-binding bindings))
                                         path
                                         (+ 2 (length declare-form))))))))

(defmethod walk-form ((walker walker) (name (eql 'let*)) form env path)
  (with-walker-bindings (bindings-form &body body) (rest form)
    (assert-type bindings-form 'proper-list)
    (let ((bindings
            (loop :for binding-form :in bindings-form
                  :for n :from 0
                  :collect (let ((binding
                                   (walk-let-binding-form walker
                                                          binding-form
                                                          env
                                                          (list* n 1 path))))
                             (setf env (extend-env env (ast-binding binding)))
                             binding))))
      (multiple-value-bind (forms declare-form)
          (parse-body body)
        ;; TODO: declare
        (make-instance 'let-form
                       :path (cons 0 path)
                       :bindings bindings
                       :body (walk-forms walker
                                         forms
                                         env
                                         path
                                         (+ 2 (length declare-form))))))))

(defmethod walk-form ((walker walker) (name (eql 'load-time-value)) form env path)
  (with-walker-bindings (form &optional read-only-p) (rest form)
    (make-instance 'load-time-value-form
                   :path (cons 0 path)
                   :form (walk walker form env (cons 1 path))
                   :read-only-p (walk walker read-only-p env (cons 2 path)))))

(defmethod walk-form ((walker walker) (name (eql 'locally)) form env path)
  (unimplemented name :form form :path path))

(defmethod walk-form ((walker walker) (name (eql 'macrolet)) form env path)
  (with-walker-bindings (definitions &body body) (rest form)
    (let ((bindings (loop :for definition :in definitions
                          :collect (with-walker-bindings (name lambda-list &body body) definition
                                     (declare (ignore body))
                                     (assert-type name 'variable-symbol)
                                     (make-instance 'macrolet-binding
                                                    :name name
                                                    :lambda-list lambda-list)))))
      (make-instance 'macrolet-form
                     :path (cons 0 path)
                     :body (make-instance 'implict-progn-form
                                          :path (cons 0 path)
                                          :forms (walk-forms walker
                                                             body
                                                             (extend-env* env bindings)
                                                             path ;???
                                                             2))))))

(defmethod walk-form ((walker walker) (name (eql 'multiple-value-call)) form env path)
  (with-walker-bindings (function argument &rest arguments) (rest form)
    (make-instance 'multiple-value-call-form
                   :path (cons 0 path)
                   :function (walk walker function env (cons 1 path))
                   :arguments (cons (walk walker argument env (cons 2 path))
                                    (walk-forms walker arguments env path 3)))))

(defmethod walk-form ((walker walker) (name (eql 'multiple-value-prog1)) form env path)
  (with-walker-bindings (values-form &rest forms) (rest form)
    (make-instance 'multiple-value-prog1-form
                   :path (cons 0 path)
                   :values-form (walk walker values-form env (cons 1 path))
                   :forms (walk-forms walker forms env path 2))))

(defmethod walk-form ((walker walker) (name (eql 'progn)) form env path)
  (with-walker-bindings (&rest forms) (rest form)
    (make-instance 'progn-form
                   :path (cons 0 path)
                   :body (walk-forms walker forms env path 1))))

(defmethod walk-form ((walker walker) (name (eql 'progv)) form env path)
  (unimplemented name :form form :path path))

(defmethod walk-form ((walker walker) (name (eql 'quote)) form env path)
  (with-walker-bindings (value) (rest form)
    (make-instance 'constant-form
                   :path (cons 0 path)
                   :value value)))

(defmethod walk-form ((walker walker) (name (eql 'setq)) form env path)
  (let ((name-value-pairs (rest form)))
    (make-instance 'setq-form
                   :path (cons 0 path)
                   :arguments (loop :for (name value) :on name-value-pairs :by #'cddr
                                    :for n :from 1 :by 2
                                    :do (assert-type name 'symbol)
                                    :collect (walk-variable walker name env (cons n path))
                                    :collect (walk walker value env (cons (1+ n) path))))))

(defmethod walk-form ((walker walker) (name (eql 'symbol-macrolet)) form env path)
  (unimplemented name :form form :path path))

(defmethod walk-form ((walker walker) (name (eql 'tagbody)) form env path)
  (with-walker-bindings (&rest statements) (rest form)
    (let* ((bindings (mapcar (lambda (tag)
                               (make-instance 'tagbody-binding :name tag))
                             (remove-if-not #'symbolp statements)))
           (env (extend-env* env bindings))
           (statements
             (loop :for statement :in statements
                   :for n :from 1
                   :collect (if (symbolp statement)
                                (make-instance 'tag
                                               :binding (lookup-tagbody-binding env statement)
                                               :path (cons n path))
                                (walk walker statement env (cons n path))))))
      (make-instance 'tagbody-form
                     :path (cons 0 path)
                     :statements statements))))

(defmethod walk-form ((walker walker) (name (eql 'go)) form env path)
  (with-walker-bindings (tag) (rest form)
    (make-instance 'go-form
                   :tag (make-instance 'tag
                                       :binding (lookup-tagbody-binding env tag)
                                       :path (cons 1 path))
                   :path (cons 0 path))))

(defmethod walk-form ((walker walker) (name (eql 'the)) form env path)
  (with-walker-bindings (value-type form) (rest form)
    (make-instance 'the-form
                   :path (cons 0 path)
                   :value-type value-type
                   :form (walk walker form env (cons 2 path)))))

(defmethod walk-form ((walker walker) (name (eql 'unwind-protect)) form env path)
  (with-walker-bindings (protected-form &body cleanup-forms) (rest form)
    (make-instance 'unwind-protect-form
                   :protected-form (walk walker protected-form env (cons 1 path))
                   :cleanup-forms (walk-forms walker cleanup-forms env path 2)
                   :path (cons 0 path))))

(defmethod walk-with-single-binding-form ((walker walker) form path env)
  (with-walker-bindings (binding-form &body body) (rest form)
    (with-walker-bindings (var &rest binding-args) binding-form
      (assert-type var 'variable-symbol)
      (multiple-value-bind (body declare-forms)
          (parse-body body)
        (let* ((declaration-spec (parse-declaration-specifiers declare-forms))
               (binding (make-variable walker declaration-spec var)))
          (make-instance 'with-single-binding-form
                         :path (list* 0 1 path)
                         :binding binding
                         :binding-args (loop :for arg :in binding-args
                                             :for n :from 1
                                             :collect (walk walker arg env (list* n 1 path)))
                         :body (make-instance 'implict-progn-form
                                              :path path
                                              :forms (walk-forms walker
                                                                 body
                                                                 (extend-env env binding)
                                                                 path
                                                                 2))))))))

(defun walk-macro (walker form path env lambda-list)
  (let ((body-pos (position '&body lambda-list)))
    (cond (;; (with-* (var &rest forms) &body body)
           (and (string-prefix-p "WITH-" (string (first form)))
                (<= 2 (length form))
                (typep (first (second form)) 'variable-symbol)
                (<= 1 body-pos))
           (walk-with-single-binding-form walker form path env))
          (t
           (if body-pos
               (make-instance 'implict-progn-form
                              :path path
                              :forms (walk-forms walker
                                                 (nthcdr body-pos (rest form))
                                                 env
                                                 path
                                                 (1+ body-pos)))
               (unimplemented form :form form :path path))))))

(defmethod walk-lambda-call-form ((walker walker) form env path)
  (with-walker-bindings (lambda-form &rest args) form
    (make-instance 'lambda-call-form
                   :lambda-form (walk-lambda-form walker lambda-form env (cons 0 path))
                   :arguments (loop :for arg :in args
                                    :for n :from 1
                                    :collect (walk walker arg env (cons n path))))))

(defmethod walk-form ((walker walker) name form env path)
  (let ((macrolet-binding (lookup-macrolet-binding env name)))
    (if macrolet-binding
        (walk-macro walker form path env (macrolet-binding-lambda-list macrolet-binding))
        (multiple-value-bind (expansion expanded) (macroexpand-1 form)
          (cond (expanded
                 (if (walking-without-expanding-unknown-macros-p walker)
                     (walk-macro walker form path env (micros/backend:arglist (first form)))
                     (walk walker expansion env path)))
                (t
                 (let ((name (first form)))
                   (if (consp name)
                       (walk-lambda-call-form walker form env path)
                       (let ((binding (lookup-function-binding env name))
                             (arguments (loop :for arg :in (rest form)
                                              :for n :from 1
                                              :collect (walk walker arg env (cons n path)))))
                         (if binding
                             (make-instance 'call-local-function-form
                                            :binding binding
                                            :arguments arguments
                                            :path (cons 0 path))
                             (make-instance 'call-function-form
                                            :operator name
                                            :arguments arguments
                                            :path (cons 0 path))))))))))))

(defmethod walk-variable ((walker walker) symbol env path)
  ;; TODO
  ;; - symbol-macro
  ;; - constant
  (let ((binding (lookup-variable env symbol)))
    (typecase binding
      (lexical-variable-binding
       (make-instance 'lexical-variable :binding binding :path path))
      (special-variable-binding
       (make-instance 'special-variable :binding binding :path path))
      (null
       (make-instance 'special-variable
                      :binding (get-special-variable-binding walker symbol)
                      :path path)))))

(defun walk (walker form &optional env path)
  (cond ((null form)
         (make-instance 'constant-form :value nil :path path))
        ((keywordp form)
         (make-instance 'constant-form :value form :path path))
        ((symbolp form)
         (walk-variable walker form env path))
        ((atom form)
         (make-instance 'constant-form :value form :path path))
        (t
         (walk-form walker (first form) form env path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric visit (visitor ast))

(defclass visitor () ())

(defun visit-foreach (visitor ast-list)
  (dolist (ast ast-list)
    (visit visitor ast)))

(defmethod visit (visitor (ast unknown-form))
  nil)

(defmethod visit (visitor (ast constant-form))
  nil)

(defmethod visit (visitor (ast load-time-value-form))
  (visit visitor (ast-form ast))
  (visit visitor (ast-read-only-p ast)))

(defmethod visit (visitor (ast eval-when-form))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast lambda-list-variable-form))
  (values))

(defmethod visit (visitor (ast lambda-list-form))
  (visit-foreach visitor (ast-variables ast))
  (visit-foreach visitor (ast-initial-forms ast)))

(defmethod visit (visitor (ast lambda-form))
  (visit visitor (ast-lambda-list ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast multiple-value-call-form))
  (visit visitor (ast-function ast))
  (visit-foreach visitor (ast-arguments ast)))

(defmethod visit (visitor (ast multiple-value-prog1-form))
  (visit visitor (ast-values-form ast))
  (visit-foreach visitor (ast-forms ast)))

(defmethod visit (visitor (ast progn-form))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast if-form))
  (visit visitor (ast-test ast))
  (visit visitor (ast-then ast))
  (visit visitor (ast-else ast)))

(defmethod visit (visitor (ast catch-form))
  (visit visitor (ast-tag ast))
  (visit visitor (ast-value ast)))

(defmethod visit (visitor (ast throw-form))
  (visit visitor (ast-tag ast))
  (visit visitor (ast-value ast)))

(defmethod visit (visitor (ast let-binding-form))
  (visit visitor (ast-value ast)))

(defmethod visit (visitor (ast let-form))
  (visit-foreach visitor (ast-bindings ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast flet-binding-form))
  (visit visitor (ast-lambda-list ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast flet-form))
  (visit-foreach visitor (ast-bindings ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast setq-form))
  (visit-foreach visitor (ast-arguments ast)))

(defmethod visit (visitor (ast unwind-protect-form))
  (visit visitor (ast-proctected-form ast))
  (visit-foreach visitor (ast-cleanup-forms ast)))

(defmethod visit (visitor (ast tagbody-form))
  (visit-foreach visitor (ast-statements ast)))

(defmethod visit (visitor (ast tag))
  (values))

(defmethod visit (visitor (ast go-form))
  (visit visitor (ast-tag ast)))

(defmethod visit (visitor (ast return-from-form))
  (values))

(defmethod visit (visitor (ast block-name-form))
  (values))

(defmethod visit (visitor (ast block-form))
  (visit visitor (ast-name ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast return-from-form))
  (visit visitor (ast-name ast))
  (visit visitor (ast-value ast)))

(defmethod visit (visitor (ast the-form))
  (visit visitor (ast-form ast)))

(defmethod visit (visitor (ast call-function-form))
  (visit-foreach visitor (ast-arguments ast)))

(defmethod visit (visitor (ast call-local-function-form))
  (visit-foreach visitor (ast-arguments ast)))

(defmethod visit (visitor (ast lambda-call-form))
  (visit visitor (ast-lambda-form ast))
  (visit-foreach visitor (ast-arguments ast)))

(defmethod visit (visitor (ast special-variable))
  (values))

(defmethod visit (visitor (ast lexical-variable))
  (values))

(defmethod visit (visitor (ast local-function-form))
  (values))

(defmethod visit (visitor (ast function-form))
  (values))

(defmethod visit (visitor (ast implict-progn-form))
  (visit-foreach visitor (ast-forms ast)))

(defmethod visit (visitor (ast macrolet-form))
  (visit visitor (ast-body ast)))

(defmethod visit (visitor (ast with-single-binding-form))
  (visit-foreach visitor (ast-binding-args ast))
  (visit visitor (ast-body ast)))

;;
(define-condition exit-visitor ()
  ((value :initarg :value
          :reader exit-visitor-value)))

;; simple-visitor
(defclass simple-visitor (visitor)
  ((function :initarg :function
             :initform (error "Missing :function")
             :reader visitor-function)))

(defmethod visit ((visitor simple-visitor) ast)
  (funcall (visitor-function visitor) ast)
  (call-next-method))

;; binding-collector
(defclass binding-collector (visitor)
  ((target-binding :initarg :target-binding
                   :initform (error "Missing :target-binding")
                   :reader binding-collector-target-binding)
   (found-paths :initform '()
                :accessor binding-collector-found-paths)))

(defmethod visit :before ((visitor binding-collector) (ast <with-binding-form>))
  (when (eq (binding-collector-target-binding visitor)
            (ast-binding ast))
    (when (ast-path ast)
      (push (ast-path ast)
            (binding-collector-found-paths visitor)))))

;; path-finder
(defclass path-finder (visitor)
  ((target-path :initarg :target-path
                :initform (error "Missing :target-path")
                :reader visitor-path)))

(defmethod visit ((visitor path-finder) ast)
  (when (equal (ast-path ast) (visitor-path visitor))
    (error 'exit-visitor :value ast))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *record-test-cases* nil)
(defvar *test-cases* '())

(defun collect-highlight-paths (form path)
  (let ((result (let ((ast
                        (handler-bind ((unimplemented #'continue))
                          (walk (make-instance 'walker) form '() '()))))
                  (handler-case (visit (make-instance 'path-finder :target-path path) ast)
                    (exit-visitor (c)
                      (typecase (exit-visitor-value c)
                        (<with-binding-form>
                         (let ((visitor
                                 (make-instance 'binding-collector
                                                :target-binding (ast-binding
                                                                 (exit-visitor-value c)))))
                           (visit visitor ast)
                           (binding-collector-found-paths visitor)))))))))
    (when *record-test-cases*
      (push `((collect-highlight-paths ,form ,path) ,result)
            *test-cases*))
    result))

(defun read-from-string-with-buffer-syntax (string package-name)
  (micros::with-buffer-syntax (package-name)
    (read-from-string string)))

(micros::defslimefun highlight (form-string path package-name)
  (handler-case (values (read-from-string-with-buffer-syntax form-string package-name))
    (error (e)
      (return-from highlight (list :read-error (princ-to-string e))))
    (:no-error (form)
      (handler-case (collect-highlight-paths form path)
        (:no-error (paths)
          (list :ok paths))
        (unimplemented (c)
          (list :error (format nil "unimplemented: ~A" (unimplemented-context c))))
        (error (c)
          (list :error (format nil "ERROR: ~A" c)))))))
