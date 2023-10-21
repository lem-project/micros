(defpackage #:micros/walker
  (:use #:cl)
  (:export #:ast-equal
           #:walker
           #:walk))
(in-package #:micros/walker)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun proper-list-p (x)
    (and (listp x)
         (null (cdr (last x))))))

(deftype proper-list (&optional (element-type '*))
  (declare (ignore element-type))
  '(and list (satisfies proper-list-p)))

(deftype variable-symbol ()
  '(and symbol (not keyword)))

;; copy from alexandria:parse-body
(defun parse-body (body &key documentation whole)
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
      (setf current (car body))
      (when (and documentation (stringp current) (cdr body))
        (if doc
            (error "Too many documentation strings in ~S." (or whole body))
            (setf doc (pop body)))
        (go :declarations))
      (when (and (listp current) (eql (first current) 'declare))
        (push (pop body) decls)
        (go :declarations)))
    (values body (nreverse decls) doc)))

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
                 :collect (intern (string name) :keyword) :and
                 :collect (slot-value object name))
           stream)))

;; env
(defclass binding (<printable>)
  ((name :initarg :name
         :reader binding-name)
   (value :initarg :value
          :reader binding-value
          :writer (setf binding-value))))

(defclass lexical-variable-binding (binding) ())
(defclass lexical-function-binding (binding) ())
(defclass block-binding (binding) ())
(defclass macrolet-binding (binding)
  ((lambda-list :initarg :lambda-list
                :reader macrolet-binding-lambda-list)))

(defun extend-env (env binding)
  (cons binding env))

(defun extend-env* (env bindings)
  (append bindings env))

(defun lookup-binding (env name type)
  (loop :for binding :in env
        :when (and (typep binding type)
                   (eq name (binding-name binding)))
        :return binding))

(defun lookup-lexical-binding (env name)
  (lookup-binding env name 'lexical-variable-binding))

(defun lookup-function-binding (env name)
  (lookup-binding env name 'lexical-function-binding))

(defun lookup-macrolet-binding (env name)
  (lookup-binding env name 'macrolet-binding))

(defun lookup-block-binding (env name)
  (lookup-binding env name 'block-binding))

;; ast
(defclass ast (<printable>)
  ((path :initarg :path
         :initform (error "Missing :path")
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

(defclass lambda-list-variable-form (ast)
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
  ((lambda-list :initarg :lambda-list :reader ast-lambda-list)
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

(defclass let-binding-form (ast)
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

(defclass let*-form (ast)
  ((bindings :initarg :bindings
             :type (proper-list let-binding-form)
             :reader ast-bindings)
   (body :initarg :body
         :reader ast-body)))

(defclass flet-binding-form (ast)
  ((binding :initarg :binding :reader ast-binding)
   (lambda-list :initarg :lambda-list :reader ast-lambda-list)
   (body :initarg :body :reader ast-body)))

(defclass flet-form (ast)
  ((bindings :initarg :bindings
             :type (proper-list flet-binding-form)
             :reader ast-bindings)
   (body :initarg :body
         :reader ast-body)))

(defclass labels-form (ast)
  ((bindings :initarg :bindings
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

(defclass block-name-form (ast)
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

(defclass call-local-function-form (ast)
  ((binding :initarg :binding
            :reader ast-binding)
   (arguments :initarg :arguments
              :reader ast-arguments)))

(defclass dynamic-variable (ast)
  ((symbol :initarg :symbol
           :reader ast-symbol)))

(defclass lexical-variable (ast)
  ((binding :initarg :binding
            :type lexical-variable-binding
            :reader ast-binding)))

(defclass local-function-form (ast)
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

;; walker
(defclass walker () ())

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
      (make-instance 'labels-form
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

(defmethod walk-lambda-list ((walker walker) lambda-list env path)
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
            :for ll :in lambda-list
            :do (case ll
                  ((&aux &key &rest &body &optional)
                   (setf state ll))
                  (otherwise
                   (ecase state
                     ((&rest &body)
                      (assert-type ll 'variable-symbol)
                      (add (make-instance 'lexical-variable-binding :name ll)
                           (cons n path)))
                     ((&key &optional &aux)
                      (let* ((var-value (uiop:ensure-list ll))
                             (var (first var-value))
                             (value (second var-value)))
                        (assert-type var 'variable-symbol)
                        (let ((initial-value
                                (when value
                                  (walk walker value env (list* 1 n path)))))
                          (when initial-value
                            (push initial-value initial-forms))
                          (add (make-instance 'lexical-variable-binding
                                              :name var
                                              :value initial-value)
                               (if (consp ll)
                                   (list* 0 n path)
                                   (cons n path))))))
                     ((nil)
                      (add (make-instance 'lexical-variable-binding :name ll)
                           (cons n path))))))))
    (values (make-instance 'lambda-list-form
                           :variables (nreverse walked-lambda-list)
                           :initial-forms initial-forms
                           :path path)
            env)))

(defmethod walk-lambda-form ((walker walker) form env path)
  (assert-type (first form) '(member lambda #+sbcl sb-int:named-lambda))
  (with-walker-bindings (lambda-list &body body) (rest form)
    (multiple-value-bind (lambda-list env)
        (walk-lambda-list walker lambda-list env (cons 1 path))
      ;; TODO: declare
      (make-instance 'lambda-form
                     :lambda-list lambda-list
                     :body (walk-forms walker body env path 2)
                     :path (cons 0 path)))))

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
  (with-walker-bindings (var value) binding-form
    (assert-type var 'variable-symbol)
    (let ((value (walk walker value env (cons 1 path))))
      (make-instance 'let-binding-form
                     :path (cons 0 path)
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
        (make-instance 'let*-form
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
  (error "unimplemented"))

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
  (error "unimplemented"))

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
  (error "unimplemented"))

(defmethod walk-form ((walker walker) (name (eql 'tagbody)) form env path)
  (error "unimplemented"))

(defmethod walk-form ((walker walker) (name (eql 'go)) form env path)
  (error "unimplemented"))

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

(defun walk-macro-1 (walker form path env lambda-list)
  (let* ((body-pos (position '&body lambda-list)))
    (if body-pos
        (make-instance 'implict-progn-form
                       :path path
                       :forms (walk-forms walker
                                          (nthcdr body-pos (rest form))
                                          env
                                          path
                                          (1+ body-pos)))
        (error "unimplemented"))))

(defmethod walk-macro ((walker walker) form env path expansion)
  (walk-macro-1 walker
                form
                path
                env
                (micros/backend:arglist (first form))))

(defmethod walk-form ((walker walker) name form env path)
  (let ((macrolet-binding (lookup-macrolet-binding env name)))
    (if macrolet-binding
        (walk-macro-1 walker
                      form
                      path
                      env
                      (macrolet-binding-lambda-list macrolet-binding))
        (multiple-value-bind (expansion expanded) (macroexpand-1 form)
          (if expanded
              (walk-macro walker form env path expansion)
              (let ((name (first form)))
                (if (consp name)
                    (error "unimplemented") ; TODO: lambda form
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
                                         :path (cons 0 path)))))))))))

(defmethod walk-variable ((walker walker) symbol env path)
  (declare (ignore walker))
  ;; TODO
  ;; - symbol-macro
  ;; - constant
  (let ((binding (lookup-lexical-binding env symbol)))
    (if binding
        (make-instance 'lexical-variable :binding binding :path path)
        (make-instance 'dynamic-variable :symbol symbol :path path))))

(defun walk (walker form env path)
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

(defmethod visit (visitor (ast let*-form))
  (visit-foreach visitor (ast-bindings ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast flet-binding-form))
  (visit visitor (ast-lambda-list ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast flet-form))
  (visit-foreach visitor (ast-bindings ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast labels-form))
  (visit-foreach visitor (ast-bindings ast))
  (visit-foreach visitor (ast-body ast)))

(defmethod visit (visitor (ast setq-form))
  (visit-foreach visitor (ast-arguments ast)))

(defmethod visit (visitor (ast unwind-protect-form))
  (visit visitor (ast-proctected-form ast))
  (visit-foreach visitor (ast-cleanup-forms ast)))

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

(defmethod visit (visitor (ast dynamic-variable))
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
                   :reader visitor-target-binding)
   (found-paths :initform '()
                :accessor visitor-found-paths)))

(defun visit-binding-collector (visitor ast)
  (when (eq (visitor-target-binding visitor)
            (ast-binding ast))
    (push (ast-path ast)
          (visitor-found-paths visitor))))

(defmethod visit ((visitor binding-collector) (ast let-binding-form))
  (visit-binding-collector visitor ast)
  (call-next-method))

(defmethod visit ((visitor binding-collector) (ast flet-binding-form))
  (visit-binding-collector visitor ast)
  (call-next-method))

(defmethod visit ((visitor binding-collector) (ast lambda-list-variable-form))
  (visit-binding-collector visitor ast)
  (call-next-method))

(defmethod visit ((visitor binding-collector) (ast lexical-variable))
  (visit-binding-collector visitor ast)
  (call-next-method))

(defmethod visit ((visitor binding-collector) (ast local-function-form))
  (visit-binding-collector visitor ast)
  (call-next-method))

(defmethod visit ((visitor binding-collector) (ast block-name-form))
  (visit-binding-collector visitor ast)
  (call-next-method))

(defmethod visit ((visitor binding-collector) (ast call-local-function-form))
  (visit-binding-collector visitor ast)
  (call-next-method))

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
(defun collect-references (form path)
  (let ((ast (walk (make-instance 'walker) form '() '())))
    (handler-case (visit (make-instance 'path-finder :target-path path) ast)
      (exit-visitor (c)
        (typecase (exit-visitor-value c)
          ((or block-name-form
               let-binding-form
               lexical-variable
               lambda-list-variable-form
               flet-binding-form
               local-function-form
               call-local-function-form)
           (let ((visitor (make-instance 'binding-collector
                                         :target-binding (ast-binding (exit-visitor-value c)))))
             (visit visitor ast)
             (visitor-found-paths visitor))))))))

(defun test ()
  (labels ((test-1 (input-form target-path expected-found-paths)
             (let ((actual-found-paths (collect-references input-form target-path)))
               (assert (equal expected-found-paths
                              actual-found-paths)))))
    (test-1 '(block fooo (return-from fooo 1))
            '(1 2)
            '((1 2) (1)))
    (test-1 '(block fooo (return-from fooo 1))
            '(1)
            '((1 2) (1)))
    (test-1 '(block fooo
              (return-from fooo 1)
              (return-from fooo 2))
            '(1)
            '((1 3) (1 2) (1)))
    (test-1 '(block fooo 1 2 (return-from fooo 1))
            '(1 4)
            '((1 4) (1)))))

(defun read-from-string-with-buffer-syntax (string package-name)
  (micros::with-buffer-syntax (package-name)
    (read-from-string string)))

(micros::defslimefun highlight (form-string path package-name)
  (let ((form (read-from-string-with-buffer-syntax form-string package-name)))
    (collect-references form path)))
