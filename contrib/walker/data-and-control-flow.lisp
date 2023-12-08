(in-package :micros/walker)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct walker-lambda-list-spec
    required-arguments
    optional-arguments
    optional-position
    rest-variable
    rest-pos)

  (defun all-arguments (spec)
    (append (walker-lambda-list-spec-required-arguments spec)
            (when (walker-lambda-list-spec-rest-variable spec)
              (list (walker-lambda-list-spec-rest-variable spec)))
            (walker-lambda-list-spec-optional-arguments spec)))

  (defun parse-walker-lambda-list (lambda-list)
    (let ((rest-pos (or (position '&rest lambda-list)
                        (position '&body lambda-list)))
          (optional-pos (position '&optional lambda-list)))
      (make-walker-lambda-list-spec
       :required-arguments (subseq lambda-list 0 (or optional-pos rest-pos))
       :optional-arguments (when optional-pos (subseq lambda-list (1+ optional-pos)))
       :optional-position optional-pos
       :rest-variable (when rest-pos (elt lambda-list (1+ rest-pos)))
       :rest-pos rest-pos)))

  (defun reader-name (symbol)
    (intern (format nil "AST-~A" symbol)))

  (defun expand-simple-walker-defclass (walker-name spec)
    `(defclass ,walker-name (ast)
       ,(loop :for argument
              :in (all-arguments spec)
              :collect `(,argument :initarg ,(make-keyword argument)
                                   :reader ,(reader-name argument)))))

  (defun expand-simple-walker-defmethod-walk-form
      (walker-name operator-name spec)
    (with-gensyms (walker name form env path arg)
      `(defmethod walk-form ((,walker walker)
                             (,name (eql ',operator-name))
                             ,form ,env ,path)
         (make-instance
          ',walker-name
          ,@(loop :for argument :in (walker-lambda-list-spec-required-arguments spec)
                  :for n :from 1
                  :collect (make-keyword argument)
                  :collect `(walk ,walker
                                  (elt ,form ,n)
                                  ,env
                                  (cons ,n ,path)))
          ,@(when (walker-lambda-list-spec-optional-position spec)
              (loop :for argument :in (walker-lambda-list-spec-optional-arguments spec)
                    :for n :from (1+ (walker-lambda-list-spec-optional-position spec))
                    :collect (make-keyword argument)
                    :collect `(let ((,arg (nth ,n ,form)))
                                (when ,arg
                                  (walk ,walker
                                        ,arg
                                        ,env
                                        (cons ,n ,path))))))
          ,@(when (walker-lambda-list-spec-rest-variable spec)
              (with-gensyms (var n)
                `(,(make-keyword (walker-lambda-list-spec-rest-variable spec))
                  (loop :for ,var :in (nthcdr ,(walker-lambda-list-spec-rest-pos spec) ,form)
                        :for ,n :from ,(walker-lambda-list-spec-rest-pos spec)
                        :collect (walk ,walker ,var ,env (cons ,n ,path))))))))))

  (defun expand-simple-walker-defmethod-visit (walker-name spec)
    (with-gensyms (visitor ast)
      `(defmethod visit (,visitor (,ast ,walker-name))
         ,@(loop :for argument :in (walker-lambda-list-spec-required-arguments spec)
                 :collect `(visit ,visitor (,(reader-name argument) ,ast)))
         ,@(loop :for argument :in (walker-lambda-list-spec-optional-arguments spec)
                 :collect `(when (,(reader-name argument) ,ast)
                             (visit ,visitor (,(reader-name argument) ,ast))))
         ,(when (walker-lambda-list-spec-rest-variable spec)
            `(visit-foreach ,visitor
                            (,(reader-name (walker-lambda-list-spec-rest-variable spec)) ,ast))))))

  (defun expand-simple-walker (walker-name operator-name lambda-list)
    (let ((spec (parse-walker-lambda-list lambda-list)))
      `(progn
         ,(expand-simple-walker-defclass
           walker-name
           spec)
         ,(expand-simple-walker-defmethod-walk-form
           walker-name
           operator-name
           spec)
         ,(expand-simple-walker-defmethod-visit
           walker-name
           spec)))))

(defmacro def-simple-walker (walker-name operator-name lambda-list)
  (expand-simple-walker walker-name operator-name lambda-list))

(def-simple-walker nth-value-form nth-value (n form))
(def-simple-walker incf-form incf (n form))
(def-simple-walker decf-form decf (n form))
(def-simple-walker or-form or (&rest forms))
(def-simple-walker and-form and (&rest forms))
(def-simple-walker when-form when (test &body forms))
(def-simple-walker unless-form unless (test &body forms))
(def-simple-walker return-form return (&optional value))
(def-simple-walker in-package-form in-package (string-designator))

;; check-type
(defclass check-type-form (ast)
  ((place :initarg :place :reader ast-place)
   (type :initarg :type :reader ast-type)
   (type-string :initarg :type-string :reader ast-type-string)))

(defmethod walk-form ((walker walker) (name (eql 'check-type)) form env path)
  (with-walker-bindings (place type &optional type-string) (rest form)
    (let ((place (walk walker place env (cons 1 path)))
          (type-string (walk walker type-string env (cons 3 path))))
      (make-instance 'check-type-form
                     :place place
                     :type type
                     :type-string type-string))))

(defmethod visit (visitor (ast check-type-form))
  (visit visitor (ast-place ast))
  (visit visitor (ast-type-string ast)))

;; setf
(defclass setf-form (ast)
  ((forms :initarg :forms
          :reader ast-forms)))

(defclass setf-symbol-form (ast)
  ((var :initarg :var
        :type binding
        :reader ast-var)
   (value :initarg :value
          :reader ast-value)))

(defclass setf-complex-form (ast)
  ((operator :initarg :operator
             :type variable-symbol
             :reader ast-operator)
   (arguments :initarg :arguments
              :reader ast-arguments)
   (value :initarg :value
          :reader ast-value)))

(defmethod walk-form ((walker walker) (name (eql 'setf)) form env path)
  (make-instance
   'setf-form
   :path path
   :forms (loop :for (place value) :on (rest form) :by #'cddr
                :for n :from 1 :by 2
                :for walked-value := (walk walker
                                           value
                                           env
                                           (cons (1+ n) path))
                :collect (if (symbolp place)
                             (make-instance 'setf-symbol-form
                                            :var (walk-variable walker place env (cons n path))
                                            :value walked-value)
                             (make-instance 'setf-complex-form
                                            :operator (first place)
                                            :arguments (loop :for m :from 1
                                                             :for arg :in (rest place)
                                                             :collect (walk walker
                                                                            arg
                                                                            env
                                                                            (list* m n path)))
                                            :value walked-value)))))

(defmethod visit (visitor (ast setf-form))
  (visit-foreach visitor (ast-forms ast)))

(defmethod visit (visitor (ast setf-symbol-form))
  (visit visitor (ast-var ast))
  (visit visitor (ast-value ast)))

(defmethod visit (visitor (ast setf-complex-form))
  (visit-foreach visitor (ast-arguments ast))
  (visit visitor (ast-value ast)))

;; cond
(defclass cond-form (ast)
  ((clauses :initarg :clauses :reader ast-clauses)))

(defclass cond-clause (ast)
  ((test :initarg :test :reader ast-test)
   (then :initarg :then :reader ast-then)))

(defmethod walk-form ((walker walker) (name (eql 'cond)) form env path)
  (make-instance
   'cond-form
   :clauses (loop :for clause :in (rest form)
                  :for n :from 1
                  :collect (with-walker-bindings (test &rest then) clause
                             (make-instance
                              'cond-clause
                              :test (walk walker test env (list* 0 n path))
                              :then (walk-forms walker then env (cons n path) 1))))))

(defmethod visit (visitor (ast cond-form))
  (visit-foreach visitor (ast-clauses ast)))

(defmethod visit (visitor (ast cond-clause))
  (visit visitor (ast-test ast))
  (visit-foreach visitor (ast-then ast)))
