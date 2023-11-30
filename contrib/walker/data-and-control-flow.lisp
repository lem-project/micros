(in-package #:micros/walker)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reader-name (symbol)
    (intern (format nil "AST-~A" symbol)))

  (defun expand-simple-walker-defclass (walker-name arguments)
    `(defclass ,walker-name (ast)
       ,(loop :for argument :in arguments
              :collect `(,argument :initarg ,(intern (string argument) :keyword)
                                   :reader ,(reader-name argument)))))

  (defun expand-simple-walker-defmethod-walk-form (walker-name operator-name arguments)
    (with-gensyms (walker name form env path)
      `(defmethod walk-form ((,walker walker) (,name (eql ',operator-name)) ,form ,env ,path)
         (make-instance ',walker-name
                        ,@(loop :for argument :in arguments
                                :for n :from 1
                                :collect (intern (string argument) :keyword)
                                :collect `(walk ,walker (elt ,form ,n) ,env (cons ,n ,path)))))))

  (defun expand-simple-walker-defmethod-visit (walker-name arguments)
    (with-gensyms (visitor ast)
      `(defmethod visit (,visitor (,ast ,walker-name))
         ,@(loop :for argument :in arguments
                 :collect `(visit ,visitor (,(reader-name argument) ,ast))))))

  (defun expand-simple-walker (walker-name operator-name arguments)
    `(progn
       ,(expand-simple-walker-defclass walker-name arguments)
       ,(expand-simple-walker-defmethod-walk-form walker-name operator-name arguments)
       ,(expand-simple-walker-defmethod-visit walker-name arguments))))

(defmacro def-simple-walker (walker-name operator-name &rest arguments)
  (expand-simple-walker walker-name operator-name arguments))

(def-simple-walker nth-value-form nth-value n form)
(def-simple-walker or-form or n form)
(def-simple-walker incf-form incf n form)
(def-simple-walker decf-form decf n form)
