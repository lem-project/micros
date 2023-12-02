(in-package :micros/walker)

(defun string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun make-keyword (x)
  (intern (string x) :keyword))

;;; copy from alexandria
(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

(defmacro with-gensyms (names &body forms)
  "Binds a set of variables to gensyms and evaluates the implicit progn FORMS.

Each element within NAMES is either a symbol SYMBOL or a pair (SYMBOL
STRING-DESIGNATOR). Bare symbols are equivalent to the pair (SYMBOL SYMBOL).

Each pair (SYMBOL STRING-DESIGNATOR) specifies that the variable named by SYMBOL
should be bound to a symbol constructed using GENSYM with the string designated
by STRING-DESIGNATOR being its first argument."
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name) (string (second name)))))
                     `(,symbol (gensym ,string))))
                 names)
     ,@forms))

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
