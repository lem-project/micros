(block foo
  (return-from foo 10)
  (block foo
    (return-from foo 20)
    (return-from foo 30))
  (return-from foo 40))

(let* ((a 0)
       (b a))
  (let* ((a a)
         (b a))
    a)
  a)

(let ((a 1))
  (let ((b a))
    a
    b))

(let ((a 0)
      (b 1))
  (load-time-value a b)
  (multiple-value-call 'f a b a)
  (setq a b
        b a)
  (progn a b)
  (multiple-value-prog1 a
    b
    a)
  (unwind-protect a (the integer b) c))

(lambda (x a b c &key (y x) z &aux (foo 10))
  x
  y
  z
  foo)

(function (lambda (x a b c &key (y x) z &aux (foo 10))
            x
            y
            z
            foo))

(flet ((f ()))
  (flet ((f (&optional (x 1))
           x
           #'f
           (f x)))
    (f x)
    #'f))

(labels ((f () (f)))
  (labels ((f (&optional (x 1))
             x
             (f x)))
    #'f))

(labels ((f (x &key (y x))
           (g x))
         (g (y)
           (f y)))
  (g 10))

(defmacro with-hoge (() &body body)
  `(progn ,@body))

(let ((a 0))
  (do ((x 1 (1+ x)))
      ((= 10 x))
    a
    (let ((a 1))
      a)
    b
    c))

(let ((a 0))
  (WITH-HOGE NIL
    A
    B
    C))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((a 0))
    a))

(macrolet ((with-foo (() &body body)
             ))
  (let ((a 0))
    (with-foo ((aaa ))
      a)))

(defmethod add (x y)
  (+ x y))

(defmethod add ((x integer) (y integer))
  (+ x y))

(defmethod add :before ((x integer) (y integer))
  (print (list x y)))


((lambda (a b)
   (+ a b))
 1
 2)
