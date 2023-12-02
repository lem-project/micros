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

(let ((a 0))
  a
  ((lambda (a)
     (declare (special a))
     a))
  ((lambda (a)
     (declare (special a))
     a))
  a)

(with-open-file (in filename)
  (read-line in))

(let ((x 0))
  (loop (f x)))

(loop :with x := 0
      :with y := x
      :with z := (f x y))

(loop :with x := 0
      :return :it
      :return (f x))

(loop :with (x . y) := (f)
      :with a := x
      :with b := y)

(loop :with ((x y) . z) := (f)
      :with a := (+ x y z))

(loop :for x :in '(1 2 3) :do (print x))
(loop :with foo
      :for x :in '(1 2 3) :do (print x))
(loop :with foo := nil
      :for x :in '(1 2 3) :do (print x))
(loop :with fn := #'cddr :and a
      :for x :in (list a) :by fn :do (print x))

(loop :for x :on '(1 2 3) :do (print x))
(loop :with foo
      :for x :on '(1 2 3) :do (print x))
(loop :with foo := nil
      :for x :on '(1 2 3) :do (print x))
(loop :with fn := #'cddr :and a
      :for x :on (list a) :by fn :do (print x))
(loop :for x := 1
      :do (f x))
(loop :for x := 1 :then (f x)
      :do (f x))
(loop :for (x . y) := (f)
      :do (f x y))

(loop for k being each hash-key in (plist-hash-table '((:a) 1 (:b) 2))
      do (print k))

(let ((v 0))
  (loop for k being each hash-key in (plist-hash-table '((:a) 1 (:b) 2))
        using (hash-value v)
        do (print (cons k v)))
  v)

(loop for v being the hash-value in *ht*
      do (print v))
(loop for v being each hash-values of *ht* using (hash-key k)
      do (format t "~a=>~a~%" k v))

(loop :for name :being :each :external-symbol
      :do (print name))

(let ((package-name (f)))
  (loop :for name :being :each :external-symbol :in package-name
        :do (print name)))

(loop :for x := 1 :then (+ y 1)
      :and y := x :then (+ x 1))

(loop :for x := 1 :then (+ y 1)
      :and y := 2 :then (+ x 1)
      :do (cons x y))

(let ((start (f))
      (end (g))
      (step (h)))
  (loop :for x :from start :to end
        :do (print x))

  (loop :for x :from start
        :do (print x))

  (loop :for x :from start :to end
        :do (print x))

  (loop :for x :from start :to end
        :do (print x))

  (loop :for x :from start :to end :by step
        :do (print x))

  (loop :for x :from start :downto end
        :do (print x))

  (loop :for x :from start :above end
        :do (print x))

  (loop :for x :downfrom start :to end
        :do (print x))

  (loop :for x :from start :downto end
        :do (print x)))

(loop :for x :from 0 :to 10
      :collect x)

(loop :for x :from 0 :to 10
      :collect (f x))

(loop :for x :from 0 :to 10
      :when (f x)
      :count :it)

(loop :for x :across "abc123"
      :when (digit-char-p x)
      :collect :it :and :collect :it)

(let ((x 1))
  (loop :for x :from x :to 10
        :collect x))

(loop :for x :from 1 :to 10
      :initially (print foo)
      :collect x :into foo
      :finally (print foo))

(loop :for x :from 1 :to 10
      :initially (print y)
      :collect (* x 2) :into y
      :finally (f x y))

(loop :for x :from 1 :to 10
      :if (f x)
      :do (g x))

(loop :for n :in list
      :do (f n)
          (g n))

(loop :for x :from 1 :to 10
      :initially (print foo)
      :collect x :into foo
      :finally (print foo))

(let ((a 0)
      b)
  a
  b)

;; TOOD
(let (storage)
  (flet (((setf storage) (value)
           (setf storage value)))
    (setf (storage) 100))
  storage)

(let ((x 0))
  (setf (car x) 100))
