(defpackage #:micros/walker/tests
  (:use #:cl #:rove))
(in-package #:micros/walker/tests)

(defun load-test-cases ()
  (uiop:read-file-forms (asdf:system-relative-pathname :micros/tests "contrib/walker/tests/test-cases.lisp")))

(deftest random
  (loop :for (act-form expected) :in (load-test-cases)
        :for n :from 0
        :do (ok (equal expected (apply (first act-form) (rest act-form)))
                (format nil "~D ~S" n act-form))))
