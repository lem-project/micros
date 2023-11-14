(defpackage #:micros/walker/tests
  (:use #:cl #:rove))
(in-package #:micros/walker/tests)

(defun load-test-cases ()
  (uiop:read-file-forms (asdf:system-relative-pathname :micros/tests "contrib/walker/tests/test-cases.lisp")))

;; copy from alexandria
(defun set-equal (list1 list2 &key (test #'eql) (key nil keyp))
  "Returns true if every element of LIST1 matches some element of LIST2 and
every element of LIST2 matches some element of LIST1. Otherwise returns false."
  (let ((keylist1 (if keyp (mapcar key list1) list1))
        (keylist2 (if keyp (mapcar key list2) list2)))
    (and (dolist (elt keylist1 t)
           (or (member elt keylist2 :test test)
               (return nil)))
         (dolist (elt keylist2 t)
           (or (member elt keylist1 :test test)
               (return nil))))))

(deftest random
  (loop :for (act-form expected) :in (load-test-cases)
        :for n :from 0
        :do (ok (set-equal expected
                           (apply (first act-form) (rest act-form))
                           :test #'equal)
                (format nil "~D ~S" n act-form))))
