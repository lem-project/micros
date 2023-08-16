(defpackage :micros/test-runner
  (:use :cl))
(in-package :micros/test-runner)

(micros/swank-api:defslimefun run-test (name package-name)
  (uiop:symbol-call '#:rove '#:run-tests
                    (list (read-from-string (format nil "~A::~A" package-name name)))))
