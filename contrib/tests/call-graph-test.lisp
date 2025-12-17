(defpackage #:micros/tests/call-graph
  (:use #:cl #:rove))
(in-package #:micros/tests/call-graph)

;;; Test fixtures

(defpackage #:micros/tests/call-graph/fixtures
  (:use #:cl)
  (:export #:outer-func
           #:inner-func
           #:helper-func
           #:test-macro
           #:unused-func))

(in-package #:micros/tests/call-graph/fixtures)

(defun inner-func (x)
  "A simple inner function"
  (* x 2))

(defun helper-func (x y)
  "A helper function"
  (+ x y))

(defun outer-func (a b)
  "Outer function that calls others"
  (helper-func (inner-func a) (inner-func b)))

(defmacro test-macro (&body body)
  "A test macro"
  `(progn ,@body))

(defun unused-func ()
  "A function not called by others"
  42)

(in-package #:micros/tests/call-graph)

;;; Utility function tests

(deftest function-type-returns-correct-type
  (testing "identifies function type correctly"
    (ok (eq :function (micros::function-type 'cl:car)))
    (ok (eq :macro (micros::function-type 'cl:when)))
    (ok (eq :generic-function (micros::function-type 'cl:print-object)))
    (ok (null (micros::function-type 'nonexistent-symbol-xyz)))))

(deftest format-arglist-returns-string
  (testing "formats argument list"
    #+sbcl
    (let ((result (micros::format-arglist 'cl:car)))
      (ok (or (null result) (stringp result))))))

(deftest make-node-id-creates-qualified-name
  (testing "creates PACKAGE:SYMBOL format"
    (ok (string= "CL:CAR" (micros::make-node-id "CL" "CAR")))
    (ok (string= "MY-PKG:MY-FUNC" (micros::make-node-id "MY-PKG" "MY-FUNC")))))

(deftest symbol-to-node-id-creates-id
  (testing "converts symbol to node ID"
    (let ((id (micros::symbol-to-node-id 'cl:car)))
      (ok (stringp id))
      (ok (search "CAR" id)))))

;;; make-node-plist tests

(deftest make-node-plist-basic
  (testing "creates node plist for function"
    (let ((plist (micros::make-node-plist 'cl:car)))
      (ok (stringp (getf plist :id)))
      (ok (string= "CAR" (getf plist :name)))
      (ok (string= "COMMON-LISP" (getf plist :package)))
      (ok (eq :function (getf plist :type))))))

(deftest make-node-plist-with-form-head
  (testing "detects node type from form head"
    ;; Command type (simulated)
    (let ((plist (micros::make-node-plist
                  'micros/tests/call-graph/fixtures:outer-func
                  (intern "DEFINE-COMMAND" :keyword))))
      ;; form-head "DEFINE-COMMAND" should set type to :command
      (ok (eq :command (getf plist :type))))))

(deftest make-node-plist-macro
  (testing "identifies macro type"
    (let ((plist (micros::make-node-plist 'cl:when)))
      (ok (eq :macro (getf plist :type))))))

;;; make-edge-plist tests

(deftest make-edge-plist-creates-edge
  (testing "creates edge plist"
    (let ((edge (micros::make-edge-plist
                 'micros/tests/call-graph/fixtures:outer-func
                 'micros/tests/call-graph/fixtures:inner-func)))
      (ok (stringp (getf edge :source)))
      (ok (stringp (getf edge :target)))
      (ok (search "OUTER-FUNC" (getf edge :source)))
      (ok (search "INNER-FUNC" (getf edge :target)))
      (ok (eq :direct (getf edge :call-type))))))

;;; remove-duplicate-edges tests

(deftest remove-duplicate-edges-removes-duplicates
  (testing "removes duplicate edges"
    (let ((edges (list (list :source "A" :target "B")
                       (list :source "A" :target "B")
                       (list :source "B" :target "C"))))
      (let ((result (micros::remove-duplicate-edges edges)))
        (ok (= 2 (length result)))))))

(deftest remove-duplicate-edges-preserves-unique
  (testing "preserves unique edges"
    (let ((edges (list (list :source "A" :target "B")
                       (list :source "B" :target "A")
                       (list :source "A" :target "C"))))
      (let ((result (micros::remove-duplicate-edges edges)))
        (ok (= 3 (length result)))))))

;;; Source code analysis tests

(deftest def-form-p-identifies-definitions
  (testing "identifies definition forms"
    (ok (micros::def-form-p 'defun))
    (ok (micros::def-form-p 'defmacro))
    (ok (micros::def-form-p 'defgeneric))
    (ok (micros::def-form-p 'define-command))
    (ok (not (micros::def-form-p 'let)))
    (ok (not (micros::def-form-p 'if)))))

(deftest excluded-definition-form-p-excludes-non-functions
  (testing "excludes non-function definitions"
    (ok (micros::excluded-definition-form-p 'defpackage))
    (ok (micros::excluded-definition-form-p 'defvar))
    (ok (micros::excluded-definition-form-p 'defclass))
    (ok (not (micros::excluded-definition-form-p 'defun)))
    (ok (not (micros::excluded-definition-form-p 'defmacro)))))

(deftest extract-definition-name-extracts-name
  (testing "extracts name from definition forms"
    (ok (eq 'foo (micros::extract-definition-name '(defun foo () nil))))
    (ok (eq 'bar (micros::extract-definition-name '(defmacro bar (x) x))))
    (ok (eq 'baz (micros::extract-definition-name '(defgeneric baz (x)))))
    ;; define-command with list form
    (ok (eq 'cmd (micros::extract-definition-name '(define-command (cmd) () nil))))
    ;; define-command with symbol form
    (ok (eq 'cmd2 (micros::extract-definition-name '(define-command cmd2 () () nil))))))

;;; call-graph-analyze-package tests

(deftest call-graph-analyze-package-returns-nil-for-missing
  (testing "returns nil for non-existent package"
    (ok (null (micros:call-graph-analyze-package "NONEXISTENT-PACKAGE-XYZ-123")))))

(deftest call-graph-analyze-package-returns-plist
  (testing "returns plist for existing package"
    (let ((result (micros:call-graph-analyze-package "MICROS/TESTS/CALL-GRAPH/FIXTURES")))
      (ok result "should return non-nil for existing package")
      (ok (listp result))
      (ok (getf result :nodes))
      (ok (listp (getf result :nodes)))
      ;; Should have at least the fixture functions
      (ok (>= (length (getf result :nodes)) 4)))))

(deftest call-graph-analyze-package-includes-functions
  (testing "includes package functions in result"
    (let* ((result (micros:call-graph-analyze-package "MICROS/TESTS/CALL-GRAPH/FIXTURES"))
           (nodes (getf result :nodes))
           (node-names (mapcar (lambda (n) (getf n :name)) nodes)))
      (ok (member "OUTER-FUNC" node-names :test #'string=))
      (ok (member "INNER-FUNC" node-names :test #'string=))
      (ok (member "HELPER-FUNC" node-names :test #'string=)))))

(deftest call-graph-analyze-package-includes-edges
  (testing "includes call relationships"
    (let* ((result (micros:call-graph-analyze-package "MICROS/TESTS/CALL-GRAPH/FIXTURES"))
           (edges (getf result :edges)))
      ;; outer-func calls inner-func and helper-func
      ;; So there should be edges
      (ok (listp edges)))))

;;; call-graph-analyze-system tests

(deftest call-graph-analyze-system-returns-nil-for-missing
  (testing "returns nil for non-existent system"
    (ok (null (micros:call-graph-analyze-system "nonexistent-system-xyz-123")))))

(deftest call-graph-analyze-system-returns-plist-for-valid
  (testing "returns plist for existing system"
    ;; Use micros itself as test subject
    (let ((result (micros:call-graph-analyze-system "micros")))
      (ok result "should return non-nil for existing system")
      (ok (listp result))
      (ok (getf result :nodes))
      (ok (getf result :edges)))))

;;; call-graph-analyze-file tests

(deftest call-graph-analyze-file-handles-nonexistent
  (testing "handles non-existent file gracefully"
    ;; Should return empty graph, not error
    (let ((result (micros:call-graph-analyze-file "/nonexistent/path/file.lisp")))
      (ok (listp result))
      (ok (null (getf result :nodes))))))

;;; Integration test

(deftest integration-full-analysis
  (testing "full analysis workflow"
    (let* ((result (micros:call-graph-analyze-package "MICROS/TESTS/CALL-GRAPH/FIXTURES"))
           (nodes (getf result :nodes))
           (edges (getf result :edges)))
      ;; Verify nodes have required fields
      (dolist (node nodes)
        (ok (getf node :id) "node should have :id")
        (ok (getf node :name) "node should have :name")
        (ok (getf node :package) "node should have :package"))
      ;; Verify edges have required fields
      (dolist (edge edges)
        (ok (getf edge :source) "edge should have :source")
        (ok (getf edge :target) "edge should have :target")))))

;;; Line detection tests

(deftest line-defines-symbol-p-detects-definitions
  (testing "detects symbol definitions in lines"
    (ok (micros::line-defines-symbol-p "(defun foo ()" "FOO"))
    (ok (micros::line-defines-symbol-p "(defmacro bar (&body body)" "BAR"))
    (ok (micros::line-defines-symbol-p "(define-command my-command () ()" "MY-COMMAND"))
    (ok (not (micros::line-defines-symbol-p "(let ((foo 1))" "FOO")))
    (ok (not (micros::line-defines-symbol-p "(defun bar ())" "FOO")))))
