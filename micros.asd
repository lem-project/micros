(defsystem "micros"
  :depends-on ()
  :version "0.0.0"
  :serial t
  :components ((:file "packages")
               (:module "sbcl"
                :pathname "backend"
                :components ((:file "backend")
                             (:file "source-path-parser")
                             (:file "source-file-cache")
                             #+sbcl
                             (:file "sbcl")
                             #+abcl
                             (:file "abcl")
                             #+clasp
                             (:file "clasp")
                             #+ccl
                             (:file "ccl")
                             #+ecl
                             (:file "ecl")
                             (:file "gray")
                             (:file "match")
                             (:file "rpc")))
               (:file "micros")
               (:module "contrib"
                :components ((:file "micros-util")
                             (:file "micros-repl")
                             (:file "micros-c-p-c" :depends-on ("micros-util"))
                             (:file "micros-arglists" :depends-on ("micros-c-p-c"))
                             (:file "micros-fuzzy" :depends-on ("micros-util" "micros-c-p-c"))
                             (:file "micros-fancy-inspector" :depends-on ("micros-util"))
                             ;; (:file "micros-presentations" :depends-on ("micros-repl"))
                             ;; (:file "micros-presentation-streams" :depends-on ("micros-presentations"))
                             (:file "micros-package-fu")
                             (:file "micros-hyperdoc")
                             (:file "micros-sbcl-exts" :depends-on ("micros-arglists"))
                             (:file "micros-mrepl")
                             (:file "micros-trace-dialog")
                             (:file "micros-macrostep")
                             (:file "micros-systems")
                             (:file "micros-pretty-eval")
                             (:file "micros-trace")
                             (:file "micros-test-runner")
                             ;; (:file "micros-asdf")
                             ;; (:file "micros-buffer-streams")
                             ;; (:file "clipboard")
                             ;; (:file "indentation")
                             ;; (:file "listener-hooks" :depends-on ("micros-repl"))
                             ;; (:file "snapshot")
                             ;; (:file "sprof")
                             (:module "walker"
                              :components ((:file "package")
                                           (:file "types")
                                           (:file "walker")
                                           (:file "defun-form")))))
               (:file "lsp-api")))

(defsystem "micros/tests"
  :depends-on ("rove" "micros")
  :serial t
  :pathname "contrib/walker/tests/"
  :components ((:file "tests")))
