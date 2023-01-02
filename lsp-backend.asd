(defsystem "lsp-backend"
  :serial t
  :components ((:file "swank-loader")
               (:file "packages")
               (:module "sbcl"
                :pathname "swank"
                :components ((:file "backend")
                             (:file "source-path-parser")
                             (:file "source-file-cache")
                             (:file "sbcl")
                             (:file "gray")
                             (:file "match")
                             (:file "rpc")))
               (:file "swank")
               (:module "contrib"
                :components ((:file "swank-util")
                             (:file "swank-repl")
                             (:file "swank-c-p-c")
                             (:file "swank-arglists")
                             (:file "swank-fuzzy")
                             (:file "swank-fancy-inspector")
                             (:file "swank-presentations")
                             (:file "swank-presentation-streams")
                             (:file "swank-asdf")
                             (:file "swank-package-fu")
                             (:file "swank-hyperdoc")
                             (:file "swank-sbcl-exts")
                             (:file "swank-mrepl")
                             (:file "swank-trace-dialog")
                             (:file "swank-macrostep")
                             (:file "swank-quicklisp")))))

