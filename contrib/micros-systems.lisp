(in-package :micros)

(defun find-quicklisp-systems ()
  "If Quicklisp is available, extract all system names."
  (when (find-package '#:QUICKLISP)
    (mapcar (lambda (dist)
              (uiop:symbol-call '#:ql-dist '#:name dist))
            (uiop:symbol-call '#:quicklisp '#:system-list))))

(defun find-ocicl-systems ()
  "If the Ocicl runtime is available, extract all system names from the current directory's systems.csv."
  (when (find-package '#:OCICL-RUNTIME)
    (with-open-file (in-stream (merge-pathnames (uiop:getcwd) "systems.csv") :direction :input)
      (unwind-protect
           (loop :for line = (read-line in-stream nil)
                 :while line
                 :collect (subseq line 0 (position #\, line)))
        (close in-stream)))))

(defun find-asdf-systems ()
  "If ASDF is available, extract system names by collecting all keys from the *source-registry* hash lists."
  (when (find-package '#:ASDF)
    (loop :for system-name :being :each :hash-key :of asdf/source-registry:*source-registry*
          :collect system-name)))

(defslimefun list-systems ()
  "Returns a list of all locally available Quicklisp, Ocicl and ASDF systems."
  (asdf:ensure-source-registry)
  (sort (delete-duplicates
         (append (find-quicklisp-systems)
                 (find-ocicl-systems)
                 (find-asdf-systems))
         :test #'string=)
        #'string<))
