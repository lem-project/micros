(in-package :micros)

(defslimefun list-systems ()
  "Returns the Quicklisp and ASDF systems list."
  (unless (member :quicklisp *features*)
    (error "Could not find Quicklisp already loaded."))
  (asdf:ensure-source-registry)
  (let ((asdf-systems
          (sort (loop :for system-name :being :each :hash-key :of asdf/source-registry:*source-registry*
                      :collect system-name)
                #'string<))
        (quicklisp-systems
          (mapcar (lambda (dist)
                    (uiop:symbol-call '#:ql-dist '#:name dist))
                  (uiop:symbol-call '#:quicklisp '#:system-list))))
    (append asdf-systems
            quicklisp-systems)))
