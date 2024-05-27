;;; trivial-system-loader.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2024  Anthony Green and Michał 'phoe' Herda
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;

(in-package :micros/lsp-api)

(defun load-systems (systems &key (verbose nil) (silent t))
  "Load system SYSTEMS, potentially downloading them from an external
 repository.  SYSTEMS may be a single system or a list of
 systems. Loader behavior is modified by VERBOSE and SILENT."
  (unless (listp systems)
    (setf systems (list systems)))
  (flet ((try-load-system (system)
           (or
            (when (find-package '#:OCICL-RUNTIME)
              (progv (list (find-symbol "*DOWNLOAD*" '#:OCICL-RUNTIME)
                           (find-symbol "*VERBOSE*" '#:OCICL-RUNTIME))
                  (list t (or verbose (not silent)))
                (funcall (find-symbol "LOAD-SYSTEM" '#:asdf) system)))
            (when (find-package '#:QUICKLISP)
              (funcall (find-symbol "QUICKLOAD" '#:QUICKLISP)
                       system :verbose verbose :silent silent))
            (when (find-package '#:ASDF)
              (funcall (find-symbol "LOAD-SYSTEM" '#:ASDF) system))
            (error "Unable to find any system-loading mechanism."))))
    (mapcar #'try-load-system systems)))
