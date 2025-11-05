;;; Filename: ww-packages.lisp


(in-package :cl-user)


(defpackage :utilities
  (:use :cl)
  (:nicknames :ut))


(defpackage :hstack
  (:use :cl)
  (:nicknames :hs))


(defpackage :wouldwork
  (:use :cl :iterate :sb-ext)
  (:nicknames :ww)
  (:shadowing-import-from :iterate)
  (:export #:main
           #:help
           #:run-test-problems
           #:run-all
           #:list-all
           #:run
          ;#:*test-problem-names*
           #:*problem-folder-paths*
	   #:get-src-folder-path
	   #:add-problem-folder
           #:remove-problem-folder
           #:save-globals
           #:read-globals
           #:*globals-file*
           #:*keep-globals-p*
           #:toggle-globals
           #:set-globals
           #:display-globals))


(defun ww-reset ()
  "Delete problem.lisp, then reload wouldwork with default problem.
   Allows recovery if wouldwork loading fails with error in problem file."
  (format t "~%Loading wouldwork defaults...~2%")
  (let* ((root (asdf:system-source-directory :wouldwork))
         (problem-file (merge-pathnames "src/problem.lisp" root)))
    (when (probe-file problem-file) (delete-file problem-file)))
  (asdf:clear-system :wouldwork)
  (let ((*compile-verbose* nil)
        (*compile-print* nil))
    (asdf:load-system :wouldwork :force t))
  (setf *package* (find-package :ww)))