;;; Filename: ww-packages.lisp


(in-package :cl-user)


#+ignore (defun ww-reset ()
  "Deletes vals.lisp & problem.lisp to start over fresh in case of wouldwork loading error."
  (let* ((root (asdf:system-source-directory :wouldwork))
         (src-dir (merge-pathnames "src/" root))
         (problem-file (merge-pathnames "problem.lisp" src-dir))
         (vals-file (merge-pathnames "vals.lisp" root)))
    (uiop:delete-file-if-exists problem-file)
    (uiop:delete-file-if-exists vals-file)
    (format t "Resetting problem.lisp & vals.lisp...")
    (quit))
  t)


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

