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
  (handler-bind ((warning #'muffle-warning))
    (let ((*compile-verbose* nil)
          (*compile-print* nil))
      (asdf:load-system :wouldwork :force t)))
  (setf *package* (find-package :ww)))


(defun refresh ()
  "Reload the current problem file after editing. Works even after load errors.
   This recovery version works from CL-USER package when ww::refresh is unavailable."
  (let* ((ww-pkg (find-package :ww))
         (problem-name-sym (and ww-pkg (find-symbol "*PROBLEM-NAME*" ww-pkg)))
         (problem-name (and problem-name-sym 
                            (boundp problem-name-sym)
                            (symbol-value problem-name-sym)))
         (goal-sym (and ww-pkg (find-symbol "*GOAL*" ww-pkg)))
         (undo-sym (and ww-pkg (find-symbol "*UNDO-CHECKPOINT*" ww-pkg)))
         (refreshing-sym (and ww-pkg (find-symbol "*REFRESHING*" ww-pkg))))
    ;; Check we have a problem to refresh
    (unless (and problem-name (not (eq problem-name 'ww::unspecified)))
      (format t "~%No problem currently loaded. Use (run <problem-name>) or (stage <problem-name>) instead.~%")
      (return-from refresh nil))
    (format t "~%Refreshing problem ~A...~2%" problem-name)
    ;; Clear goal if bound
    (when (and goal-sym (boundp goal-sym))
      (setf (symbol-value goal-sym) nil))
    ;; Warn about undo checkpoint if set
    (when (and undo-sym (boundp undo-sym) (symbol-value undo-sym))
      (format t "~%Note: Refresh invalidates the current goal-chaining session.~%~
                 Use (ww-continue <new-goal>) to restart goal chaining after refresh completes.~%")
      (setf (symbol-value undo-sym) nil))
    ;; Set refreshing flag to preserve REPL-set parameters
    (when (and refreshing-sym (boundp refreshing-sym))
      (setf (symbol-value refreshing-sym) t))
    ;; Locate and copy problem file
    (let* ((root (asdf:system-source-directory :wouldwork))
           (src-dir (merge-pathnames "src/" root))
           (problem-file (merge-pathnames "problem.lisp" src-dir))
           (source-file (merge-pathnames 
                         (format nil "problem-~A.lisp" (string-downcase problem-name))
                         src-dir)))
      ;; Check source exists
      (unless (probe-file source-file)
        (format t "~%Source file not found: ~A~%" source-file)
        (when (and refreshing-sym (boundp refreshing-sym))
          (setf (symbol-value refreshing-sym) nil))
        (return-from refresh nil))
      ;; Delete current problem.lisp and copy fresh source
      (when (probe-file problem-file)
        (delete-file problem-file))
      (uiop:copy-file source-file problem-file)
      ;; Clear and reload with warning muffling
      (asdf:clear-system :wouldwork)
      (unwind-protect
          (handler-bind ((warning #'muffle-warning))
            (let ((*compile-verbose* nil)
                  (*compile-print* nil))
              (asdf:load-system :wouldwork :force t)))
        ;; Cleanup: reset refreshing flag (look up fresh after reload)
        (let* ((new-ww-pkg (find-package :ww))
               (new-refreshing-sym (and new-ww-pkg 
                                        (find-symbol "*REFRESHING*" new-ww-pkg))))
          (when (and new-refreshing-sym (boundp new-refreshing-sym))
            (setf (symbol-value new-refreshing-sym) nil)))))
    (setf *package* (find-package :ww))))