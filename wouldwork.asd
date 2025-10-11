;;; Filename: wouldwork.asd

;;; ASDF instructions for loading wouldwork


(in-package :asdf-user)


(defclass always-compile-file (asdf:cl-source-file)
  ()
  (:documentation "A source file that is always compiled, regardless of timestamps."))


(defmethod asdf:operation-done-p ((o asdf:compile-op) (c always-compile-file))
  "Always return NIL to force recompilation."
  nil)


;; Use *load-pathname* instead of asdf:system-source-directory to avoid circular dependency
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((root (make-pathname :name nil :type nil :defaults *load-pathname*))
         (src-dir (merge-pathnames "src/" root))
         (problem-file (merge-pathnames "problem.lisp" src-dir))
         (blocks3-file (merge-pathnames "problem-blocks3.lisp" src-dir))
         (vals-file (merge-pathnames "vals.lisp" root)))
    (unless (probe-file problem-file)
      ;; No problem.lisp exists, copy default
      (uiop:copy-file blocks3-file problem-file)
      ;; Delete vals.lisp to force rebuild
      (uiop:delete-file-if-exists vals-file))))


(defsystem "wouldwork"
  :author ("Program Development, Dave Brown <davypough@gmail.com>"
           "Quicklisp Integration & Test, Gwang-Jin Kim <gwang.jin.kim.phd@gmail.com>")
  :version "0.0.1"
  :license "MIT"
  :description "classical planning with the wouldwork planner"
  :homepage "https://github.com/davypough/quick-wouldwork"
  :bug-tracker "https://github.com/davypough/quick-wouldwork/issues"
  :source-control (:git "https://github.com/davypough/quick-wouldwork.git")
  :depends-on (:alexandria :iterate :lparallel
               #-sbcl :genhash
               #-sbcl :trivial-backtrace
               #-sbcl :metering)
  :perform (compile-op :after (o c)
                      (declare (ignore o c))
                      (pushnew :wouldwork *features*))
  :around-compile (lambda (next)
                    (handler-bind (((or style-warning warning) #'muffle-warning))
                      (funcall next)))
  :components ((:module "src"
                :serial t
                :components ((:file "ww-packages")
		                     (:file "ww-utilities")
		                     (:file "ww-hstack")
                             (:file "ww-preliminaries")
		                     (:file "ww-settings")
		                     (:file "ww-structures")
		                     (:file "ww-converter")
		                     (:file "ww-validator")
		                     (:file "ww-frequencies")
		                     (:file "ww-support")
		                     (:file "ww-happenings")
		                     (:file "ww-translator")
		                     (:file "ww-installer")
                             (:file "ww-interface")
                             (:file "ww-problem-tests")
		                     (:file "ww-set")
                             (:file "ww-command-tests")
		                     (always-compile-file "problem"
                                      :around-compile (lambda (thunk) 
                                                        ;asdf sometimes doesn't recompile ww-preliminaries
                                                        (setf (symbol-value (find-symbol "*WW-LOADING*" "WOULDWORK")) t) 
                                                        (funcall thunk)))
		                     (:file "ww-planner")
		                     (:file "ww-searcher")
                             (:file "ww-backtracker")
		                     (:file "ww-parallel")
		                     (:file "ww-initialize"))))
  :build-operation "program-op"
  :build-pathname "wouldwork"
  :entry-point "wouldwork:main")