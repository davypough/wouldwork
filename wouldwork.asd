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
                             (:file "ww-patroller-installer")
                             (:file "ww-interface")
                             (:file "ww-problem-tests")
		                     (:file "ww-set")
                             (:file "ww-command-tests")
		                     (always-compile-file "problem" :around-compile 
                                      (lambda (thunk)
                                        (setf (symbol-value (find-symbol "*WW-LOADING*" "WOULDWORK")) t)
                                        ;; Pre-scan problem.lisp before compilation
                                        (let ((query-names (find-symbol "*QUERY-NAMES*" "WOULDWORK"))
                                              (update-names (find-symbol "*UPDATE-NAMES*" "WOULDWORK"))
                                              (happening-names (find-symbol "*HAPPENING-NAMES*" "WOULDWORK"))
                                              (problem-path (asdf:system-relative-pathname :wouldwork "src/problem.lisp"))
                                              (*package* (find-package "WOULDWORK")))
                                          (with-open-file (stream problem-path :direction :input)
                                            (loop for form = (read stream nil nil)
                                                  while form
                                                  do (when (and (consp form)
                                                                (symbolp (car form)))
                                                       (let ((form-name (symbol-name (car form))))
                                                         ;; Handle queries and updates (2nd element is function name)
                                                         (when (member form-name 
                                                                       '("DEFINE-QUERY" "DEFINE-UPDATE")
                                                                       :test #'string=)
                                                           (let ((fn-name (second form))
                                                                 (is-query (string= form-name "DEFINE-QUERY")))
                                                             (push fn-name
                                                                   (symbol-value (if is-query query-names update-names)))))
                                                         ;; Handle happenings and patrollers (2nd element is object name)
                                                         (when (member form-name
                                                                       '("DEFINE-HAPPENING" "DEFINE-PATROLLER")
                                                                       :test #'string=)
                                                           (push (second form) (symbol-value happening-names))))))))
                                        (funcall thunk)))
                             (:file "ww-action-trace")
                             (:file "ww-goal-chaining")
                             (:file "ww-solution-validation")
                             (:file "ww-backwards")
		                     (:file "ww-planner")
                             (:file "ww-symmetry")
		                     (:file "ww-searcher")
                             (:file "ww-backtracker")
                             (:file "ww-parallel-infrastructure")
		                     (:file "ww-parallel")
		                     (:file "ww-initialize"))))
  :build-operation "program-op"
  :build-pathname "wouldwork"
  :entry-point "wouldwork:main")