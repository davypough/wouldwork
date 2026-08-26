;;; Filename: wouldwork.asd

;;; ASDF instructions for loading wouldwork


(in-package :asdf-user)


;; Distinguishes concurrently-run SBCL processes so each stages and compiles its own
;; problem<suffix>.lisp / vals<suffix>.lisp instead of contending over the same two files.
;; Set the WOULDWORK_INSTANCE environment variable before starting a second SBCL process
;; against this same checkout; leave it unset for normal single-process use, which reduces
;; the suffix to "" and reproduces today's plain problem.lisp / vals.lisp filenames exactly.
;; Lives in CL-USER, not WOULDWORK, because this form and everything below it in this file
;; runs before ww-packages.lisp defines the :wouldwork package.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar cl-user::*ww-instance-suffix*
    (let ((instance (uiop:getenv "WOULDWORK_INSTANCE")))
      (if instance (concatenate 'string "-" instance) ""))))


(defclass always-compile-file (asdf:cl-source-file)
  ()
  (:documentation "A source file that is always compiled, regardless of timestamps."))


(defmethod asdf:operation-done-p ((o asdf:compile-op) (c always-compile-file))
  "Always return NIL to force recompilation."
  nil)


(defmethod asdf:component-pathname ((c always-compile-file))
  "Resolve to problem<suffix>.lisp, where <suffix> is CL-USER::*WW-INSTANCE-SUFFIX*, so
   concurrently-run SBCL processes compile and stage independent problem files."
  (let ((default-pathname (call-next-method)))
    (make-pathname
      :name (concatenate 'string (pathname-name default-pathname) cl-user::*ww-instance-suffix*)
      :defaults default-pathname)))


;; Use *load-pathname* instead of asdf:system-source-directory to avoid circular dependency.
;; This is the earliest-possible bootstrap: it runs before ww-packages.lisp defines the
;; :wouldwork package, so it cannot call copy-problem-with-tech-includes or
;; ensure-problem-staged (ww-preliminaries.lisp) -- neither symbol exists yet.  It therefore
;; does a plain file copy, not a tech-splicing one.  INVARIANT: problem-blocks3.lisp must
;; never contain an (include-tech ...) directive, or this copy leaves an unexpanded
;; directive in problem.lisp and the subsequent compile fails.  Every other entry point
;; that stages a problem (stage, run, refresh, the cl-user recovery refresh, and the
;; reload-time eval-when in ww-preliminaries.lisp) delegates to ensure-problem-staged
;; instead, once the system is loaded far enough for that function to exist.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((root (make-pathname :name nil :type nil :defaults *load-pathname*))
         (src-dir (merge-pathnames "src/" root))
         (problem-file (merge-pathnames
                          (concatenate 'string "problem" cl-user::*ww-instance-suffix* ".lisp")
                          src-dir))
         (blocks3-file (merge-pathnames "problem-blocks3.lisp" (merge-pathnames "probs/" root)))
         (vals-file (merge-pathnames
                       (concatenate 'string "vals" cl-user::*ww-instance-suffix* ".lisp")
                       root)))
    (unless (probe-file problem-file)
      ;; No problem<suffix>.lisp exists, copy default
      (uiop:copy-file blocks3-file problem-file)
      ;; Delete vals<suffix>.lisp to force rebuild
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
		                     (:file "ww-problem-lifecycle")
		                     (:file "ww-structures")
		                     (:file "ww-relaxed-heuristics")
		                     (:file "ww-converter")
		                     (:file "ww-validator")
		                     (:file "ww-frequencies")
		                     (:file "ww-support")
		                     (:file "ww-happenings")
		                     (:file "ww-translator")
		                     (:file "ww-init-validator")
		                     (:file "ww-installer")
		                     (:file "ww-propagation-order")
                             (:file "ww-patroller-installer")
                             (:file "ww-interface")
                             (:file "ww-test-characterization")
                             (:file "ww-problem-tests")
		                     (:file "ww-set")
		                     (:file "ww-command-tests")
		                     (:file "ww-enumerator-build")
		                     (:file "ww-enumerator-run")
		                     (always-compile-file "problem" :around-compile
                                      (lambda (thunk)
                                        (setf (symbol-value (find-symbol "*WW-LOADING*" "WOULDWORK")) t)
                                        (funcall (symbol-function
                                                   (find-symbol "PRESCAN-PROBLEM-FILE" "WOULDWORK"))
                                                 (asdf:system-relative-pathname
                                                   :wouldwork
                                                   (concatenate 'string "src/problem"
                                                                cl-user::*ww-instance-suffix* ".lisp")))
                                        (funcall thunk)))
                             (:file "ww-action-trace")
                             (:file "ww-goal-chaining")
                             (:file "ww-advisor")
                             (:file "ww-solution-validation")
                             (:file "ww-backward")
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
