;;; Filename: wouldwork.asd

;;; ASDF instructions for loading wouldwork


(in-package :asdf-user)


(defsystem "wouldwork"
  :author ("Program Development, Dave Brown <davypough@gmail.com>"
           "Quicklisp Integration & Test, Gwang-Jin Kim <gwang.jin.kim.phd@gmail.com>")
  :version "0.0.1"
  :license "MIT"
  :description "classical planning with the wouldwork planner"
  :homepage "https://github.com/davypough/quick-wouldwork"
  :bug-tracker "https://github.com/davypough/quick-wouldwork/issues"
  :source-control (:git "https://github.com/davypough/quick-wouldwork.git")
  :depends-on (:alexandria :iterate #+sbcl :lparallel
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
                :components ((:file "packages")
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
		                     (:file "problem" 
                                      :around-compile (lambda (thunk) 
                                                        ;asdf sometimes doesn't recompile ww-preliminaries
                                                        (setf (symbol-value (find-symbol "*WW-LOADING*" "WOULDWORK")) t) 
                                                        (funcall thunk)))
		                     (:file "ww-planner")
		                     (:file "ww-searcher")
		              #+sbcl (:file "ww-parallel")
		                     (:file "ww-initialize"))))
  :build-operation "program-op"  ;build a binary: binary name: adapt.
  :build-pathname "wouldwork"
  :entry-point "wouldwork:main")  ;entry point: here "main" is an exported symbol. Otherwise, use a double ::
