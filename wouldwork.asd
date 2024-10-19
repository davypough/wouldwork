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
  :serial t
  ;:around-compile (lambda (thunk)
  ;                  (print (funcall thunk)))
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
		                     (:file "ww-utilities")
		                     (:file "ww-hstack")
		                     (:file "ww-settings")
		                     (:file "ww-structures")
		                     (:file "ww-converter")
		                     (:file "ww-validator")
		                     (:file "ww-frequencies")
		                     (:file "ww-support")
		                     (:file "ww-happenings")
		                     (:file "ww-translator")
		                     (:file "ww-installer")
		                     (:file "ww-set")
                             (:file "ww-interface")
		                     (:file "problem")
		                     (:file "ww-searcher")
		                     (:file "ww-planner")
		                     #+sbcl (:file "ww-parallel")
		                     (:file "ww-initialize"))))
  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "wouldwork"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "wouldwork:main")
