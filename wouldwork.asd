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
  ;:around-compile (lambda (thunk)
  ;                  (print (funcall thunk)))
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
		                     (:file "ww-set")
                             (:file "ww-interface")
		                     (:file "problem")
		                     (:file "ww-searcher")
		                     (:file "ww-planner")
		              #+sbcl (:file "ww-parallel")
		                     (:file "ww-initialize"))))
  :build-operation "program-op"  ;build a binary: binary name: adapt.
  :build-pathname "wouldwork"
  :entry-point "wouldwork:main")  ;entry point: here "main" is an exported symbol. Otherwise, use a double ::


(defmethod asdf:prepare-op ((operation asdf:operation)
                           (system (eql (asdf:find-system :wouldwork)))
                           &key)
  "First delete all compiled files so ASDF will always recompile wouldwork before reloading."
  (let* ((sys (asdf:find-system :wouldwork))
         (files (asdf:output-files 'asdf:compile-op sys)))
    (dolist (file files)
      (when (probe-file file)
        (delete-file file))))
  (call-next-method))