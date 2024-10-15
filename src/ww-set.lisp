;;; Filename: ww-set.lisp

;;; User interface for resetting Wouldwork's search control parameters.

(in-package :ww)


(defmacro ww-set (param val)
  "Allows resetting of user parameters during and after loading."
  `(progn
     (check-problem-parameter ',param ',val)  ;catch errors before setting
     (case ',param
       ((*depth-cutoff* *tree-or-graph* *solution-type*
         *progress-reporting-interval* *randomize-search* *branch*)
        (progn (setq ,param ,(if (symbolp val) `',val val))
               (save-repl-parameters)
               ,(if (symbolp val) `',val val)))
       (*debug*
        (progn (setq *debug* ,(if (symbolp val) `',val val))
               (if (or (> *debug* 0) *probe*)
                   (pushnew :ww-debug *features*)
                   (setf *features* (remove :ww-debug *features*)))
               (save-repl-parameters)
               ,(if (symbolp val) `',val val)))
       (*probe*
        (progn 
          (unless (listp ',val)
            (error "*probe* value must be an unquoted list"))
          (setq *probe* ',val)
          (setf *debug* 0)
          (setq *counter* 1)
          (save-repl-parameters)
          ',val))
       (*threads*
        '(block sbcl-test
           (progn (unless (member :sbcl *features*)
                    (format t "~%Note that multi-threading is not available unless running SBCL.~2%")
                    (return-from sbcl-test))
                  (format t "~%*threads* cannot be changed with ww-set.")
                  (format t "~%Instead, set its value in the file settings.lisp, and then exit and restart SBCL.~2%"))))
       ((*problem-name* *problem-type*)
        (if *ww-loading*
          (setq ,param ,(if (symbolp val) `',val val))
          (format t "~%Please set the parameter ~A in the problem specification file, not in the REPL.~%" ',param)))
       (otherwise
        (format t "~%~A is not a valid parameter name in ww-set.~%" ',param)))))


(defun save-repl-parameters ()
  (unless *ww-loading*  ;only save if ww-set command comes from REPL after loading
    (defparameter *keep-globals-p* t)
    (save-globals)))
