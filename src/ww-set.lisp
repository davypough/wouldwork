;;; Filename: ww-set.lisp

;;; User interface for resetting Wouldwork's search control parameters.

(in-package :ww)


(defmacro ww-set (param val)
  "Allows resetting of user parameters during and after loading."
  `(progn
     (check-problem-parameter ',param ',val)  ;catch syntax errors before setting
     (case ',param
       ((*depth-cutoff* *solution-type* *progress-reporting-interval* *randomize-search* *branch*)
          (setf ,param ',val)
          (unless *ww-loading*
            (save-globals)
            (display-current-parameters)))
       (*tree-or-graph*
         (if (and (eq *algorithm* 'backtracking) (eq ',val 'graph))
           (format t "~%When *algorithm* is backtracking, *tree-or-graph* must be set to tree.~%")
           (progn (setf ,param ',val)
                  (unless *ww-loading*
                    (save-globals)
                    (display-current-parameters)))))
       (*debug*  ;need to recompile
         (when *ww-loading*
           (error "Please remove (ww-set *debug* ~S) from the current problem specification file.
                   Instead, enter it at the REPL after loading/staging)." ',val))
         (setf ,param ',val)
         (if (or (> *debug* 0) *probe*)
           (pushnew :ww-debug *features*)
           (setf *features* (remove :ww-debug *features*)))
         (save-globals)
         (with-silenced-compilation
           (asdf:compile-system :wouldwork :force t)
           (asdf:load-system :wouldwork)))
       (*algorithm*  ;need to recompile current problem for new translations
         (unless *ww-loading*
           (setf ,param ',val)
           (save-globals)
           (with-silenced-compilation
             (load-problem (string *problem-name*)))))
       (*probe*
         (if (null ',val)
           (progn (setf ,param nil)
                  (unless *ww-loading*
                    (save-globals)
                    (display-current-parameters)))
           (destructuring-bind (action instantiations depth &optional (count 1)) ',val
             (declare (ignore action instantiations depth))
             (setf ,param ',val)
             (setf *debug* 0)
             (setf *counter* count)
             (unless *ww-loading*
               (save-globals)
               (display-current-parameters)))))
         ;(unless *ww-loading*
         ;  (display-current-parameters)))
       ((*problem-name* *problem-type*)
          (if *ww-loading*
            (setf ,param ',val)
            (format t "~%Please set the parameter ~A in the problem specification file, not in the REPL.~%" ',param))))))
