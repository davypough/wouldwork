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
           (format t "~2%When *algorithm* is backtracking, *tree-or-graph* must be set to tree.~%")
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
           (load-problem (string *problem-name*))))
           ;(asdf:compile-system :wouldwork :force t)
           ;(asdf:load-system :wouldwork)))
       (*algorithm*  ;need to recompile current problem for new translations
         (when *ww-loading*
           (error "Please remove (ww-set *algorithm* ~S) from the current problem specification file.
                   Instead, enter it at the REPL after loading/staging)." ',val))
         (unless *ww-loading*  ;ignore (ww-set *algorithm* ...) in problem specification
           (setf ,param ',val)
           (save-globals)
           (with-silenced-compilation
             (load-problem (string *problem-name*)))))
       (*probe*
         (when *ww-loading*
           (error "Please remove (ww-set *probe* ~S) from the current problem specification file.
                   Instead, enter it at the REPL after loading/staging)." ',val))
         (setf ,param ',val)
         (if (or (> *debug* 0) *probe*)
           (pushnew :ww-debug *features*)
           (setf *features* (remove :ww-debug *features*)))
         (setf *debug* 0)
         (save-globals)
         (with-silenced-compilation
           (load-problem (string *problem-name*))))
       ((*problem-name* *problem-type*)
          (if *ww-loading*
            (setf ,param ',val)
            (format t "~%Please set the parameter ~A in the problem specification file, not in the REPL.~%" ',param))))))
