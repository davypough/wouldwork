;;; Filename: ww-set.lisp

;;; User interface for resetting Wouldwork's search control parameters.

(in-package :ww)


(defmacro ww-set (param val)
  "Set a problem parameter. During refresh, problem-file ww-set forms are ignored
   so current parameter settings are preserved."
  `(unless (and *refreshing* *ww-loading*)
     (check-problem-parameter ',param ',val)  ;catch syntax errors before setting
     (case ',param
       ((*depth-cutoff* *progress-reporting-interval* *randomize-search*
         *branch* *auto-wait* *tasks-per-thread* *min-tasks* *split-depth-max*
         *bound-refresh-interval* *donation-check-interval* *donation-threshold*
         *donation-fraction* *enable-work-donation* *max-recorder-cycles*
         *recorder-prefix-pruning*)
          (setf ,param ',val)
          (unless *ww-loading*
            (save-globals)
            (display-current-parameters)))
       (*solution-type*
         (when (and (eq *algorithm* 'backtracking)
                    (member ',val '(min-length min-time min-value max-value)))
           (format t "~%Note: *solution-type* ~A requires optimality pruning, which the backtracking algorithm does not perform; all solutions will be enumerated without pruning.~%" ',val))
         (setf ,param ',val)
         (unless *ww-loading*
           (save-globals)
            (display-current-parameters)))
       (*max-connector-pairings*
         (setf ,param ',val)
         ;; A technology-specific problem setting should survive refresh, but not
         ;; leak into another problem through the persisted globals file.
         (unless *ww-loading*
           (display-current-parameters)))
       (*num-closed-shards*
         (setf *num-closed-shards* ',val
               *closed-shard-mask* (1- ',val))
         (unless *ww-loading*
           (save-globals)
           (display-current-parameters)))
       (*tree-or-graph*
         (if (and (eq *algorithm* 'backtracking) (eq ',val 'graph))
           (format t "~2%When *algorithm* is backtracking, *tree-or-graph* must be set to tree.~2%")
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
           (asdf:load-system :wouldwork :force t)))
       (*algorithm*  ;need to recompile current problem for new translations
         (when *ww-loading*
           (error "Please remove (ww-set *algorithm* ~S) from the current problem specification file.
                   Instead, enter it at the REPL after loading/staging)." ',val))
         (unless *ww-loading*  ;ignore (ww-set *algorithm* ...) in problem specification
           (setf ,param ',val)
           (when (and (eq ',val 'backtracking) (eq *tree-or-graph* 'graph))
             (setf *tree-or-graph* 'tree))
             ;(format t "~2%Note: setting *tree-or-graph* to tree (graph not compatible with backtracking).~%"))
           (save-globals)
           (with-silenced-compilation
             (asdf:load-system :wouldwork :force t))))
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
           (asdf:load-system :wouldwork :force t)))
       (*symmetry-pruning*
          (setf ,param ',val)
          (unless *ww-loading*
            (save-globals)
            (with-silenced-compilation
              (asdf:load-system :wouldwork :force t))))
       (*problem-type*
         (if *ww-loading*
           (setf ,param ',val)
           (progn
             (setf ,param ',val)
             (save-globals)
             (with-silenced-compilation
               (asdf:load-system :wouldwork :force t)))))
       (*problem-name*  ;unchanged: must be set in problem specification file
          (if *ww-loading*
            (setf ,param ',val)
            (format t "~%Please set the parameter ~A in the problem specification file, not in the REPL.~%" ',param)))
       (*threads*
          (let ((crossing-boundary (or (and (zerop *threads*) (> ',val 0))
                                       (and (zerop ',val) (> *threads* 0)))))
            (setf ,param ',val)
            (save-globals)
            (if crossing-boundary
              (with-silenced-compilation
                (asdf:load-system :wouldwork :force t))
              (display-current-parameters)))))))
