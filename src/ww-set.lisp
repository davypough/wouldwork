;;; Filename: ww-set.lisp

;;; User interface for resetting Wouldwork's search control parameters.

(in-package :ww)


(defmacro ww-set (param val)
  "Generate only the selected parameter's setting code.
   During refresh, problem-file ww-set forms are ignored
   so current parameter settings are preserved."
  `(unless (and *refreshing* *ww-loading*)
     (reject-worker-read-write 'ww-set)
     (check-problem-parameter ',param ',val)  ;catch syntax errors before setting
     ,(case param
       (*worker-read-snapshots*
         `(progn
           (setf *worker-read-snapshots* ',val)
           (format t "~&Worker read snapshots: ~S (STAGE restores T)~%" ',val)))
       ((*depth-cutoff* *progress-reporting-interval* *progress-first-report* *randomize-search*
         *branch* *time-limit* *auto-wait* *failed-state-memory* *tasks-per-thread* *min-tasks* *split-depth-max*  ;CHANGED
         *bound-refresh-interval* *donation-check-interval* *donation-threshold*
         *donation-fraction* *enable-work-donation* *max-recorder-cycles*
         *recorder-prefix-pruning* *bt-cycle-check*)
         `(progn
            (setf ,param ',val)
            (unless *ww-loading*
              (save-globals)
              (display-current-parameters))))
       (*solution-type*
         `(progn
           (when (and (eq *algorithm* 'backtracking)
                      (member ',val '(min-length min-time min-value max-value)))
             (format t "~%Note: Backtracking with *solution-type* ~A uses user bounding-function? and move lower bounds, but has no automatic objective-bound pruning.~%" ',val))
           (setf ,param ',val)
           (unless *ww-loading*
             (save-globals)
              (display-current-parameters))))
       ((*max-connector-pairings* *search-registers-dynamic-objects*)
         `(progn
           (setf ,param ',val)
           ;; A technology-specific problem setting should survive refresh, but not
           ;; leak into another problem through the persisted globals file.
           (unless *ww-loading*
             (display-current-parameters))))
       (*num-closed-shards*
         `(progn
           (setf *num-closed-shards* ',val
                 *closed-shard-mask* (1- ',val))
           (unless *ww-loading*
             (save-globals)
             (display-current-parameters))))
       (*tree-or-graph*
         `(progn
           (if (and (eq *algorithm* 'backtracking) (eq ',val 'graph))
             (format t "~2%When *algorithm* is backtracking, *tree-or-graph* must be set to tree.~2%")
             (progn (setf ,param ',val)
                    (unless *ww-loading*
                      (save-globals)
                      (display-current-parameters))))))
       (*debug*  ;need to recompile
         `(progn
           (when *ww-loading*
             (error "Please remove (ww-set *debug* ~S) from the current problem specification file.
                   Instead, enter it at the REPL after loading/staging)." ',val))
           (setf ,param ',val)
           (if (or (> *debug* 0) *probe*)
             (pushnew :ww-debug *features*)
             (setf *features* (remove :ww-debug *features*)))
           (save-globals)
           (with-silenced-compilation
             (asdf:load-system :wouldwork :force t))))
       (*algorithm*  ;translation depends on it, so a REPL change recompiles the current problem
         `(progn
           (if *ww-loading*
             ;; A problem file's setting applies only on a fresh stage, before vals.lisp exists.
             ;; On a later reload vals.lisp has already restored the saved value, which the
             ;; problem's actions are about to be translated with, so the saved value stands.
             (unless (probe-file *globals-file*)
               (setf ,param ',val))
             (progn
               (setf ,param ',val)
               (when (and (eq ',val 'backtracking) (eq *tree-or-graph* 'graph))
                 (setf *tree-or-graph* 'tree))
               (save-globals)
               (with-silenced-compilation
                 (asdf:load-system :wouldwork :force t))))))
       (*probe*
         `(progn
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
             (asdf:load-system :wouldwork :force t))))
       (*symmetry-pruning*
         `(progn
            (setf ,param ',val)
            (unless *ww-loading*
              (save-globals)
              (with-silenced-compilation
                (asdf:load-system :wouldwork :force t)))))
       (*problem-type*
         `(progn
           (if *ww-loading*
             (setf ,param ',val)
             (progn
               (setf ,param ',val)
               (save-globals)
               (with-silenced-compilation
                 (asdf:load-system :wouldwork :force t))))))
       (*problem-name*  ;unchanged: must be set in problem specification file
         `(progn
            (if *ww-loading*
              (setf ,param ',val)
              (format t "~%Please set the parameter ~A in the problem specification file, not in the REPL.~%" ',param))))
       (*threads*
         `(progn
            (let ((crossing-boundary (not (eql (zerop *threads*) (zerop ',val)))))
              (setf ,param ',val)
              ;; Problem declarations run inside ASDF. The prescan in ww-preliminaries
              ;; has already applied a top-level declaration, so this setf changes
              ;; nothing during staging.
              (unless *ww-loading*
                (save-globals)
                (if crossing-boundary
                  (with-silenced-compilation
                    (asdf:load-system :wouldwork :force t))
                  (display-current-parameters)))))))))
