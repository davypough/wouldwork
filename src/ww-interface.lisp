;;; Filename: ww-interface.lisp

;;; Misc file handling & test managment functions


(in-package :wouldwork)


(defparameter *default-parameters*
  '(unspecified      ; *problem-name*
    0                ; *depth-cutoff*  
    depth-first      ; *algorithm*
    graph            ; *tree-or-graph*
    planning         ; *problem-type*
    first            ; *solution-type*
    100000           ; *progress-reporting-interval*
    nil              ; *randomize-search*
    -1               ; *branch*
    nil              ; *probe*
    0                ; *debug*
    nil)             ; *goal* 
  "Default parameter values in save/read order")


(defparameter *globals-file* 
  (merge-pathnames "vals.lisp" (asdf:system-source-directory :wouldwork))
  "In the vals.lisp file of this package the values of parameters
     are stored as a list.
   This should preserve when reloading the package for problems
   the values of these global variables.")


(defun help ()  ;;; text which appears if user enters (help)
  (format t "~%
THE LIST OF WOULDWORK COMMANDS RECOGNIZED IN THE REPL:

(run <problem-name>) eg, (run \"blocks3\") or (run blocks3)
   -- load and solve a problem 

(run-test-problems) alias (test)
   -- solve all test problems

(test-commands)
  -- run a series of tests to exercise potential user REPL commands

(list-all-problems) alias (probs)
   -- lists all currently specifed problems
      in the src directory (use these names with run or stage)

(stage <problem-name>) eg, (stage \"blocks3\") or (stage blocks3)
  -- loads a problem into wouldwork in preparation for solving or debugging,
     without attempting to solve it

(solve)
  -- attempts to solve the currently staged problem

(get-src-folder-path)
   -- the location where all problem specification files should appear

(profile)
   -- employs a basic profiler on the currently staged problem,
      for analyzing the efficiency of action rules in the problem specification

(display-current-parameters) alias (params)
   -- displays all parameters associated with the currently staged problem

(refresh)
  -- refreshes (ie, reloads) the current problem specification file after editing it

(ww-reset)
  -- if Wouldwork throws an error, it can be reinitialized and maybe enable continuing


(ww-set <problem-parameter> <new-value>)
   -- set a problem parameter to a new value
   eg, (ww-set *solution-type* <one of first, every, min-length, min-time,
                                       min-value, or max-value>)
       (ww-set *tree-or-graph* <one of tree or graph>)
       (ww-set *depth-cutoff* <positive integer (search to specified depth) or
                                                 0 (no depth limit)>)
       (ww-set *progress-reporting-interval* <positive integer;
                                              eg, 100000 (how often to report progress)>)
       (ww-set *randomize-search* <t (random depth-first search) or
                                   nil (standard depth-first search)>)
       (ww-set *branch* <number (eg, search only branch 1 (first) of 10 initial branches) or
                         -1 (search all branches)>)
       (ww-set *debug* <one of 0 (no debugging), 1-4 (increasing debugging info),
                               5 (step through search)>)
       (ww-set *probe* (<action name> <instantiations> <depth> &optional <count>))
           -- probe enables debugging when a state is reached during search
              see ww-settings.lisp and User Manual for probe format examples

Note that setting problem parameters at the REPL with ww-set will override
any such settings appearing in the problem specification file.
"))


;; -------------------- some basic string functions ------------------ ;;

(defun string-prefix-p (prefix str)
  "Return T if PREFIX is a prefix of STR, otherwise NIL."
  (and (<= (length prefix) (length str))
       (string= prefix (subseq str 0 (length prefix)))))

(defun string-suffix-p (suffix str)
  "Return T if SUFFIX is a suffix of STR, otherwise NIL."
  (and (<= (length suffix) (length str))
       (string= suffix (subseq str (- (length str) (length suffix))))))

(defun lstrip (str prefix)
  "Removes prefix from str (only 1x)."
  (let ((result str))
    (when (string-prefix-p prefix result)
      (setf result (subseq result (length prefix))))
    result))

(defun rstrip (str suffix)
  "Removes suffix from str (only 1x)."
  (let ((result str))
    (when (string-suffix-p suffix result)
      (setf result (subseq result 0 (- (length result) (length suffix)))))
    result))

(defun strip-name (str prefix suffix)
  "Removes prefix and suffix from str."
  (let* ((without-prefix (lstrip str prefix))
         (suffix-with-dot (concatenate 'string "." suffix))
         (result (rstrip without-prefix suffix-with-dot)))
    result))

(defun lookup (key plist &key (test #'string-equal) (default))
  "Key value lookup in plist with #'string= or any other function as test.
   The plist-related getf can only handle eql."
  (let ((res nil)
	(foundp nil))
    (loop for (k v) on plist by #'cddr
	  when (funcall test k key)
	    do (setf res v
		     foundp t)
	  finally (return (values (if res res default) foundp)))))


;; -------------------- pathname handling ---------------------------- ;;

  
(Defun get-src-folder-path ()
  (add-dir (asdf:system-source-directory :wouldwork) "src"))
   
(defun add-dir (root dir)
  "Add to absolute path an additional directory"
  (merge-pathnames (make-pathname :directory `(:relative ,dir)) root))
  
(defun add-file (root file)
  "Add to absolute path a filename"
  (merge-pathnames (pathname file) root))
  
(defun directory-exists-p (directory)
  "Returns pathname if the directory exists and is a directory.
   Currently only works with SBCL - but not CLISP!"
  (let ((path (pathname directory)))
    (and (probe-file path)
         (string-suffix-p "/" (format nil "~a" (probe-file path))))))
  
(defun in-src (filename)
  "Shortcut to add filename to current package directory's src folder"   
  (add-file (get-src-folder-path) filename))


;; --------------------- file handling ------------------------------- ;;


(defun copy-file-content (source-file target-file)
  "Replace the content of target-file by the content of source-file."
  (with-open-file (in source-file :direction :input)
    (with-open-file (out target-file :direction :output :if-exists :supersede)
      (loop for line = (read-line in nil nil)
	    while line
	    do (write-line line out)))))


(defun save-to-file (list filename)
  (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "~S" list)))


(defun read-from-file (filename &optional (default '()))
  (if (probe-file filename)  ; Check if the file exists
      (with-open-file (stream filename :direction :input)
        (read stream))
      ;; If file doesn't exist, create it with the default values
      (progn
        (save-to-file default filename)
        default)))


(defun display-globals ()
  (format t "~&*problem-name* ~A~% 
               *depth-cutoff* ~A~%
               *algorithm* ~A~%
               *tree-or-graph* ~A~%
               *problem-type* ~A~%
               *solution-type* ~A~%
               *progress-reporting-interval* ~A~%
               *randomize-search* ~A~%
               *branch* ~A~%
               *probe* ~A~%
               *debug* ~A~2%"
            *problem-name* *depth-cutoff* *algorithm* *tree-or-graph* *problem-type*
            *solution-type* *progress-reporting-interval* *randomize-search* *branch* 
            *probe* *debug*))


(defun refresh ()
  "Refreshes the current problem.lisp file--eg, after editing it."
  (uiop:delete-file-if-exists (in-src "problem.lisp"))
  (with-silenced-compilation
    (load-problem (string *problem-name*))))
  

(defun reset-parameters ()
   "Resets global parameters to defaults"
  (destructuring-bind 
       (default-problem-name default-depth-cutoff default-algorithm default-tree-or-graph 
        default-problem-type default-solution-type default-progress-reporting-interval 
        default-randomize-search default-branch default-probe default-debug default-goal)
      *default-parameters*
    (setf *problem-name* default-problem-name
          *depth-cutoff* default-depth-cutoff  
          *algorithm* default-algorithm
          *tree-or-graph* default-tree-or-graph
          *problem-type* default-problem-type
          *solution-type* default-solution-type
          *progress-reporting-interval* default-progress-reporting-interval
          *randomize-search* default-randomize-search
          *branch* default-branch
          *probe* default-probe
          *debug* default-debug
          *goal* default-goal))
  (setf *features* (remove :ww-debug *features*)))


(defun save-globals ()
  "Save the values of the globals in the vals.lisp file."
  (save-to-file (list *problem-name* *depth-cutoff* *algorithm* *tree-or-graph* *problem-type*
                      *solution-type* *progress-reporting-interval* *randomize-search* 
                      *branch* *probe* *debug* *goal*)
                *globals-file*))


(defun read-globals ()
  "Read and setf values for global variables from vals.lisp file."
  (destructuring-bind 
       (problem-name depth-cutoff algorithm tree-or-graph problem-type solution-type
        progress-reporting-interval randomize-search branch probe debug goal)
      (read-from-file *globals-file* *default-parameters*)
      (setf *problem-name* problem-name
            *depth-cutoff* depth-cutoff
            *algorithm* algorithm
            *tree-or-graph* tree-or-graph
            *problem-type* problem-type
            *solution-type* solution-type
            *progress-reporting-interval* progress-reporting-interval
            *randomize-search* randomize-search
            *branch* branch
            *probe* probe
            *debug* debug
            *goal* goal)))


;; -------------------- problem.lisp file handling ------------------------ ;;


(defparameter *problem-folder-paths* (list (get-src-folder-path))
"This variable holds all folder pathnames which can hold problems in this system.
   The user cann add custom folder pathnames to this folder using the function
   `add-problem-folder` and remove by `remove-problem-folder`.
   The Package directory's `src` folder, however will always persist.")


(defun add-problem-folder (folder-path)
  "Adds an additional path to a folder containing problem-*.lisp files to the
   global list `*problem-folder-paths*`."
  (let ((path (pathname folder-path)))
    (if (directory-exists-p path)
        (push (probe-file path) *problem-folder-paths*)
        (format t "\"~a\" is either not a path to a folder or there are other problems."
                path))))


(defun remove-problem-folder (folder-path)
  "Removes folder-path from global `*problem-folder-paths*` list.
   It always leaves the packages' `src` folder present!"
  (let ((path (probe-file (pathname folder-path))))
    (cond ((<= (length *problem-folder-paths*) 1)
           (Format t "Not removing anything, because *problem-folder-paths* contains only the src folder")
           *problem-folder-paths*)
          (t
           (setf *problem-folder-paths* (remove-if (lambda (p) (string= (format nil "~a" p)
                                                                        (format nil "~a" path)))
                                                   *problem-folder-paths*))
           *problem-folder-paths*))))


(defun list-problem-files-plist (&optional (prefix "problem-") (suffix "lisp"))
  "Return a plist of files in the 'src' directory that start with 'problem-'.
   The key is the filename without 'problem-' and '.lisp'.
   The value is the full path of the file. Uses the root directory of the 'wouldwork' system."
  (let ((files)
        (result))
    (loop for dir in *problem-folder-paths*
          do (let ((path (format nil "~A~A*.~A" (namestring dir) prefix suffix)))
               (setf files (append (directory path) files))))
    (dolist (file files)
      (let* ((filename (file-namestring file))
             (name (strip-name filename prefix suffix)))
        (when (and (string-prefix-p prefix filename)
                   (string-suffix-p (concatenate 'string "." suffix) filename))
          (push name result)
          (push file result))))
    (nreverse result)))


(defun list-problem-names ()
  (let* ((plist (list-problem-files-plist)))
    (loop for (k nil) on plist by #'cddr
	  collect k)))

(setf (fdefinition 'probs) #'list-problem-names)


(defun exchange-problem-file (problem-name-str)
  "Copies problem file to src/problem.lisp so it will be compiled by asdf."
  (let* ((plist (list-problem-files-plist))
	     (problem-file (lookup problem-name-str plist)))
    (copy-file-content problem-file (in-src "problem.lisp"))))


(defun load-problem (problem-name-str)
  "Given a problem-name, replace the content of the problem.lisp file by
   the content of the correponsing problem file, and then reload everything."
  (exchange-problem-file problem-name-str)
  (asdf:load-system :wouldwork :force t))


(declaim (ftype (function () t) solve))  ;function ww-solve located in searcher.lisp


(defmacro run (problem-name)
  "Stages and solves a user specified problem."
  `(%run ,(string problem-name)))


(defun %run (problem-name-str)
  "Stages and solves a user specified problem with default parameters."
  (when (%stage problem-name-str)
    (ww-solve)))


(defmacro stage (problem-name)
  "Loads a specified problem to be subsequently solved. This allows the user to verify/debug their problem
   specification, and check the current parameters, without asking wouldwork to solve it as run does.
   Once the problem loads correctly, it can then be solved with a follow-up (solve) command."
  `(%stage ,(string problem-name)))


(defun %stage (problem-name-str)
  "Loads a specified problem to be subsequently solved. This allows the user to verify/debug their problem
   specification, and check the current parameters, without asking wouldwork to solve it as run does.
   Once the problem loads correctly, it can then be solved with a follow-up (solve) command."
  (unless (member problem-name-str (list-problem-names) :test #'string-equal)
    (format t "The problem ~A was not found." problem-name-str)
    (format t "~&Enter (list-all-problems) for a complete list of problems." )
    (return-from %stage))
  (uiop:delete-file-if-exists *globals-file*)
  (exchange-problem-file problem-name-str)  ;copy problem-<problem-name-str>.lisp to problem.lisp
  (setf *problem-name* (intern problem-name-str)  ;reset to defaults
        *depth-cutoff* 0
        *algorithm* 'depth-first
        *probe* nil
        *progress-reporting-interval* 100000
        *problem-type* 'planning
        *solution-type* 'first
        *tree-or-graph* 'graph
        *debug* 0
        *branch* -1
        *randomize-search* nil
        *features* (remove :ww-debug *features*))
  (with-silenced-compilation
    (load-problem problem-name-str)))


(defun solve ()
  (ww-solve))


(defun list-all-problems (&optional (prettyp nil))
  "List all problem names in the problem folder.
   One-per-line: (list-all t) or (list-all :pretty)"
  (if prettyp
      (loop for name in (list-problem-names)
            do (format t "~a~%" name))
      (list-problem-names)))
