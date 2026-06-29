;;; Filename: ww-preliminaries.lisp

;;; Initial setup functions & macros for wouldwork.


(in-package :ww)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *threads* 0
    "The number of parallel threads to use.
      0 means no parallelism (ie, serial processing)
      1 means use one parallel thread
        (in addition to parallel management, effectively serial, useful for debugging)
      2 means two or more parallel processing threads
      N up to the number of available CPU threads"))


(defmacro with-search-structures-lock (&body body)
  "Protects composite operations on *open* and *closed* search structures."
  (if (> *threads* 0)
      `(bt:with-lock-held (*search-lock*)
         ,@body)
      `(progn ,@body)))



(defparameter *ww-loading* t
  "Flag to indicate if Wouldwork is currently being loaded. Reset in ww-initialize.lisp")


(defparameter *lock* (bt:make-lock))  ;for general thread protection
(defparameter *search-lock*  (bt:make-lock "ww-search-lock"))
(defparameter *integer-lock* (bt:make-lock "ww-integer-lock"))


(defvar *debug* 0
    "Set the debug level for subsequent runs.
      0 - no debugging
      1 - display full search tree
      2 - display full search tree with states
      3 - display basic nodes
      4 - display full nodes
      5 - display full nodes + break after each expansion cycle")


(defmacro with-silenced-compilation (&body body)
  "Macro to suppress normal compilation output while preserving error reporting."
  `(let ((*compile-verbose* nil)
         (*compile-print* nil))
     ,@body))


;; -------------------- string and problem file helpers ------------------ ;;

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

(defun copy-problem-with-tech-includes (source-file target-file)
  "Copy SOURCE-FILE to TARGET-FILE, expanding each (include-tech NAME) directive in
   place by splicing tech/NAME-tech.lisp.  Included tech files may include other
   tech files the same way.  Each technology is spliced at most once per problem
   copy.  This lets a problem compose itself from self-contained technology files
   before the pre-scan reads problem.lisp."
  (let ((included-techs (make-hash-table :test #'equal)))
    (with-open-file (in source-file :direction :input)
      (with-open-file (out target-file :direction :output :if-exists :supersede)
        (loop for line = (read-line in nil nil)
              while line
              do (let ((tech-name (include-tech-directive line)))
                   (if tech-name
                     (write-tech-include tech-name out nil included-techs)
                     (write-line line out))))))))

(defun write-tech-include (tech-name-str out include-stack included-techs)
  "Splice the capability file for TECH-NAME-STR into stream OUT, or note its absence.
   A missing tech file is replaced by a comment and a console notice, so a problem may
   be staged before all of its technologies have been written."
  (when (member tech-name-str include-stack :test #'string=)
    (error "Circular technology include: ~{~A -> ~}~A"
           (reverse include-stack)
           tech-name-str))
  (cond ((gethash tech-name-str included-techs)
         (format out "~&;; (include-tech ~A): already included -- skipped~%"
                 tech-name-str))
        (t
         (let ((tech-file (tech-file-path tech-name-str)))
           (if tech-file
             (progn (setf (gethash tech-name-str included-techs) t)
                    (format out "~&~%;;;; ==== begin technology ~A ====~%~%" tech-name-str)
                    (with-open-file (in tech-file :direction :input)
                      (loop for line = (read-line in nil nil)
                            while line
                            do (let ((nested-tech-name (include-tech-directive line)))
                                 (if nested-tech-name
                                   (write-tech-include nested-tech-name
                                                       out
                                                       (cons tech-name-str include-stack)
                                                       included-techs)
                                   (write-line line out)))))
                    (format out "~%;;;; ==== end technology ~A ====~%" tech-name-str)
                    (format t "~&  included technology: ~A~%" tech-name-str))
             (progn (format out ";; (include-tech ~A): tech/~A-tech.lisp not found -- skipped~%"
                            tech-name-str tech-name-str)
                    (format t "~&  MISSING technology, skipped: ~A~%" tech-name-str)))))))


(defun include-tech-directive (line)
  "If LINE is an (include-tech NAME) directive, return NAME as a string, else NIL."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line))
        (head "(include-tech "))
    (when (string-prefix-p head trimmed)
      (let* ((rest-str (subseq trimmed (length head)))
             (name-end (position-if (lambda (ch) (member ch '(#\Space #\Tab #\))))
                                    rest-str)))
        (when name-end
          (subseq rest-str 0 name-end))))))

(defun tech-file-path (tech-name-str)
  "Resolve tech/<TECH-NAME>-tech.lisp below the Wouldwork root, or NIL if absent."
  (let ((relative (make-pathname :directory '(:relative "tech")
                                 :name (concatenate 'string tech-name-str "-tech")
                                 :type "lisp"))
        (root (asdf:system-source-directory :wouldwork)))
    (probe-file (merge-pathnames relative root))))


(defun snapshot-source-file (snapshot-file)
  "Return the source problem file recorded in SNAPSHOT-FILE's leading Filename
   header (eg, ';;; Filename: problem-claustro4.lisp'), resolved under src/, or
   NIL if the header is absent or the named file does not exist.  This lets the
   loader recover a snapshot's provenance without consulting vals.lisp."
  (let* ((root (asdf:system-source-directory :wouldwork))
         (src-dir (merge-pathnames "src/" root))
         (marker ";;; Filename:")
         (first-line (with-open-file (in snapshot-file :direction :input)
                       (read-line in nil nil)))
         (trimmed (and first-line (string-trim '(#\Space #\Tab #\Return) first-line))))
    (when (and trimmed (string-prefix-p marker trimmed))
      (probe-file (merge-pathnames (string-trim '(#\Space #\Tab #\Return)
                                                (subseq trimmed (length marker)))
                                   src-dir)))))


(defun ww-reset ()
  "Discard generated problem and saved settings, then reload the default problem.
   Allows recovery if wouldwork loading fails with error in problem file."
  (format t "~%Loading wouldwork defaults...~2%")
  (let* ((root (asdf:system-source-directory :wouldwork))
         (problem-file (merge-pathnames "src/problem.lisp" root))
         (vals-file (merge-pathnames "vals.lisp" root))
         (ww-pkg (find-package :ww))
         (refreshing-sym (and ww-pkg (find-symbol "*REFRESHING*" ww-pkg))))
    (when (and refreshing-sym (boundp refreshing-sym))
      (setf (symbol-value refreshing-sym) nil))
    (when (probe-file problem-file) (delete-file problem-file))
    (when (probe-file vals-file) (delete-file vals-file)))
  (asdf:clear-system :wouldwork)
  (handler-bind ((warning #'muffle-warning))
    (let ((*compile-verbose* nil)
          (*compile-print* nil))
      (asdf:load-system :wouldwork :force t)))
  (setf *package* (find-package :ww)))



(defun cleanup-resources ()
  "Attempt to shutdown dangling threads safely in SBCL."
  (format t "~&Cleaning up resources and shutting down threads...~%")
  (let ((current-thread sb-thread:*current-thread*))
    (dolist (thread (sb-thread:list-all-threads))
      (unless (eq thread current-thread)
        (when (sb-thread:thread-alive-p thread)
          (format t "~&Terminating thread: ~A~%" thread)
          (ignore-errors
            (sb-thread:terminate-thread thread))))))
  (format t "~&Cleanup completed.~%"))


(pushnew 'cleanup-resources sb-ext:*exit-hooks*)


;Mainly for debugging
(setf *print-length* nil)  ; Don't limit number of elements printed
(setf *print-level* nil)   ; Don't limit nesting depth
(setf *print-circle* nil)  ; Don't include prior reference #n
;(setf *print-readably* t)
(setq *print-right-margin* 140) ;Allows non-wrap printing of *search-tree* for deep trees.


(defmacro increment-global (var-name &optional (delta-form 1))
  `(progn
     (declaim (type fixnum ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-incf ,var-name ,delta-form)
        `(incf ,var-name ,delta-form))))


(defmacro push-global (item var-name)
  `(progn
     (declaim (type list ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-push ,item ,var-name)
        `(push ,item ,var-name))))


(defmacro pop-global (var-name)
  `(progn
     (declaim (type list ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-pop ,var-name)
        `(pop ,var-name))))


(defun reset-user-syms (symbols)
  "Unintern symbols and unbind any functions stored in function name lists."
  (dolist (symbol symbols)
    (when (boundp symbol)
      ;; If this symbol holds a list of function names, unbind each function
      (let ((value (symbol-value symbol)))
        (when (and (listp value)
                   (every #'symbolp value))
          (dolist (fn-name value)
            (when (fboundp fn-name)
              (fmakunbound fn-name))))))
    (unintern symbol)))


;Reset certain user defined symbols, when defined on previous load.
(eval-when (:load-toplevel :execute)
  (reset-user-syms '(goal-fn constraint-fn heuristic? min-steps-remaining?
                     state-feasible? prune-state? bounding-function?)))


(defun reset-global-hash-tables ()
  "Reinitialize all global hash tables and reset global lists between problem loads.
   sb-ext:defglobal only evaluates initialization forms ONCE per image session.
   On subsequent ASDF reloads the variables remain bound to their previous values,
   causing state contamination between problem loads. This function must execute
   at top-level so it runs on every system reload.
   Two classes of table are handled differently:
     RECREATED  -- tables whose defglobal init form includes :synchronized (> *threads* 0);
                   setf'd to a fresh hash table so the flag reflects the current *threads*.
     CLEARED    -- tables with a fixed or absent :synchronized flag; clrhash suffices.
   This function executes AFTER read-init-vals has restored *threads* from
   vals.lisp, so that the recreated tables carry the correct :synchronized value."
  ;; CLEARED: fixed-size tables, write-only during init, lock-free during search
  (when (and (boundp '*types*) (hash-table-p *types*))
    (clrhash *types*))
  (when (and (boundp '*constant-integers*) (hash-table-p *constant-integers*))
    (clrhash *constant-integers*))
  (when (and (boundp '*symmetrics*) (hash-table-p *symmetrics*))
    (clrhash *symmetrics*))
  (when (and (boundp '*complements*) (hash-table-p *complements*))
    (clrhash *complements*))
  (when (and (boundp '*fluent-relation-indices*) (hash-table-p *fluent-relation-indices*))
    (clrhash *fluent-relation-indices*))
  (when (and (boundp '*bijective-relations*) (hash-table-p *bijective-relations*))
    (clrhash *bijective-relations*))
  (when (and (boundp '*bijective-canonical*) (hash-table-p *bijective-canonical*))
    (clrhash *bijective-canonical*))
  (when (and (boundp '*static-db*) (hash-table-p *static-db*))
    (clrhash *static-db*))
  (when (and (boundp '*hap-db*) (hash-table-p *hap-db*))
    (clrhash *hap-db*))
  (when (and (boundp '*hap-idb*) (hash-table-p *hap-idb*))
    (clrhash *hap-idb*))
  ;; RECREATED: tables whose :synchronized flag must reflect the current *threads* value
  (when (boundp '*relations*)
    (setf *relations* (make-hash-table :test #'eq :synchronized (> *threads* 0))))
  (when (boundp '*static-relations*)
    (setf *static-relations* (make-hash-table :test #'eq :synchronized (> *threads* 0))))
  (when (boundp '*db*)
    (setf *db* (make-hash-table :test #'equal :synchronized (> *threads* 0))))
  (when (boundp '*hdb*)
    (setf *hdb* (make-hash-table :test #'equal :synchronized (> *threads* 0))))
  (when (boundp '*idb*)
    (setf *idb* (make-hash-table :synchronized (> *threads* 0))))
  (when (boundp '*hidb*)
    (setf *hidb* (make-hash-table :synchronized (> *threads* 0))))
  (when (boundp '*integer-constants*)
    (setf *integer-constants* (make-hash-table :synchronized (> *threads* 0))))
  (when (boundp '*static-idb*)
    (setf *static-idb* (make-hash-table :synchronized (> *threads* 0))))
  (when (boundp '*prop-key-cache*)
    (setf *prop-key-cache* (make-hash-table :test #'equal :synchronized (> *threads* 0))))
  ;; Reset lists that accumulate problem definitions
  (when (and (boundp '*query-names*) (listp *query-names*))
    (setf *query-names* nil))
  (when (and (boundp '*update-names*) (listp *update-names*))
    (setf *update-names* nil))
  (when (and (boundp '*actions*) (listp *actions*))
    (setf *actions* nil))
  (when (and (boundp '*init-actions*) (listp *init-actions*))
    (setf *init-actions* nil))
  (when (and (boundp '*happening-names*) (listp *happening-names*))
    (setf *happening-names* nil))
  ;; Reset object index counter
  (when (and (boundp '*last-object-index*) (integerp *last-object-index*))
    (setf *last-object-index* 0)))


(defun read-init-vals (vals-file)
  "Load critical initialization parameters from vals.lisp if it exists.
   Sets *problem-name*, *algorithm*, *debug*, and *threads* for proper loading.
   Returns the problem-name string for eval-when path construction, or nil if file absent."
  (when (probe-file vals-file)
    (with-open-file (stream vals-file :direction :input)
      (let ((parameters (read stream)))
        (setf *problem-name* (first parameters)      ; position 0
              *algorithm* (third parameters)         ; position 2  
              *debug* (nth 11 parameters)            ; position 11
              *threads* (or (nth 13 parameters) 0))  ; position 13
        ;; Handle debug feature flag based on loaded value
        (if (> *debug* 0)
            (pushnew :ww-debug *features*)
            (setf *features* (remove :ww-debug *features*)))
        ;; Return problem-name string for eval-when path logic
        (string *problem-name*)))))


(eval-when (:load-toplevel :execute)
  (let* ((root (asdf:system-source-directory :wouldwork))
         (src-dir (merge-pathnames "src/" root))
         (problem-file (merge-pathnames "problem.lisp" src-dir))
         (vals-file (merge-pathnames "vals.lisp" root))
         (blocks3-file (merge-pathnames "problem-blocks3.lisp" src-dir))
         (vals-problem-name (read-init-vals vals-file))
         (vals-problem-file (merge-pathnames (concatenate 'string "problem-" vals-problem-name ".lisp") src-dir)))
    (cond ((not (probe-file problem-file))  ;no problem.lisp file?
             (copy-problem-with-tech-includes blocks3-file problem-file)  ;default problem.lisp
             (uiop:delete-file-if-exists vals-file))  ;rebuild in ww-initialize.lisp
          ((and (probe-file vals-file) (probe-file vals-problem-file))  ;CHANGED: vals.lisp names an existing source
             (copy-problem-with-tech-includes vals-problem-file problem-file))  ;make problem.lisp match vals.lisp
          (t  ;ADDED: vals.lisp absent or inconsistent -- recover source from problem.lisp's own header
             (uiop:delete-file-if-exists vals-file)  ;ADDED: discard any inconsistent vals.lisp
             (let ((header-source (snapshot-source-file problem-file)))  ;ADDED: provenance from snapshot header
               (when header-source  ;ADDED: re-splice from recovered source, else leave snapshot as-is
                 (copy-problem-with-tech-includes header-source problem-file)))))))


;; Call AFTER read-init-vals has restored *threads* from vals.lisp, so that
;; recreated :synchronized tables reflect the correct value for this session.
(eval-when (:load-toplevel :execute)
  (reset-global-hash-tables))
