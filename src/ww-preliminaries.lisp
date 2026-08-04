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
(defparameter *solution-validation-lock*
  (bt:make-lock "ww-solution-validation-lock"))


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

(defvar *tech-inclusion-trace* nil
  "Accumulates technology-inclusion and missing-technology notices, in reverse
   order, from the most recent call to copy-problem-with-tech-includes -- reset at
   the start of every such call, regardless of whether that call ends up rewriting
   problem.lisp.  Printed by ensure-problem-staged immediately after splicing
   completes, before the reload/compile that follows begins -- see
   *staging-trace-already-shown* for how the two ensure-problem-staged passes
   within one command avoid printing it twice.")


(defvar *included-tech-names* nil
  "The bare names, as strings, of the technologies the problem file itself included on the
   most recent call to copy-problem-with-tech-includes, in reverse order.  Nested
   technologies are deliberately absent, which is the whole difference between this list
   and *tech-inclusion-trace*: the trace records every splice, because its job is to show
   what the staged problem.lisp is made of.

   Read by report-inert-techs, whose only useful advice is to drop an include.  Naming a
   technology the author never wrote and cannot remove would be advice with nothing behind
   it.  A receiver-free blower problem is the case that forced the distinction:
   -beam-substrate contributes only update-receiver-status!, which quantifies solely over
   an empty receiver type and so is genuinely inert there -- but it arrives through
   -controls and -gears-fan beneath floor-blower and wall-blower, several levels below
   anything the problem wrote.  The technology worth naming in such a report is always the
   one the problem included: were beam-relay included in a connector-free problem,
   beam-relay itself is inert and is reported.")


(defvar *spliced-tech-names* nil
  "The bare names, as strings, of every technology spliced into problem.lisp on the most
   recent call to copy-problem-with-tech-includes, in reverse order.  Nested technologies
   are present, which is the whole difference between this list and *included-tech-names*.

   Read by driver-candidate-updates, which needs the opposite of what report-inert-techs
   needs.  update-gears-status! and update-receiver-status! reach a blower problem only
   through -gears-fan and -beam-substrate, several levels below anything the problem wrote,
   yet both belong in its propagation driver.  A candidate set built from
   *included-tech-names* would silently omit them.

   Deduplicated by construction: write-tech-include splices each technology at most once
   per problem copy, and pushes only where it splices, so -beam-substrate appears once
   however many peers nest it.")


(defun read-file-string (path)
  "Return the complete contents of the file at PATH as a string."
  (with-open-file (in path :direction :input)
    (let* ((length (file-length in))
           (buffer (make-string length)))
      (subseq buffer 0 (read-sequence buffer in)))))


(defun copy-problem-with-tech-includes (source-file target-file)
  "Copy SOURCE-FILE to TARGET-FILE, expanding each (include-tech NAME) directive in
   place by splicing tech/NAME-tech.lisp.  Included tech files may include other
   tech files the same way.  Each technology is spliced at most once per problem
   copy.  This lets a problem compose itself from self-contained technology files
   before the pre-scan reads problem.lisp.
   Content-addressed: the full spliced content is computed first and compared
   against TARGET-FILE's current content; the file is only overwritten when the
   two differ, so any number of callers may invoke it freely without needing to
   coordinate with one another.
   Resets *tech-inclusion-trace* to reflect this call's computation regardless of
   whether the file ends up rewritten; printing that trace is init's job, not this
   function's -- see *tech-inclusion-trace*."
  (setf *tech-inclusion-trace* nil)
  (setf *included-tech-names* nil)
  (setf *spliced-tech-names* nil)
  (let* ((included-techs (make-hash-table :test #'equal))
         (new-content (with-output-to-string (out)
                        (with-open-file (in source-file :direction :input)
                          (loop for line = (read-line in nil nil)
                                while line
                                do (let ((tech-name (include-tech-directive line)))
                                     (if tech-name
                                       (write-tech-include tech-name out nil included-techs)
                                       (write-line line out)))))))
         (unchanged (and (probe-file target-file)
                         (string= new-content (read-file-string target-file)))))
    (unless unchanged
      (with-open-file (out target-file :direction :output :if-exists :supersede)
        (write-string new-content out)))))

(defun write-tech-include (tech-name-str out include-stack included-techs)
  "Splice the capability file for TECH-NAME-STR into stream OUT, or note its absence.
   A missing tech file is replaced by a comment and a console notice, so a problem may
   be staged before all of its technologies have been written.

   INCLUDE-STACK carries the chain of technologies currently being expanded, which detects
   a circular include.  It is empty exactly when the directive came from the problem file
   rather than from another technology, so it also decides what reaches
   *included-tech-names* -- see that variable for why nested technologies are excluded."
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
             (progn (check-tech-file-syntax tech-name-str tech-file)
                    (setf (gethash tech-name-str included-techs) t)
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
                    (push (format nil "~&  included technology: ~A~%" tech-name-str)
                          *tech-inclusion-trace*)
                    (push tech-name-str *spliced-tech-names*)
                    (when (null include-stack)
                      (push tech-name-str *included-tech-names*)))
             (progn (format out ";; (include-tech ~A): tech/~A.lisp not found -- skipped~%"
                            tech-name-str tech-name-str)
                    (push (format nil "~&  MISSING technology, skipped: ~A~%" tech-name-str)
                          *tech-inclusion-trace*)))))))


(defun check-tech-file-syntax (tech-name-str tech-file)
  "Read every top-level form in TECH-FILE with the standard reader before splicing it
   into problem.lisp.  A malformed tech file -- most commonly an unbalanced parenthesis
   -- is caught and blamed on TECH-NAME-STR and TECH-FILE right here, instead of
   surfacing later as an uninformative end-of-file error while SBCL compiles the merged
   problem.lisp.  The trace of technologies already spliced successfully is flushed
   first, so the halt point is visible before the error is signaled."
  (let ((text (read-file-string tech-file))
        (forms nil))
    (with-input-from-string (in text)
      (handler-case (setf forms (loop for form = (read in nil in)
                                      until (eq form in)
                                      collect form))
        (error ()
          (dolist (message (reverse *tech-inclusion-trace*)) (write-string message))
          (error "Technology tech/~A.lisp (~A) failed to read cleanly -- check for ~
                  an unbalanced parenthesis.  Splicing halted here."
                 tech-name-str tech-file))))
    (check-tech-file-form-order tech-name-str forms)))


(defun check-tech-file-form-order (tech-name-str forms)
  "Errors when a definition form precedes an (include-tech ...) directive in a technology
   file.

   Splice order is the seed the derived propagation driver orders its updates by, and splice
   order is a depth-first walk of the include directives in the order they appear.  A
   directive sitting below a DEFINE-UPDATE therefore silently changes which update reaches
   the driver first -- a reordering with no syntactic symptom, in a file that still reads,
   compiles and runs.  Every technology already places its directives above its definitions;
   this makes that an invariant rather than a habit.

   Read with whatever *PACKAGE* is current, so the test is on symbol names rather than on
   symbol identity."
  (let ((first-definition nil))
    (dolist (form forms)
      (let ((head (tech-form-head-name form)))
        (when (and (null first-definition)
                   (eql 0 (search "DEFINE-" head)))
          (setf first-definition head))
        (when (and first-definition
                   (string= head "INCLUDE-TECH"))
          (error "Technology tech/~A.lisp places an (include-tech ...) directive below ~
                  ~(~A~).~2%~
                  Splice order seeds the derived propagation driver, so a directive under a ~
                  definition reorders the updates reaching that driver without any other ~
                  symptom.  Move every (include-tech ...) directive above the file's ~
                  definitions."
                 tech-name-str first-definition))))))


(defun tech-form-head-name (form)
  "The symbol name of FORM's head, or the empty string when FORM has no symbol in head
   position."
  (if (and (consp form) (symbolp (car form)))
    (symbol-name (car form))
    ""))


(defun include-tech-directive (line)
  "If LINE is an (include-tech NAME) directive, return NAME as a string, else NIL."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Linefeed) line))
        (head "(include-tech "))
    (when (string-prefix-p head trimmed)
      (let* ((rest-str (subseq trimmed (length head)))
             (name-end (position-if (lambda (ch) (member ch '(#\Space #\Tab #\))))
                                    rest-str)))
        (when name-end
          (check-include-tech-line trimmed rest-str name-end)
          (subseq rest-str 0 name-end))))))


(defun check-include-tech-line (trimmed rest-str name-end)
  "Errors when anything but whitespace or a trailing comment follows an (include-tech ...)
   directive on its own line.  COPY-PROBLEM-WITH-TECH-INCLUDES substitutes the spliced
   technology for the whole line, so code sharing that line is discarded -- and nothing
   downstream notices, because the resulting problem.lisp still reads, compiles, and runs.
   A directive tucked onto the end of a DEFINE-UPDATE body deletes the form beside it and
   splices a tech file into the middle of that body, where its DEFINE-UPDATEs stop being
   top-level forms and install on first call instead of at load.  Every symptom of that is
   downstream of the deletion, so the error belongs here, at the point of loss."
  (let ((close (position #\) rest-str :start name-end)))
    (unless close
      (error "Malformed technology directive -- no closing parenthesis:~%  ~A" trimmed))
    (let ((tail (string-trim '(#\Space #\Tab) (subseq rest-str (1+ close)))))
      (unless (or (zerop (length tail))
                  (char= (char tail 0) #\;))
        (error "Code follows a technology directive on the same line:~%  ~A~2%~
                Splicing replaces the entire line, so~%  ~A~%would be discarded without ~
                warning.  Put the directive on a line of its own."
               trimmed tail)))))

(defun tech-file-path (tech-name-str)
  "Resolve tech/<TECH-NAME>.lisp below the Wouldwork root, or NIL if absent."
  (let ((relative (make-pathname :directory '(:relative "tech")
                                 :name tech-name-str
                                 :type "lisp"))
        (root (asdf:system-source-directory :wouldwork)))
    (probe-file (merge-pathnames relative root))))


(defun problem-source-file (filename)
  "Resolve FILENAME (eg, \"problem-blocks3.lisp\") in the standard problem folders
   below the Wouldwork root -- probs/ first, then test/ -- or NIL if absent from
   both.  Deliberately independent of ww-interface's user-extensible
   *problem-folder-paths*, since this runs during the reload-time eval-when
   before ww-interface loads."
  (let ((root (asdf:system-source-directory :wouldwork)))
    (or (probe-file (merge-pathnames filename (merge-pathnames "probs/" root)))
        (probe-file (merge-pathnames filename (merge-pathnames "test/" root))))))


(defun snapshot-source-file (snapshot-file)
  "Return the source problem file recorded in SNAPSHOT-FILE's leading Filename
   header (eg, ';;; Filename: problem-claustro4.lisp'), resolved in the standard
   problem folders (probs/, test/), or NIL if the header is absent or the named
   file does not exist.  This lets the loader recover a snapshot's provenance
   without consulting vals.lisp."
  (let* ((marker ";;; Filename:")
         (first-line (with-open-file (in snapshot-file :direction :input)
                       (read-line in nil nil)))
         (trimmed (and first-line (string-trim '(#\Space #\Tab #\Return) first-line))))
    (when (and trimmed (string-prefix-p marker trimmed))
      (problem-source-file (string-trim '(#\Space #\Tab #\Return)
                                        (subseq trimmed (length marker)))))))


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
  (when (and (boundp '*type-signatures*) (hash-table-p *type-signatures*))
    (clrhash *type-signatures*))
  (when (and (boundp '*type-components*) (hash-table-p *type-components*))
    (clrhash *type-components*))
  (when (and (boundp '*derived-relations*) (hash-table-p *derived-relations*))
    (clrhash *derived-relations*))
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
  ;; Remove compiled functions and translation metadata owned by the previous problem.
  ;; Otherwise an omitted technology can leave a stale query/update looking like an
  ;; ordinary Lisp function during translation.
  (let ((old-function-names
          (append (when (and (boundp '*query-names*) (listp *query-names*))
                    (copy-list *query-names*))
                  (when (and (boundp '*update-names*) (listp *update-names*))
                    (copy-list *update-names*))
                  (when (and (boundp '*problem-function-names*)
                             (listp *problem-function-names*))
                    (copy-list *problem-function-names*))
                  ;; Clean functions registered by the pre-Stage-4 lifecycle once
                  ;; when this revision is first reloaded into an existing image.
                  (when (and (boundp '*init-check-function-names*)
                             (listp
                               (symbol-value '*init-check-function-names*)))
                    (copy-list
                      (symbol-value '*init-check-function-names*))))))
    (dolist (function-name (delete-duplicates old-function-names))
      (when (fboundp function-name)
        (fmakunbound function-name))
      (when (boundp function-name)
        (makunbound function-name))
      (remprop function-name :raw-body)
      (remprop function-name :raw-args)
      (remprop function-name :param-types)
      (remprop function-name :init-check-consumed-types)))
  ;; Reset lists that accumulate problem definitions
  (when (and (boundp '*query-names*) (listp *query-names*))
    (setf *query-names* nil))
  (when (and (boundp '*update-names*) (listp *update-names*))
    (setf *update-names* nil))
  (when (and (boundp '*actions*) (listp *actions*))
    (setf *actions* nil))
  (when (and (boundp '*init-actions*) (listp *init-actions*))
    (setf *init-actions* nil))
  (when (and (boundp '*init-checks*) (listp *init-checks*))
    (setf *init-checks* nil))
  (when (and (boundp '*problem-function-names*)
             (listp *problem-function-names*))
    (setf *problem-function-names* nil))
  (when (boundp '*init-check-function-names*)
    (makunbound '*init-check-function-names*))
  (when (and (boundp '*test-claims*) (listp *test-claims*))
    (setf *test-claims* nil))
  (when (and (boundp '*test-mutations*) (listp *test-mutations*))
    (setf *test-mutations* nil))
  (when (and (boundp '*happening-names*) (listp *happening-names*))
    (setf *happening-names* nil))
  (when (and (boundp '*solution-report-printers*)
             (listp *solution-report-printers*))
    (setf *solution-report-printers* nil))
  (when (and (boundp '*solution-validators*)
             (listp *solution-validators*))
    (setf *solution-validators* nil))
  (when (and (boundp '*goal-chaining-checkpoint-extensions*)
             (listp *goal-chaining-checkpoint-extensions*))
    (setf *goal-chaining-checkpoint-extensions* nil))
  (when (boundp '*goal-chaining-policy*)
    (setf *goal-chaining-policy* nil))
  (when (boundp '*solutions-valid*)
    (setf *solutions-valid* nil))
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


(defvar *staging-trace-already-shown* nil
  "T immediately after the explicit-name branch of ensure-problem-staged has
   printed *tech-inclusion-trace*, so the reload it's about to trigger via
   (asdf:load-system ...) doesn't print the identical trace again when its own
   autodetect pass -- which recomputes the same content -- runs moments later
   within the same command.  Cleared by the very next autodetect pass regardless
   of whether it actually skipped a print, so it can never linger stale into
   some later, unrelated command.")


(defun ensure-problem-staged (&optional problem-name-designator)
  "The single point of decision for what src/problem.lisp's content should currently
   be, and the single point of action for making it so, via copy-problem-with-tech-includes
   (a harmless no-op when the computed content is already current).  Every entry point
   that can trigger a (re)splice -- load-problem below, this file's own reload-time
   eval-when, and the cl-user recovery refresh in ww-packages.lisp -- delegates here,
   so none of them needs to coordinate with the others.
   With PROBLEM-NAME-DESIGNATOR (a string or symbol naming a registered problem or a
   project-relative path): resolves it via resolve-problem-file, splices that file
   into problem.lisp, and prints the inclusion trace immediately -- before the
   caller's subsequent reload/compile begins, so a failure during that reload is
   easy to correlate with what was just spliced in.  Returns the resolved
   problem-file pathname, or NIL if it could not be resolved.
   With no argument: autodetects the authoritative source for an unattended reload --
   problem.lisp's absence defaults to blocks3; otherwise vals.lisp, if it names an
   existing source, takes precedence; failing that, problem.lisp's own snapshot header
   is consulted.  Prints the inclusion trace only if this pass actually spliced
   something and the explicit-name branch didn't already show the identical content
   moments ago -- see *staging-trace-already-shown*.  Always returns NIL in this case."
  (if problem-name-designator
    (let* ((root (asdf:system-source-directory :wouldwork))
           (target-file (merge-pathnames "problem.lisp" (merge-pathnames "src/" root)))
           (problem-file (resolve-problem-file (string problem-name-designator))))
      (when problem-file
        (copy-problem-with-tech-includes problem-file target-file)
        (dolist (message (reverse *tech-inclusion-trace*))
          (write-string message))
        (setf *staging-trace-already-shown* t))
      problem-file)
    (let* ((root (asdf:system-source-directory :wouldwork))
           (src-dir (merge-pathnames "src/" root))
           (problem-file (merge-pathnames "problem.lisp" src-dir))
           (vals-file (merge-pathnames "vals.lisp" root))
           (blocks3-file (problem-source-file "problem-blocks3.lisp"))
           (vals-problem-name (read-init-vals vals-file))
           (vals-problem-file (problem-source-file
                                (concatenate 'string "problem-" vals-problem-name ".lisp")))
           (spliced nil))
      (cond ((not (probe-file problem-file))  ;no problem.lisp file?
              (copy-problem-with-tech-includes blocks3-file problem-file)  ;default problem.lisp
              (setf spliced t)
              (uiop:delete-file-if-exists vals-file))  ;rebuild in ww-initialize.lisp
            ((and (probe-file vals-file) vals-problem-file)  ;vals.lisp names an existing source
              (copy-problem-with-tech-includes vals-problem-file problem-file)  ;make problem.lisp match vals.lisp
              (setf spliced t))
            (t  ;vals.lisp absent or inconsistent -- recover source from problem.lisp's own header
              (uiop:delete-file-if-exists vals-file)  ;discard any inconsistent vals.lisp
              (let ((header-source (snapshot-source-file problem-file)))  ;provenance from snapshot header
                (when header-source  ;re-splice from recovered source, else leave snapshot as-is
                  (copy-problem-with-tech-includes header-source problem-file)
                  (setf spliced t)))))
      (cond (*staging-trace-already-shown*
              (setf *staging-trace-already-shown* nil))
            (spliced
              (dolist (message (reverse *tech-inclusion-trace*))
                (write-string message))))
      nil)))


(eval-when (:load-toplevel :execute)
  (ensure-problem-staged))


;; Call AFTER read-init-vals has restored *threads* from vals.lisp, so that
;; recreated :synchronized tables reflect the correct value for this session.
(eval-when (:load-toplevel :execute)
  (reset-global-hash-tables))
