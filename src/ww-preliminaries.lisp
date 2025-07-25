;;; Filename: ww-preliminaries.lisp

;;; Initial setup functions & macros for wouldwork.


(in-package :ww)


(defmacro with-search-structures-lock (&body body)
  "Protects composite operations on *open* and *closed* search structures."
  `(bt:with-lock-held (*lock*)
     ,@body))


(defparameter *ww-loading* t
  "Flag to indicate if Wouldwork is currently being loaded. Reset in ww-initialize.lisp")


(defparameter *lock* (bt:make-lock))  ;for thread protection


(defmacro with-silenced-compilation (&body body)
  "Macro to suppress normal compilation output while preserving error reporting."
  `(let ((*compile-verbose* nil)
         (*compile-print* nil))
     ,@body))


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


(defmacro define-global (var-name val-form &optional doc-string)
  "Convenience for defining globals in ww-setting.lisp for single- or multi-threading operation."
  `(progn
     ,(if (> *threads* 0)
        (if (boundp var-name)
          `(setf ,var-name ,val-form)
          `(sb-ext:defglobal ,var-name ,val-form ,doc-string))
        `(defparameter ,var-name ,val-form ,doc-string))))


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
  (dolist (symbol symbols)
    (unintern symbol)))


;Reset certain user defined symbols, when defined on previous load.
(reset-user-syms '(goal-fn constraint-fn heuristic? prune? bounding-function? *actions*))


(eval-when (:load-toplevel :execute)
  (let* ((root (asdf:system-source-directory :wouldwork))
         (src-dir (merge-pathnames "src/" root))
         (problem-file (merge-pathnames "problem.lisp" src-dir))
         (vals-file (merge-pathnames "vals.lisp" root))
         (blocks3-file (merge-pathnames "problem-blocks3.lisp" src-dir))
         (vals-problem-name (when (probe-file vals-file)
                              (with-open-file (in-file vals-file :direction :input)
                                (let* ((parameters (read in-file nil nil))
                                       (problem-name (string (first parameters)))
                                       (debug (tenth parameters)))
                                  (when (> debug 0)
                                    (pushnew :ww-debug *features*))
                                  problem-name))))
         (vals-problem-file (merge-pathnames (concatenate 'string "problem-" vals-problem-name ".lisp") src-dir)))
    (cond ((not (probe-file problem-file))  ;no problem.lisp file?
             (uiop:copy-file blocks3-file problem-file)  ;default problem.lisp
             (uiop:delete-file-if-exists vals-file))  ;rebuild in ww-initialize.lisp
          ((probe-file vals-file)  ;does vals.lisp exist?
             (if (probe-file vals-problem-file)  ;does problem-<vals-problem-name>.lisp exist?
               (uiop:copy-file vals-problem-file problem-file)  ;make sure problem.lisp corresponds with vals.lisp
               (delete-file vals-file)))  ;vals.lisp inconsistent with problem.lisp
          ((not (probe-file problem-file))
             (uiop:copy-file blocks3-file problem-file)))))

           