;;; Filename: ww-preliminaries.lisp

;;; Initial setup functions & macros for wouldwork.


(in-package :ww)


#+sbcl 
(defparameter *lock* (bt:make-lock))  ;for thread protection


;#-sbcl
;(defparameter *global-locks* (make-hash-table))  ;Use for future non-sbcl multi-thread implementations


;#-sbcl
;(defun ensure-global-lock (var-name)
;  "Store one lock for each global variable. Use for future non-sbcl implementations."
;  (or (gethash var-name *global-locks*)
;      (setf (gethash var-name *global-locks*)
;            (bordeaux-threads:make-lock (format nil "Lock for ~S" var-name)))))


(defun cleanup-resources ()
  "Attempt to shutdown dangling threads safely in SBCL."
  #+sbcl
  (progn (format t "~&Cleaning up resources and shutting down threads...~%")
         (let ((current-thread sb-thread:*current-thread*))
           (dolist (thread (sb-thread:list-all-threads))
             (unless (eq thread current-thread)
               (when (sb-thread:thread-alive-p thread)
                 (format t "~&Terminating thread: ~A~%" thread)
                 (ignore-errors
                   (sb-thread:terminate-thread thread))))))
           (format t "~&Cleanup completed.~%"))
  #-sbcl
  nil)


#+sbcl
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
     #+sbcl
     ,(if (> *threads* 0)
          (if (boundp var-name)
              `(setf ,var-name ,val-form)
              `(sb-ext:defglobal ,var-name ,val-form ,doc-string))
          `(defparameter ,var-name ,val-form ,doc-string))
     #-sbcl
     (defparameter ,var-name ,val-form ,doc-string)))


(defmacro increment-global (var-name &optional (delta-form 1))
  `(progn
     (declaim (type fixnum ,var-name))
     #+sbcl
     ,(if (> *threads* 0)
        `(sb-ext:atomic-incf ,var-name ,delta-form)
        `(incf ,var-name ,delta-form))
     #-sbcl
     ,(if (> *threads* 0)
        (format t "~2%Sorry, multi-threading only supported in SBCL.
                   Please set *threads* to 0 in ww-settings.lisp~%")
        ;`(bordeaux-threads:with-lock-held ((gethash ',var-name *global-locks*))
        ;(incf ,var-name ,delta-form))
        `(incf ,var-name ,delta-form))))


(defmacro push-global (item var-name)
  `(progn
     (declaim (type list ,var-name))
     #+sbcl
     ,(if (> *threads* 0)
        `(sb-ext:atomic-push ,item ,var-name)
        `(push ,item ,var-name))
     #-sbcl
     ,(if (> *threads* 0)
        (format t "~2%Sorry, multi-threading only supported in SBCL.
                   Please set *threads* to 0 in ww-settings.lisp~%")
        ;`(bordeaux-threads:with-lock-held ((gethash ',var-name *global-locks*))
        ;(push ,item ,var-name))
        `(push ,item ,var-name))))


(defmacro pop-global (var-name)
  `(progn
     (declaim (type list ,var-name))
     #+sbcl
     ,(if (> *threads* 0)
        `(sb-ext:atomic-pop ,var-name)
        `(pop ,var-name))
     #-sbcl
     ,(if (> *threads* 0)
        (format t "~2%Sorry, multi-threading only supported in SBCL.
                   Please set *threads* to 0 in ww-settings.lisp~%")
        ;`(bordeaux-threads:with-lock-held ((gethash ',var-name *global-locks*))
        ;(pop ,var-name))
       `(pop ,var-name))))


(defun reset-globals (symbols)
  (dolist (symbol symbols)
    (unintern symbol)))


;Reset certain user defined functions, when defined on previous load.
(reset-globals '(goal-fn constraint-fn heuristic? prune? bounding-function?))


;Make sure proper problem.lisp exists before loading wouldwork
(let* ((root (asdf:system-source-directory :wouldwork))
       (src-dir (merge-pathnames "src/" root))
       (problem-file (merge-pathnames "problem.lisp" src-dir))
       (vals-file (merge-pathnames "vals.lisp" root))
       (blocks3-file (merge-pathnames "problem-blocks3.lisp" src-dir))
       (vals-problem-name (when (probe-file vals-file)
                            (with-open-file (in-file vals-file :direction :input)
                              (let ((first-item (read in-file nil nil)))
                                (and (symbolp first-item) (string first-item))))))
       (vals-problem-file (merge-pathnames (concatenate 'string "problem-" vals-problem-name ".lisp") src-dir)))
  (cond ((not (probe-file problem-file))  ;no problem.lisp file?
           (uiop:copy-file blocks3-file problem-file)  ;default problem.lisp
           (uiop:delete-file-if-exists vals-file))  ;rebuild in ww-initialize.lisp
        ((probe-file vals-file)  ;does vals.lisp exist?
           (if (probe-file vals-problem-file)  ;does problem-<vals-problem-name>.lisp exist?
             (uiop:copy-file vals-problem-file problem-file)  ;make sure problem.lisp corresponds with vals.lisp
             (delete-file vals-file)))))  ;vals.lisp inconsistent with problem.lisp
           