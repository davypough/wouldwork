;;; Filename: ww-problem-lifecycle.lisp

;;; Lifecycle support for problem-owned Lisp functions.


(in-package :ww)


(defun register-problem-function (function-name)
  "Register a problem-owned Lisp function for restaging cleanup."
  (unless (and (symbolp function-name)
               (fboundp function-name))
    (error "Problem function must name a defined function: ~S" function-name))
  (pushnew function-name *problem-function-names* :test #'eq)
  function-name)


(defmacro define-problem-helper (name lambda-list &body body)
  "Define a problem-owned Lisp helper and register it for restaging cleanup."
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (register-problem-function ',name)))
