;;; Filename: ww-translator.lisp

;;; Translates a domain file containing formulas into lisp.


(in-package :ww)


(defun merge-idb-hidb (state)
  "Merges the two databases of state."
  (let ((idb (alexandria:copy-hash-table (problem-state.idb state))))
    (maphash (lambda (key val)
               (setf (gethash key idb) val))
             (problem-state.hidb state))
    idb))


(defun translate-list (form flag)
  "Most basic form translation."
  (declare (ignore flag))
  (check-proposition form)
  `(list ,@(iter (for item in form)
             (if (or (varp item)
                     (numberp item)
                     (stringp item)
                     (characterp item)
                     (listp item))
               (collect item)
               (collect `(quote ,item))))))


(defun translate-simple-atom (form flag)
  "Pre/ante only.
   Eg, (velocity ?car wheel1 50) -> (list 'velocity ?car 'wheel1 50) with no fluents"
  `(eql t (gethash ,(translate-list form flag)
                      ,(if (gethash (car form) *relations*)
                         (if *happening-names*
                           '(merge-db-hdb state)  ;dummy function replaced by merge-idb-hidb
                           (case flag
                             (pre '(problem-state.db state))
                             ((ante eff) 'idb)))
                  '*static-db*))))


(defun translate-fluent-atom (form flag)
  "Pre/ante only."
  (let* ((fluent-indices (get-prop-fluent-indices form))
         (fluentless-atom (ut::remove-at-indexes fluent-indices form))
         (fluents (ut::collect-at-indexes fluent-indices form)))  ;eg, area1, ?area1, or $area1
    `(equalp (gethash ,(translate-list fluentless-atom flag) ,(if (gethash (car form) *relations*)
                                                               (case flag
                                                                 (pre '(problem-state.db state))
                                                                 ((ante eff) 'idb))
                                                               '*static-db*))
             (list ,@(mapcar (lambda (x)
                               (if (or (varp x)
                                       (and (consp x)
                                         (symbolp (car x))
                                         (or (fboundp (car x))
                                             (special-operator-p (car x)))))
                                  x
                                  `',x))
                               ;(if (symbolp x) `',x x))  ;(if (varp x) x `',x))
                             fluents)))))


(defun translate-proposition (form flag)
  "Distinguishes fluent from non-fluent propositions."
  (check-proposition form)
  (if (get-prop-fluent-indices form)
    (translate-fluent-atom form flag)
    (translate-simple-atom form flag)))


(defun translate-positive-relation (form flag)
  "Distinguishes between pre/ante and eff relations. General form consisting of
   the relation, ?vars, $vars, numbers, symbols, lisp objects, and functions.
   Eg, (loc me ?area) -> (gethash (list 'loc 'me ?area) (problem-state.db state))  ;pre/ante
                      -> (update idb (list 'loc 'me ?area))  ;eff"
  (ecase flag
    ((pre ante) (translate-proposition form flag))
    (eff `(update idb ,(translate-list form flag)))))


(defun translate-negative-relation (form flag)
  "Translates a negated relation form."
    (ecase flag
      ((pre ante) `(not ,(translate-positive-relation (second form) flag)))
      (eff `(update idb (list 'not ,(translate-list (second form) flag))))))


(defun translate-function-call (form flag)
  "Form consists only of the function name & vars.
   Eg, (elevation? ?support)  ;pre
       (toggle! 'grave1 'grave2 'grave3)  ;eff"
  (check-query/update-call form)
  (let ((fn-call (concatenate 'list (list (car form))
                                    (case flag
                                      ((pre ante) '(state))
                                      (eff (if (member (car form) *query-names*)
                                             '(state)
                                             '(state idb))))
                                    (mapcar (lambda (arg)
                                              (if (and (symbolp arg)
                                                       (not (varp arg)))
                                                `(quote ,arg)
                                                arg))
                                            (cdr form)))))
     `,fn-call))


(defun get-prop-fluents (proposition)
  "Returns the fluent values in an arbitrary proposition."
  (let ((indices (get-prop-fluent-indices proposition)))
    (when indices
      (mapcar (lambda (index)
                (let ((item (nth index proposition)))
                  (if (and (symbolp item) (boundp item))
                    (symbol-value item)
                    item)))
              indices))))


(defun translate-bind (form flag)
  "Translates a binding for a relation form, returns t if there is a binding,
   even NIL. But returns NIL if there is no binding."
  (check-proposition (second form))
  (let* ((fluent-indices (get-prop-fluent-indices (second form)))
         (fluentless-atom (ut::remove-at-indexes fluent-indices (second form)))
         (prop-fluents (get-prop-fluents (second form))))
    `(let ((vals (gethash ,(translate-list fluentless-atom flag)
                         ,(if (gethash (car (second form)) *relations*)
                            (case flag
                              (pre '(problem-state.db state))
                              ((ante eff) 'idb))
                            '*static-db*))))
       (when vals
         (progn (setf ,@(mapcan #'list prop-fluents 
                               (loop for i from 0 below (length prop-fluents)
                                    collect ;`(nth ,i vals))))
                                            (case i
                                              (0 '(first vals))
                                              (1 '(second vals)) 
                                              (2 '(third vals))
                                              (3 '(fourth vals))
                                              (otherwise `(nth ,i vals))))))
                t)))))



(defun translate-existential (form flag)
  "The existential is interpreted differently for pre vs eff.  
   In pre, (exists (vars) form) is true when form is true
   for any instantiation of vars, otherwise false. In eff, it asserts form
   for the first true instantiation of the vars, and then exits."
  (check-form-body form)
  (let ((parameters (second form))
        (body (third form)))
    (check-precondition-parameters parameters)
    (unless (member (first parameters) *parameter-headers*)
      (push 'standard parameters))
    (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params parameters)
      (let ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
            (type-inst (instantiate-type-spec pre-param-types)))
        `(apply #'some (lambda (&rest args)
                         (destructuring-bind ,pre-param-?vars args
                           ,(translate body flag)))
                ,(if queries
                   `(ut::transpose (eval-instantiated-spec ',type-inst state))
                   `(ut::transpose (quote ,(eval-instantiated-spec type-inst)))))))))


(defun translate-universal (form flag)
  "The universal is interpreted differently for pre vs eff.  
   In pre, (forall (vars) form) is true when form is always true, otherwise false.
   In eff, it asserts form for every instantiation of the vars."
  (check-form-body form)
  (let ((parameters (second form))
        (body (third form)))
    (check-precondition-parameters parameters)
    (unless (member (first parameters) *parameter-headers*)
      (push 'standard parameters))
    (when (eql flag 'eff)
      (warn "Found FORALL statement in effect; DOALL is often intended: ~A" form))
    (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params parameters)
      (let ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
            (type-inst (instantiate-type-spec pre-param-types)))
        `(apply #'every (lambda (&rest args)
                         (destructuring-bind ,pre-param-?vars args
                           ,(translate body flag)))
                ,(if queries
                   `(ut::transpose (eval-instantiated-spec ',type-inst state))
                   `(ut::transpose (quote ,(eval-instantiated-spec type-inst)))))))))


(defun translate-doall (form flag)
  "The doall form is a generator for all its variable instances. It always returns true. 
   It can be used to do something for all instances of its variables."
  (check-form-body form)
  (let ((parameters (second form))
        (body (third form)))
    (check-precondition-parameters parameters)
    (unless (member (first parameters) *parameter-headers*)
      (push 'standard parameters))
    (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params parameters)
      (let ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
            (type-inst (instantiate-type-spec pre-param-types)))
        `(apply #'mapc (lambda (&rest args)
                         (destructuring-bind ,pre-param-?vars args
                           ,(translate body flag)))
                ,(if queries
                   `(ut::transpose (eval-instantiated-spec ',type-inst state))
                   `(ut::transpose (quote ,(eval-instantiated-spec type-inst)))))))))


(defun translate-connective (form flag)
  "Translates and, or statements."
  (ecase flag
    ((pre ante eff)  `(,(car form) ,@(iter (for item in (cdr form))
                                   (collect (translate item flag)))))))


(defun translate-conditional (form flag)
  "Returns t if condition satisfied or nil if condition not satisfied."
  (when (or (and (third form) (listp (third form)) (eql (car (third form)) 'and))
            (and (fourth form) (listp (fourth form)) (eql (car (fourth form)) 'and)))
    (error "AND not allowed in <then> or <else> clause of IF statement; use DO: ~A"
           form))
  (if (fourth form)  ;else clause
    `(if ,(translate (second form) (if (eql flag 'eff) 'ante flag))
       ,(translate (third form) flag)
       ,(translate (fourth form) flag))
    `(when ,(translate (second form) (if (eql flag 'eff) 'ante flag))
       ,(translate (third form) flag))))


(defun translate-assert (form flag)
  "Translates an assert relation."
  (ecase flag
    (eff (error "Nested ASSERT statements not allowed:~%~A" form))
    (pre `(let ((idb (copy-idb (problem-state.idb state))))
            ,@(iter (for statement in (cdr form))
                    (collect (translate statement 'eff)))
                    (push (make-update :changes idb   ;:changes changes
                                       :value ,(if *objective-value-p*
                                                 '$objective-value
                                                 0.0)
                                       :instantiations (list ,@*eff-param-vars*)
                                       :followups (nreverse followups))
                          updated-dbs)))))


(defun translate-do (form flag)
  "Translates a simple set of clauses."
  `(progn ,@(iter (for statement in (cdr form))
              (collect (translate statement flag)))))


(defun translate-equivalent (form flag)
  "Translates an equivalent statement of logical statements--ie, if the truth values are all the same."
  `(equivalent ,@(iter (for statement in (cdr form))
                   (collect (translate statement flag)))))


(defun translate-let (form flag)
  "Translates a let clause, including binding forms."
  `(let ,(mapcar (lambda (binding)
                   (if (consp binding)
                       ;; Binding with initial value - translate the value
                       `(,(first binding) ,(translate (second binding) flag))
                       ;; Just a variable name - keep as is
                       binding))
                 (second form))
     ,@(iter (for statement in (cddr form))
             (collect (translate statement flag)))))


#+ignore (defun translate-let (form flag)
  "Translates a let clause."
  `(let ,(second form)
     ,@(iter (for statement in (cddr form))
             (collect (translate statement flag)))))


(defun translate-mvsetq (form flag)
  "Translates a multiple-value-setq clause."
  `(multiple-value-setq ,(second form) ,(translate (third form) flag)))


(defun translate-setq (form flag)
  "Translates a setq statement. Used to assign a variable the value of a function.
   Always returns t, even if nil is assigned."
  `(progn (setq ,(second form) ,(translate (third form) flag))
          t))


(defun translate-case (form flag)
  "Translates a case statement."
  `(case ,(second form)
     ,@(iter (for clause in (cddr form))
         (collect `(,(first clause) ,@(iter (for statement in (rest clause))
                                            (collect (translate statement flag))))))))


;(defun translate-cond (form flag)
;  "Translates a cond statement."
;  `(cond ,@(iter (for clause in cddr form))
;             (collect `(,(first clause) ,@(iter (for statement in (rest clause))
;                                            (collect (translate statement flag)))))))

(defun translate-print (form flag)
  "Translates a print statement for debugging actions."
  `(print ,(translate (second form) flag)))


(defun translate-ww-loop (form flag)
  "Translates a ww-loop into a lisp loop with interpreted ww forms."
  `(loop ,@(loop for item in (cdr form) collect (translate item flag))))


(defun translate-followup (form flag)
  ;Processes a trigger followup form for next & finally.
  (declare (ignore flag))
  (let ((base-form (second form)))
    `(push (list ',(car base-form) ,@(cdr base-form)) followups)))


(defun translate (form flag)  ;test-then distinguishes between if stmt forms
  "Beginning translator for all forms in actions."
  (cond ((atom form) form)  ;atom or (always-true) translates as itself
        ((null form) t)  ;if form=nil simply continue processing
        ((equal form '(always-true)) (translate-simple-atom form flag))
        ((eql (car form) 'assert) (translate-assert form flag))
        ((member (car form) '(forsome exists exist)) (translate-existential form flag))  ;specialty first
        ((member (car form) '(forall forevery)) (translate-universal form flag)) ;removed every
        ((member (car form) '(finally next)) (translate-followup form flag))
        ((eql (car form) 'doall) (translate-doall form flag))
        ((eql (car form) 'if) (translate-conditional form flag))
        ((eql (car form) 'do) (translate-do form flag))
        ((eql (car form) 'equivalent) (translate-equivalent form flag))
        ((eql (car form) 'bind) (translate-bind form flag))
        ((eql (car form) 'ww-loop) (translate-ww-loop form flag))
        ((eql (car form) 'setq) (translate-setq form flag))
        ((eql (car form) 'let) (translate-let form flag))
        ((eql (car form) 'case) (translate-case form flag))
        ((member (car form) '(mvsetq multiple-value-setq)) (translate-mvsetq form flag))
        ((eql (car form) 'declare) form)
        ((eql (car form) 'print) (translate-print form flag))
        ;((eql (car form) 'cancel-assert) (translate-cancel-assert form flag))
        ((eql (char (format nil "~S" form) 0) #\`) (translate (eval form) flag))
        ;((eql (car form) #+sbcl 'sb-int:quasiquote #+allegro 'excl::backquote) (translate (eval form) flag))
        ((and (eql (car form) 'not) (gethash (caadr form) *relations*)) (translate-negative-relation form flag))
        ((member (car form) *connectives*) (translate-connective form flag))
        ((or (gethash (car form) *relations*) (gethash (car form) *static-relations*))
           (translate-positive-relation form flag))
        ((member (car form) (append *query-names* *update-names*)) (translate-function-call form flag))
        ;((or (fboundp (car form)) (special-operator-p (car form)) form)   ;any lisp function
        (t form)))
