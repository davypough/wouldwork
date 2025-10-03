;;; Filename:  ww-converter.lisp

;;; Procedures for converting database hashtable lookups from symbols to integers


(in-package :ww)


(defun do-integer-conversion ()
  "Convert all objects to integers & put in idatabases."
  (format t "~&Optimizing lambda expressions and compiling...")
  (clrhash *prop-key-cache*)
  (associate-objects-with-integers)
  (iter (for (type constants) in-hashtable *types*)
    (iter (for constant in constants)
      (when (or (symbolp constant) (realp constant) (characterp constant))
        (setf (gethash (convert-to-integer-memoized (list type constant)) *static-idb*) t))))
  (iter (for (prop-key value) in-hashtable *db*)
        (for iproposition = (convert-to-integer-memoized prop-key))
        (setf (gethash iproposition *idb*) value)
        (setf (gethash iproposition (problem-state.idb *start-state*)) value))
  (iter (for (prop-key value) in-hashtable *hdb*)
        (for iproposition = (convert-to-integer-memoized prop-key))
        (setf (gethash iproposition *hidb*) value)
        (setf (gethash iproposition (problem-state.hidb *start-state*)) value))
  (iter (for (prop-key value) in-hashtable *static-db*)
        (for iproposition = (convert-to-integer-memoized prop-key))
        (setf (gethash iproposition *static-idb*) value))
  (iter (for (prop-key value) in-hashtable *hap-db*)
        (for iproposition = (convert-to-integer-memoized prop-key))
        (setf (gethash iproposition *hap-idb*) value)
        (setf (gethash iproposition (problem-state.hidb *start-state*)) value))
  (iter (for action in *actions*)
        (format t "~&  ~A...~%" (action.name action))
        (finish-output)
        (with-slots (pre-defun-name eff-defun-name precondition-lambda effect-lambda) action
          (compile pre-defun-name (subst-int-code precondition-lambda))
          (compile eff-defun-name (subst-int-code effect-lambda))))
        ;(setf (action.iprecondition-lambda action)
        ;  (subst-int-code (copy-tree (action.precondition-lambda action)))))
        ;(setf (action.ieffect-lambda action)
        ;  (subst-int-code (copy-tree (action.effect-lambda action)))))
  (iter (for fname in (append *query-names* *update-names*))
        (format t "~&  ~A...~%" fname)
        (finish-output)
        (compile fname (subst-int-code (symbol-value fname))))
  (iter (for obj in *happening-names*)
        (format t "~&  ~A...~%" obj)
        (finish-output)
        (when (get obj :interrupt)  ;ie, there is an :interrupt function
          (setf (get obj :interrupt)
                (compile nil (subst-int-code (symbol-value obj))))))
  (when (boundp 'goal-fn)
    (format t "~&  ~A...~%" 'goal-fn)
    (finish-output)
    (compile 'goal-fn (subst-int-code (symbol-value 'goal-fn))))
  (when (boundp 'constraint-fn)
    (format t "~&  ~A...~%" 'constraint-fn)
    (finish-output)
    (compile 'constraint-fn (subst-int-code (symbol-value 'constraint-fn)))))


(defun associate-objects-with-integers ()
  "Build list of all object constants requiring conversion."
  (let (objects)    
    (push 'always-true objects)
    (push 'waiting objects)
    (push nil objects)
    (iter (with flat-codes = (append (alexandria:flatten (when (boundp 'goal-fn)
                                                           (symbol-value 'goal-fn)))
                                     (alexandria:flatten (when (boundp 'constraint-fn)
                                                           (symbol-value 'constraint-fn)))))
          (for item in flat-codes)
          (when (numberp item)
            (collecting item into numbers))
          (finally (alexandria:appendf objects numbers)))
    (iter (for (prop nil) in-hashtable *db*)
          (appending (remove-if-not #'numberp prop) into numbers)
          (finally (alexandria:appendf objects numbers))) 
    (iter (for (prop nil) in-hashtable *static-db*)
          (appending (remove-if-not #'numberp prop) into numbers)
          (finally (alexandria:appendf objects numbers)))
    (alexandria:appendf objects (iter (for (type constants) in-hashtable *types*)
                                      ;(when (symbolp (first constants))
                                        (collecting type)
                                        (appending constants)))
    (alexandria:appendf objects (iter (for (predicate nil) in-hashtable *relations*)
                                      (collecting predicate)))
    (alexandria:appendf objects (iter (for (predicate nil) in-hashtable *static-relations*)
                                      (collecting predicate)))
    (setf objects (delete-duplicates objects))
    (iter (for obj in objects)
          (when (or (listp obj) (vectorp obj))
            (setf *constant-integers* (make-hash-table :test #'equal))  ;redefine if necessary
            (leave)))
    (iter (for obj in objects)
          (for i from 100)
          (setf (gethash obj *constant-integers*) i)
          (setf (gethash i *integer-constants*) obj)
          (finally (setf *last-object-index* i)))))


(defun register-dynamic-object (object type-name)
  "Registers a dynamically-created object in the integer constants system
   and adds the type proposition to the static database.
   This function MUST be called immediately after creating any dynamic object
   (via intern, gensym, etc.) and BEFORE using it in any propositions.
   Purpose:
   - Assigns an integer code enabling database lookups for the new object
   - Creates type proposition (type-name object) in *static-idb*
   - Makes the object discoverable via type queries (e.g., (beam ?b))
   - Ensures thread-safe registration in parallel search environments
   Parameters:
   - object: The dynamically-created symbol (e.g., BEAM3)
   - type-name: The type it belongs to (e.g., BEAM)
   Returns: The registered object (for convenient chaining)
   Example Usage:
   (setq $new-beam (intern (format nil \"BEAM~D\" $index)))
   (register-dynamic-object $new-beam 'beam)
   ;; Now $new-beam can be safely used in propositions like:
   ;; (beam-segment $new-beam ?source ?target $x $y)"
  (declare (type symbol object type-name))
  ;; Input validation
  (check-type object symbol "a symbol")
  (check-type type-name symbol "a symbol")
  ;; Register object in integer constants system
  ;; This enables convert-to-integer to process propositions containing this object
  (unless (gethash object *constant-integers*)
    (bt:with-lock-held (*lock*)
      ;; Double-check pattern: object might have been added by another thread
      (unless (gethash object *constant-integers*)
        (when (>= *last-object-index* 999)
          (error "Design Limit Error: Total number of planning objects exceeds 999"))
        (incf *last-object-index*)
        (setf (gethash object *constant-integers*) *last-object-index*)
        (setf (gethash *last-object-index* *integer-constants*) object))))
  ;; Create type proposition (type-name object) and convert to integer
  ;; This makes the object discoverable via type-based iteration and queries
  (let* ((type-prop (list type-name object))
         (type-prop-int (convert-to-integer-memoized type-prop)))
    (bt:with-lock-held (*lock*)
      ;; Add to static database if not already present
      (unless (gethash type-prop-int *static-idb*)
        (setf (gethash type-prop-int *static-idb*) t))))
  ;; Return the registered object for convenient chaining
  object)
  

(defun subst-int-code (code-tree)
  (labels ((process-item (item)
             (cond ((atom item) item)
                   ((not (typep item 'alexandria:proper-list)) item)
                   ((and (consp item)
                         (eql (first item) 'gethash)
                         (consp (second item)) (eql (first (second item)) 'list))
                      (list (first item)
                            (convert-prop-list (second item))
                            (cond ;((equal (third item) '(problem-state.db state))
                                  ;   '(problem-state.idb state))
                                  ((equal (third item) '(problem-state.idb state))
                                     '(problem-state.idb state))
                                  ((equal (third item) '(problem-state.idb state+))
                                     '(problem-state.idb state+))
                                  ((equal (third item) '(problem-state.idb state-or-state+))
                                     '(problem-state.idb state-or-state+))
                                  ((eql (third item) '*static-db*)
                                     '*static-idb*)
                                  ;((eql (third item) 'idb)
                                  ;   'idb)
                                  ((equal (third item) '(merge-idb-hidb state))
                                     '(merge-idb-hidb state))
                                  ;; ← Add this case for context-aware conditionals
                                  ;((and (consp (third item))
                                  ;      (eql (first (third item)) 'if)
                                  ;      (equal (second (third item)) '(hash-table-p state-or-idb))
                                  ;      (eql (third (third item)) 'state-or-idb)
                                  ;      (equal (fourth (third item)) '(problem-state.idb state-or-idb)))
                                  ;   (third item))  ; Pass through unchanged
                                  ((error "Error in subst-int-code: ~A" (third item))))))
                   (t (mapcar #'process-item item)))))
    (process-item code-tree)))


(defun convert-to-integer-memoized (prop-key)
  "Memoized version for straightforward lookup"
  (or (gethash prop-key *prop-key-cache*)
      (setf (gethash prop-key *prop-key-cache*)
            (convert-to-integer prop-key))))


(defun convert-to-integer (prop-key)
  "Thread-safe original version"
  (iter (for item in prop-key)
        (for multiplier in '(1 1000 1000000 1000000000 1000000000000))
        (ut::if-it (gethash item *constant-integers*)
          (summing (* ut::it multiplier))
          ;; Critical section: check-increment-assign must be atomic
          (bt:with-lock-held (*lock*)
            ;; Double-check pattern: item might have been added by another thread
            (ut::if-it (gethash item *constant-integers*)
              (summing (* ut::it multiplier))
              (progn (when (>= *last-object-index* 999)
                       (error "Design Limit Error: Total # of actual + derived planning objects > 999"))
                     (incf *last-object-index*)
                     (setf (gethash item *constant-integers*) *last-object-index*)
                     (setf (gethash *last-object-index* *integer-constants*) item)
                     (summing (* *last-object-index* multiplier))))))))


(defun convert-prop-list (prop-list)
  "Converts a statement form in an action--eg, (list 'loc ?jammer ?area)
   with no $vars to an integer key form for efficient db access."
  (iter (for item in (cdr prop-list))
        (for multiplier in '(1 1000 1000000 1000000000 1000000000000))
        (for new-item = (cond ((and (consp item) (eql (car item) 'quote))
                                 (* (gethash (second item) *constant-integers*) multiplier))
                              ((and (symbolp item)
                                    (or (char= (char (symbol-name item) 0) #\$)
                                        (char= (char (symbol-name item) 0) #\?)))
                                 `(* (gethash ,item *constant-integers*) ,multiplier))
                              ((numberp item)
                                 (* (gethash item *constant-integers*) multiplier))
                              ((error "Error in convert-prop-list: ~A invalid in ~A"
                                       item prop-list))))
        (collect new-item into new-items)
        (finally (return (cons '+ new-items)))))
