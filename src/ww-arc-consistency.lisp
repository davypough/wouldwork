;;; Filename: ww-arc-consistency.lisp

;;; Propagation to a fixpoint (arc consistency) for problem specs.
;;; A spec numbers its variables, keeps one domain per variable in a simple-vector, and
;;; describes its constraints with make-support-constraint (list domains, a test of a full
;;; combination) or make-narrowing-constraint (any domains, the spec's own narrowing
;;; function).  make-constraint-network indexes them once; propagate-network then narrows
;;; the domains until nothing changes, or returns nil as soon as some domain is empty.
;;; Called from a define-update after an assignment, a nil result should assert
;;; (inconsistent-state), which drops the successor.
;;; Narrowing only removes values that no combination of the other domains supports, so
;;; no solution is lost.  A domain read from a state may be shared with other states, so a
;;; narrowing replaces such an entry with a new list or vector rather than altering it; a
;;; domains vector built privately by the caller (e.g. a completion solver's own copies)
;;; may be narrowed in place.


(in-package :ww)


(defstruct (constraint-network (:conc-name network.) (:constructor new-constraint-network))
  (constraints #() :type simple-vector)  ;each a (variables . narrow-fn)
  (watchers #() :type simple-vector))    ;by variable number, the constraint numbers reading it


(defun make-support-constraint (variables test)
  "A constraint over distinct variable numbers with list domains.  TEST takes one value per
   variable, in order, and is true for an allowed combination.  Narrowing keeps each value
   that occurs in some allowed combination."
  (cons variables
        (lambda (domains)
          (narrow-by-support variables test domains))))


(defun make-narrowing-constraint (variables narrow-fn)
  "A constraint narrowed by the spec's NARROW-FN, called with the domains vector, and run
   again whenever one of VARIABLES (those it reads) changes.  NARROW-FN removes only
   unsupported values and must reach its own fixpoint.  It returns :empty if some domain
   became empty, else the list of variable numbers it changed."
  (cons variables narrow-fn))


(defun make-constraint-network (number-of-variables constraints)
  "Indexes CONSTRAINTS by the variables each one reads."
  (let ((watchers (make-array number-of-variables :initial-element nil))
        (constraint-vector (coerce constraints 'simple-vector)))
    (loop for (variables) across constraint-vector
          for k from 0
          do (dolist (v variables)
               (push k (svref watchers v))))
    (new-constraint-network :constraints constraint-vector :watchers watchers)))


(defun propagate-network (network domains changed)
  "Narrows DOMAINS (a simple-vector by variable number) in place until no constraint
   removes anything.  Starts from the constraints using the variable numbers in CHANGED,
   or from every constraint if CHANGED is :all.  Returns nil as soon as a domain is empty,
   else t."
  (let* ((constraints (network.constraints network))
         (watchers (network.watchers network))
         (queued (make-array (length constraints) :element-type 'bit :initial-element 0))
         (queue (if (eq changed :all)
                  (loop for k below (length constraints) collect k)
                  (remove-duplicates (loop for v in changed append (svref watchers v))))))
    (dolist (k queue)
      (setf (sbit queued k) 1))
    (loop while queue
          do (let* ((k (pop queue))
                    (result (funcall (cdr (svref constraints k)) domains)))
               (setf (sbit queued k) 0)
               (when (eq result :empty)
                 (return-from propagate-network nil))
               (dolist (v result)
                 (dolist (j (svref watchers v))
                   (when (and (/= j k) (zerop (sbit queued j)))
                     (setf (sbit queued j) 1)
                     (push j queue))))))
    t))


(defun narrow-by-support (variables test domains)
  "Narrowing for a support constraint: :empty, or the variable numbers whose domains shrank."
  (let ((supported (make-array (length variables) :initial-element nil))
        (changed nil))
    (collect-support (mapcar (lambda (v) (svref domains v)) variables) nil test supported)
    (loop for v in variables
          for keep across supported
          for old = (svref domains v)
          do (cond ((null keep)
                    (return-from narrow-by-support :empty))
                   ((< (length keep) (length old))
                    (setf (svref domains v) (remove-if-not (lambda (x) (member x keep)) old))
                    (push v changed))))
    changed))


(defun collect-support (remaining-domains reversed-values test supported)
  "Tries every combination drawn from REMAINING-DOMAINS after the REVERSED-VALUES already
   chosen; for each one TEST allows, records its values in SUPPORTED by position."
  (if (null remaining-domains)
    (let ((values (reverse reversed-values)))
      (when (apply test values)
        (loop for x in values
              for i from 0
              do (pushnew x (svref supported i)))))
    (dolist (x (first remaining-domains))
      (collect-support (rest remaining-domains) (cons x reversed-values) test supported))))
