(defun canonical-base-propositions (props base-rels groups)
  "Filter PROPS to BASE-RELS, then canonicalize under interchangeable GROUPS.
   Tries all permutations of each group and returns the lex-minimum form.
   Without groups, returns sorted base propositions."
  (let ((current (sort (copy-list
                         (remove-if-not (lambda (p) (member (car p) base-rels :test #'eq))
                                        props))
                       #'string< :key #'prin1-to-string)))
    (dolist (group groups current)
      (let ((best current))
        (dolist (perm (enum-all-permutations group))
          (let* ((alist (mapcar #'cons group perm))
                 (subst (mapcar (lambda (p) (enum-substitute-in-form p alist))
                                current))
                 (sorted (sort (copy-list subst) #'string< :key #'prin1-to-string)))
            (when (string< (prin1-to-string sorted) (prin1-to-string best))
              (setf best sorted))))
        (setf current best)))))


(defun find-soln-in-enumerated (soln-props)
  "Search for SOLN-PROPS among enumerated goal states.
   Compares only base-relation propositions (derived state like beams,
   colors, and current-beams is deterministic given base relations).
   Uses canonical comparison under interchangeable groups so that
   symmetry-equivalent states match regardless of object naming.
   Checks *enumerated-goal-states* first, then *enumerated-unique-solutions*.
   Returns (values matching-state index) or (values nil nil)."
  (let* ((base-rels (get-base-relations))
         (groups *enumerator-detected-groups*)
         (canonical-soln (canonical-base-propositions
                           soln-props base-rels groups))
         (states (or *enumerated-goal-states*
                     (mapcar #'solution.goal *enumerated-unique-solutions*))))
    (iter (for state in states)
          (for i from 0)
          (when (equal canonical-soln
                       (canonical-base-propositions
                         (list-database (problem-state.idb state))
                         base-rels groups))
            (return (values state i)))
          (finally (return (values nil nil))))))


(defparameter *soln* '((ACTIVE RECEIVER2)
    (ACTIVE RECEIVER3)
    (BEAM-SEGMENT BEAM4 TRANSMITTER2 CONNECTOR1 9 8)
    (BEAM-SEGMENT BEAM5 TRANSMITTER1 CONNECTOR3 10 9)
    (BEAM-SEGMENT BEAM6 CONNECTOR3 RECEIVER2 7 109/10)
    (BEAM-SEGMENT BEAM7 CONNECTOR3 CONNECTOR1 19/2 17/2)
    (BEAM-SEGMENT BEAM8 CONNECTOR1 RECEIVER1 81/10 1)
    (BEAM-SEGMENT BEAM9 CONNECTOR1 RECEIVER3 1 109/10)
    (BEAM-SEGMENT BEAM10 CONNECTOR1 CONNECTOR3 19/2 17/2)
    (COLOR CONNECTOR3 RED)
    (COLOR CONNECTOR1 BLUE)
    (CURRENT-BEAMS (BEAM10 BEAM9 BEAM8 BEAM7 BEAM6 BEAM5 BEAM4 NIL NIL NIL))
    (HOLDS AGENT1 CONNECTOR2)
    (LOC AGENT1 AREA4)
    (LOC CONNECTOR1 AREA2)
    (LOC CONNECTOR3 AREA3)
    (PAIRED CONNECTOR1 RECEIVER3)
    (PAIRED CONNECTOR1 RECEIVER1)
    (PAIRED CONNECTOR1 TRANSMITTER2)
    (PAIRED CONNECTOR3 CONNECTOR1)
    (PAIRED CONNECTOR3 RECEIVER2)
    (PAIRED CONNECTOR3 TRANSMITTER1)))


(find-soln-in-enumerated *soln*)
