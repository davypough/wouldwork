;;; triangle-xyz-6 -- accepted actions from the original start (constraint-method pilot).
;;; Load after (stage triangle-xyz-6) in a separate top-level form.
;;; Jump forms are (jump-<dir> x y): the jumping peg's position; the peg is found from it.
(in-package :ww)

(defparameter *accepted-actions*
  '(;; SG1 corner 16 peg out (16 pegs; empty 13 15 16 22 31)
    (jump-ru 1 3)    ; 13 over 12 to 11 (first move fixed by symmetry)
    (jump-lh 3 1)    ; 31 over 22 to 13
    (jump-ru 1 4)    ; 14 over 13 to 12
    (jump-ru 1 6)    ; 16 over 15 to 14
    ;; SG2 corner 61 peg out (13 pegs; empty 13 15 16 22 33 42 51 61)
    (jump-lu 5 1)    ; 51 over 41 to 31
    (jump-rh 3 3)    ; 33 over 42 to 51
    (jump-lu 6 1)    ; 61 over 51 to 41
    ;; SG3 bottom edge down to 25 34, finish at 11 kept (9 pegs)
    (jump-ld 3 1)    ; 31 over 32 to 33
    (jump-rh 1 4)    ; 14 over 23 to 32
    (jump-lu 3 4)    ; 34 over 24 to 14
    (jump-lh 5 2)    ; 52 over 43 to 34
    ;; SG4 meeting board 11 12 21 23 32 (5 pegs), two ways to finish at 11
    (jump-rh 2 5)    ; 25 over 34 to 43
    (jump-lh 4 1)    ; 41 over 32 to 23
    (jump-rh 1 4)    ; 14 over 23 to 32
    (jump-lu 4 3)    ; 43 over 33 to 23
    ;; SG5 goal, one peg at 41 -- search-found, depth 4
    (jump-ld 1 1)    ; 11 over 12 to 13
    (jump-rd 1 3)    ; 13 over 23 to 33
    (jump-ru 3 3)    ; 33 over 32 to 31
    (jump-rd 2 1)))  ; 21 over 31 to 41

(loop for form in *accepted-actions*
      for index from 1
      for action = (find (first form) *actions* :key #'action.name)
      do (unless action
           (error "Action ~D is unknown: ~S" index form))
         (unless (= (length (strip-display-connectives action (rest form)))
                    (length (action.effect-variables action)))
           (error "Action ~D is malformed: ~S" index form)))

(defparameter *accepted-validation*
  (validate-action-sequence *start-state* *accepted-actions*
                            :goal-test (symbol-function 'goal-fn) :verbose t))

(format t "~%~D actions: success=~S goal-checked=~S goal-satisfied=~S failure-index=~S reason=~S~%"
        (length *accepted-actions*)
        (action-sequence-validation-success-p *accepted-validation*)
        (action-sequence-validation-goal-checked-p *accepted-validation*)
        (action-sequence-validation-goal-satisfied-p *accepted-validation*)
        (action-sequence-validation-failure-index *accepted-validation*)
        (action-sequence-validation-failure-reason *accepted-validation*))
(display-validation-state (action-sequence-validation-final-state *accepted-validation*))

(defparameter *accepted-validators-p*
  (and (action-sequence-validation-goal-satisfied-p *accepted-validation*)
       (report-solution-validator-verdicts
         *accepted-actions* (action-sequence-validation-final-state *accepted-validation*))))

(when (and (action-sequence-validation-success-p *accepted-validation*)
           (action-sequence-validation-goal-checked-p *accepted-validation*)
           (action-sequence-validation-goal-satisfied-p *accepted-validation*)
           *accepted-validators-p*)
  (with-open-file (*standard-output*
                   (merge-pathnames "doc/constraint-led-solving/constraint-pilot/triangle-xyz-6/Validation.txt"
                                    (asdf:system-source-directory :wouldwork))
                   :direction :output :if-exists :supersede)
    (format t ";;; triangle-xyz-6 -- complete validated solution, ~D actions.~%" (length *accepted-actions*))
    (format t ";;; SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T; solution validators accepted~%")
    (format t ";;; To re-validate: (stage triangle-xyz-6), then evaluate this form separately.~%")
    (format t "(validate-solution :verbose")
    (let ((*print-case* :downcase)
          (*print-pretty* nil))
      (loop for form in *accepted-actions*
            for index from 1
            do (format t "~%  ~S" (list index form))))
    (format t ")~2%Final state:~%")
    (display-validation-state (action-sequence-validation-final-state *accepted-validation*))))
