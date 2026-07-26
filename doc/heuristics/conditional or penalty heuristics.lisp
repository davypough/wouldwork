;;; Conditional/Penalty Heuristics

;;; These are binary or step-function heuristics based on state conditions.


;;; Returns penalty if condition true, 0 otherwise
(define-query h-gate-closed-penalty (?gate ?penalty)
  (if (open ?gate) 0 ?penalty))


;;; Returns 1 if condition true, 0 otherwise (for weighted combination)
(define-query h-flag (?condition-result)
  (if ?condition-result 1 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-query h-gate1-closed ()
  (if (open gate1) 0 1))


(define-query h-agent-not-in-goal-area ()
  (do (bind (loc agent1 $a))
      (if (eql $a 'area4) 0 1)))


(define-query h-not-holding-needed-item ()
  (if (bind (holds agent1 $cargo)) 0 1))