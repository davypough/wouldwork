;;; UNVALIDATED candidate, transcribed verbatim from the printed E007 action.
;;; Display-form action uses the engine's existing action-phrase normalizer.
;;; No assertion that replay or cumulative recorder validation will succeed.
(:problem crelay-topo :experiment e007 :status :unvalidated
 :parent c008 :parent-file "E006-candidates.sexp" :parent-candidate 1
 :source "E007-search-output.txt"
 :goal (and (has-location box1 location5) (not (on box1 tray1*))
            (holding agent1* tray1*))
 :segment ((22.0 (put-box > agent1 puts box1 on ground at location5))))
