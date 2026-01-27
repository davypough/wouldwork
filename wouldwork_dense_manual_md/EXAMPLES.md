# Examples Index

Each code example is assigned an ID so the rewritten docs can reference it unambiguously.

| Example ID | Source Part | Source Section | Para Range | First Line |
| --- | --- | --- | --- | --- |
| EX0001 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Quickstart with SBCL | 100-101 | `> sbcl –dynamic-space-size 2048` |
| EX0002 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Specifying Environmental Objects & Types | 117-121 | `(define-types` |
| EX0003 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Specifying Object Relations | 130-132 | `(define-dynamic-relations` |
| EX0004 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Specifying Possible Actions | 139-144 | `(and (not (exists (?b block) ;no block is on ?block` |
| EX0005 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Specifying Possible Actions | 149-152 | `(and (cleartop? ?block)))` |
| EX0006 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Specifying Possible Actions | 157-159 | `(assert (on ?block ?support)` |
| EX0007 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Specifying Possible Actions | 166-178 | `(define-action put` |
| EX0008 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Specifying Initial Conditions | 193-197 | `(define-init` |
| EX0009 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Specifying the Goal Condition | 205-209 | `(define-goal` |
| EX0010 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 242-243 | `*problem-name* = unspecified (any symbol, string, or number)` |
| EX0011 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 245-246 | `*problem-type* = planning (planning or csp)` |
| EX0012 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 248-249 | `*algorithm* = depth-first (depth-first or backtracking)` |
| EX0013 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 251-252 | `*depth-cutoff* = 0 (n >= 0)` |
| EX0014 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 254-255 | `*tree-or-graph* = graph (tree or graph)` |
| EX0015 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 257-258 | `*solution-type* = first (first, min-length, min-time, min-value, max-value, ever` |
| EX0016 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 260-261 | `*progress-reporting-interval* = 200000 (n > 0)` |
| EX0017 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 265-266 | `*debug* 0 (0 <= n <= 5)` |
| EX0018 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 268-269 | `*probe* nil (any action step)` |
| EX0019 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 273-274 | `*threads* 0 (0 <= n <= N-1)` |
| EX0020 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 276-277 | `*branch* -1 (-1 or 1 <= n <= N)` |
| EX0021 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 279-280 | `*randomize-search* nil (nil or t)` |
| EX0022 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Control Settings | 282-283 | `CTRL-C` |
| EX0023 | PART 1. THE WOULDWORK PLANNER USER INTERFACE | Program Output | 288-364 | `(run “blocks3”)` |
| EX0024 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Object Types | 381-386 | `(define-types` |
| EX0025 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Bijective Relations | 410-413 | `(define-dynamic-relations` |
| EX0026 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Bijective Relations | 416-418 | `(define-dynamic-relations` |
| EX0027 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Bijective Relations | 424-427 | `(and (bind (loc agent1 $area)) ;$area gets bound, say to AREA1` |
| EX0028 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Bijective Relations | 435-451 | `(define-types` |
| EX0029 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Complementary Relations | 464-468 | `(define-complementary-relations` |
| EX0030 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Logical Statements, Quantifiers, & Doall | 477-482 | `(doall (?g gate ?s switch)` |
| EX0031 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Logical Statements, Quantifiers, & Doall | 487-492 | `(doall (?g gate)` |
| EX0032 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Durative Actions | 503-511 | `(define-action move` |
| EX0033 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Fluent Variables & Variable Binding | 527-535 | `(define-types` |
| EX0034 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Fluent Variables & Variable Binding | 538-546 | `(define-action fill` |
| EX0035 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Global Variables | 558-559 | `(sb-ext:defglobal var-name val-form doc-string)` |
| EX0036 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Global Variables | 562-565 | `(sb-ext:atomic-incf var-name delta) for fixnums, or` |
| EX0037 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Dynamic (aka Special) Variables | 573-589 | `(define-query bag-item ($item)` |
| EX0038 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Global Constraint | 629-634 | `(define-constraint` |
| EX0039 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Query, Update, and Lisp Functions | 645-651 | `(define-query elevation? ($support)` |
| EX0040 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Query, Update, and Lisp Functions | 658-661 | `(if (stable? ?support)` |
| EX0041 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Query, Update, and Lisp Functions | 665-666 | `(deactivate-receiver! ?receiver)` |
| EX0042 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Query, Update, and Lisp Functions | 669-675 | `(define-update deactivate-receiver! ($receiver)` |
| EX0043 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Exogenous Events | 697-708 | `(define-happening sentry1` |
| EX0044 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Exogenous Events | 713-718 | `(define-constraint ;avoid kill situation` |
| EX0045 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Exogenous Events | 723-735 | `(define-happening sentry1` |
| EX0046 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Exogenous Patrolling Objects | 742-754 | `(define-patroller mine1` |
| EX0047 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Waiting | 763-766 | `(ww-set *tree-or-graph* tree)` |
| EX0048 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Initialization Actions | 777-783 | `(define-init-action activate-sentries` |
| EX0049 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Initialization Actions | 788-794 | `(define-action delete-add-first-peg` |
| EX0050 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Multiple Action Effects | 808-813 | `(do (if …` |
| EX0051 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 820-821 | `(loc ?sentry ?area)` |
| EX0052 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 824-827 | `(assert (loc ?sentry ?area)), or` |
| EX0053 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 829-830 | `(loc sentry1 ?area)` |
| EX0054 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 832-833 | `(not (loc ?sentry ?area))` |
| EX0055 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 835-836 | `(loc ?sentry $area)` |
| EX0056 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 838-840 | `(bind (loc ?sentry $area1))` |
| EX0057 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 842-845 | `(assert (if (active ?sentry)` |
| EX0058 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 847-848 | `(cleartop? ?block)` |
| EX0059 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 851-852 | `(ww-loop for ($x $y) in $positions do …)` |
| EX0060 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 854-855 | `(setf $elevation (elevation? ?support))` |
| EX0061 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 857-858 | `(count #.(1+ *n*))` |
| EX0062 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 860-861 | `(climbable> ?ladder ?area1 ?area2)` |
| EX0063 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 863-866 | `(exists (?sentry sentry)` |
| EX0064 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 868-873 | `(forall ((?sentry1 ?sentry2))` |
| EX0065 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 876-881 | `(doall (?g gate ?s switch)` |
| EX0066 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 883-885 | `(do (activate-transmitter! ?transmitter)` |
| EX0067 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 887-888 | `(different ?block ?support)` |
| EX0068 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 890-891 | `(setq $objective-value (+ $current-value $additional-value))` |
| EX0069 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 893-894 | `(delete-actions 'add-first-peg 'delete-add-first-peg))` |
| EX0070 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 896-897 | `(always-true)` |
| EX0071 | PART 2: EXPLANATION OF OPTIONAL FEATURES | The Variety of Logical Statements | 899-900 | `(print ?area), (< $height1 $height2), etc.` |
| EX0072 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Plan Monitoring & General Debugging | 919-929 | `(define-action put` |
| EX0073 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Plan Monitoring & General Debugging | 934-941 | `(ut::show (first *actions*)) ;show first rule code` |
| EX0074 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Solution Validation | 952-956 | `(validate-solution` |
| EX0075 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Debugging with Runtime Invariants | 981-988 | `(define-update check-emptys ()` |
| EX0076 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Debugging with Runtime Invariants | 996-1000 | `(define-invariant holds-cargo-location ()` |
| EX0077 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1005-1006 | `sbcl --dynamic-space-size 24000` |
| EX0078 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1008-1009 | `(run-test-problems) or (test)` |
| EX0079 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1011-1012 | `(list-all-problems)` |
| EX0080 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1014-1015 | `(ut::show (first *actions*))` |
| EX0081 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1018-1019 | `(pprint (get '*goal* 'fn))` |
| EX0082 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1021-1022 | `(ut::show *types*)` |
| EX0083 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1024-1025 | `(ut::show *relations*)` |
| EX0084 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1027-1028 | `(ut::show *static-relations*)` |
| EX0085 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1030-1031 | `(ut::show *db*)` |
| EX0086 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1033-1034 | `(ut::show *static-db*)` |
| EX0087 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1036-1037 | `(ut::prt s-expr s-expr …)` |
| EX0088 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1039-1040 | `(profile)` |
| EX0089 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Potentially Useful User Commands | 1042-1043 | `(reset)` |
| EX0090 | PART 2: EXPLANATION OF OPTIONAL FEATURES | Some Useful User Functions to Use in Rules | 1050-1088 | `(different sym1 sym2)` |
| EX0091 | PART 3: PROBLEM SOLVING STRATEGIES | Symmetry Pruning | 1169-1170 | `(ww-set *symmetry-pruning* t)` |
| EX0092 | PART 3: PROBLEM SOLVING STRATEGIES | Symmetry Pruning | 1174-1178 | `Symmetry groups detected: 1` |
| EX0093 | PART 3: PROBLEM SOLVING STRATEGIES | Symmetry Pruning | 1182-1183 | `Symmetry: 847 canonical duplicates pruned (23.4% of repeated states)` |
| EX0094 | APPENDIX: SAMPLE PROBLEMS | Blocks World Problem | 1244-1363 | `Blocks Problem Specification:` |
| EX0095 | APPENDIX: SAMPLE PROBLEMS | Boxes Problem | 1370-1570 | `Boxes Problem Specification:` |
| EX0096 | APPENDIX: SAMPLE PROBLEMS | 2-Jugs Problem | 1576-1722 | `2-Jugs Problem Specification:` |
| EX0097 | APPENDIX: SAMPLE PROBLEMS | Sentry Problem | 1727-2006 | `Sentry Problem Specification:` |
| EX0098 | APPENDIX: SAMPLE PROBLEMS | 4-Queens Problem | 2018-2080 | `4-Queens Problem Specification:` |
| EX0099 | APPENDIX: SAMPLE PROBLEMS | 4-Queens Problem | 2180-2181 | `2,030,576 bytes consed` |
| EX0100 | APPENDIX: SAMPLE PROBLEMS | Smallspace Problem | 2187-2930 | `Smallspace Problem Specification:` |
| EX0101 | APPENDIX: SAMPLE PROBLEMS | 7. Capt John’s Journey Problem | 2939-3228 | `Capt John Problem Specification` |
| EX0102 | APPENDIX: SAMPLE PROBLEMS | 8. Triangle Peg Puzzle | 3235-3527 | `Triangle Peg Problem Specification` |
| EX0103 | APPENDIX: SAMPLE PROBLEMS | 9. Knapsack Problem | 3537-3771 | `Knapsack Problem Specification` |