;;; corner-topo -- hand-derived full-path candidate (A, 2026-09-27), from Briefing subgoal log.
;;; Termini lists follow CONNECT-CONNECTOR's enumeration order (reverse of declaration:
;;; connectors, receivers, transmitters, each descending); the validator compares them strictly.
;;; Move route segments are A's guess at the derived WALK segment form; adjust if replay rejects them.

(defparameter *corner-topo-candidate*
  '((pickup-connector > agent1 picks up connector1 at location1)
    (connect-connector > agent1 connects connector1 on ground to (receiver1 transmitter1) at location1)
    (move agent1 ((walk location1 nil location2)))
    (pickup-connector > agent1 picks up connector2 at location2)
    (move agent1 ((walk location2 (gate1) location4)))
    (connect-connector > agent1 connects connector2 on ground to (transmitter1) at location4)
    (move agent1 ((walk location4 (gate1) location1)))
    (pickup-connector > agent1 picks up connector1 at location1)
    (move agent1 ((walk location1 nil location2)))
    (connect-connector > agent1 connects connector1 on ground to (receiver3 receiver1 transmitter2) at location2)
    (move agent1 ((walk location2 nil location3)))
    (pickup-connector > agent1 picks up connector3 at location3)
    (connect-connector > agent1 connects connector3 on ground to (connector1 receiver2 transmitter1) at location3)
    (move agent1 ((walk location3 (gate1) location4)))
    (pickup-connector > agent1 picks up connector2 at location4)))
