;;; claustro-topo -- full candidate solution from the start state, 36 actions.
;;; Actions 1-8: search result (cp1).  9-12: hand sequence validated from cp1 2026-09-27.
;;; 13-36: hand-written 2026-09-27 from the interview (window jam of gate1, handover).
;;; 2026-09-30: traversal-separator migration -- action 34's jump names (edge1), action 36's
;;; stairs name (staircase1).  Action 33 (onto box2 within location10) keeps NIL.
;;; Validate with VALIDATE-ACTION-SEQUENCE from the staged start and the staged goal.

(;; 1-8  jam gate1, cut the beam with box1 at location2, take jammer1 back through the window
 (pickup-jammer > agent1 picks up jammer1 at location1)
 (jam-target > agent1 jams gate1 with jammer1 at location1 on ground)
 (move agent1 ((walk location1 (gate1 gate3) location4)))
 (pickup-box > agent1 picks up box1 at location4 from location4)
 (move agent1 ((walk location4 nil location3)))
 (put-box > agent1 puts box1 on ground at location2)
 (move agent1 ((walk location3 (gate4 screen1) location7)))
 (pickup-jammer > agent1 picks up jammer1 at location7)
 ;; 9-12  jam gate5 from location8, fetch jammer2
 (move agent1 ((walk location7 nil location8)))
 (jam-target > agent1 jams gate5 with jammer1 at location8 on ground)
 (move agent1 ((walk location8 (gate4 gate5 screen1) location9)))
 (pickup-jammer > agent1 picks up jammer2 at location9)
 ;; 13-18  park jammer2 on plate3 holding gate5; fetch jammer1; jam gate1 through the window
 (move agent1 ((walk location9 (gate5) location6)))
 (jam-target > agent1 jams gate5 with jammer2 at location6 on plate3)
 (move agent1 ((walk location6 (gate4 screen1) location8)))
 (pickup-jammer > agent1 picks up jammer1 at location8)
 (move agent1 ((walk location8 nil location7)))
 (jam-target > agent1 jams gate1 with jammer1 at location1 on ground)
 ;; 19-24  ladder to location1, clear location2 (beam lights), box1 onto plate1
 (move agent1 ((ladder location7 (ladder1) location1)))
 (move agent1 ((walk location1 nil location2)))
 (pickup-box > agent1 picks up box1 at location2 from location2)
 (move agent1 ((walk location2 nil location1)))
 (move agent1 ((walk location1 (gate1 gate3) location4)))
 (put-box > agent1 puts box1 on plate1 at location4)
 ;; 25-31  handover: jammer2 jams gate1 from plate3; jammer1 fetched, jams gate5 from plate2
 (move agent1 ((walk location4 nil location6)))
 (pickup-jammer > agent1 picks up jammer2 at location6)
 (jam-target > agent1 jams gate1 with jammer2 at location6 on plate3)
 (move agent1 ((walk location6 (gate1 gate3) location1)))
 (pickup-jammer > agent1 picks up jammer1 at location1)
 (move agent1 ((walk location1 (gate1 gate3) location5)))
 (jam-target > agent1 jams gate5 with jammer1 at location5 on plate2)
 ;; 32-36  out through gate5, gate6, gate7; onto box2; jump to the slab; cross; stairs
 (move agent1 ((walk location5 (gate5 gate6 gate7) location10)))
 (move agent1 ((jump (location10 ground) nil (location10 box2))))
 (move agent1 ((jump (location10 box2) (edge1) (location12 ground))))
 (move agent1 ((walk location12 (gate8 gate9) location13)))
 (move agent1 ((stairs location13 (staircase1) location11))))
