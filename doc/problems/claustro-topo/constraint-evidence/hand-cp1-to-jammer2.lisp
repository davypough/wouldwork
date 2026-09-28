;;; claustro-topo -- hand sequence from cp1 (cp1-jammer1-at-location7.txt) to agent1 holding jammer2.
;;; VALIDATED 2026-09-27 with VALIDATE-ACTION-SEQUENCE from (search-checkpoint-state *cp1*): SUCCESS-P T.
;;; Endpoint: agent1 at location9 holding jammer2; jammer1 at location8 jamming gate5; box1 at location2;
;;; box2 at location10; open gate4, gate5.

((move agent1 ((walk location7 nil location8)))
 (jam-target > agent1 jams gate5 with jammer1 at location8 on ground)
 (move agent1 ((walk location8 (gate4 gate5 screen1) location9)))
 (pickup-jammer > agent1 picks up jammer2 at location9))
