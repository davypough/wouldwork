;;; Filename: problem-connector-clash-test.lisp

;;; Regression for beam-relay's state invariant: no location may hold two active
;;; connectors.  Each case mirrors an in-game put-down test (connector-selection plan,
;;; Phase 5, 2H1-2H5 and 2J).  All beams are red; E-CONNECTOR is active at SITE-P, paired
;;; with T-RED; F-CONNECTOR stands beside it at SITE-P, unpaired and inactive.
;;;
;;;   2H1  c1 keeps a pairing with T-RED and would be active at SITE-P  -> disallowed.
;;;   2H2  c2 keeps a pairing with R-RED only and stays inactive       -> allowed.
;;;   2H3  c3 has selected T-RED and would be active at SITE-P          -> disallowed.
;;;   2H4  c4 has selected R-RED only and stays inactive                -> allowed.
;;;   2H5  c5 keeps a pairing with E-CONNECTOR; at SITE-P the side-by-side link carries
;;;        E's beam, so c5 would be active there                         -> disallowed.
;;;   2J   cj selects T-RED and F-CONNECTOR; put down at ALTJ-SEEN it lights red and
;;;        feeds F-CONNECTOR, which would then be active beside E        -> disallowed.
;;;        At ALTJ-BLIND, with no sightline to SITE-P, F stays inactive  -> allowed.
;;;
;;; The kept-pairing cases (2H1, 2H2, 2H5) start with the connector set down at its probe
;;; origin, take the PICKUP-CONNECTOR-RETAINING-PAIRINGS child, and inspect that child's
;;; PUT-CONNECTOR children.  The selection cases (2H3, 2H4, 2J) start held, with SELECTED
;;; authored directly, and inspect the start state's PUT-CONNECTOR children.  Every
;;; disallowed case also has an allowed control put-down elsewhere.
;;;
;;; The goal holds in the start state (expected length 0); the claims carry the test.


(in-package :ww)


(ww-set *problem-name* connector-clash-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (a1 a2 a3 a4 a5 aj)
  location (site-p o1 o2 o3 o4 o5 oj alt1 alt3 alt5 altj-seen altj-blind)
  connector (e-connector f-connector c1 c2 c3 c4 c5 cj)
  transmitter (t-red)
  receiver (r-red)
  hue (red))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech beam-relay)
(include-tech visibility)
(include-tech reachability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; The active connector E and the inactive connector F share SITE-P.
  (has-chroma t-red red)
  (has-chroma r-red red)
  (has-location e-connector site-p)
  (paired e-connector t-red)
  (los-via site-p () t-red)
  (has-location f-connector site-p)
  ;; 2H1: kept pairing with the transmitter.
  (has-location a1 o1)
  (has-location c1 o1)
  (paired c1 t-red)
  (los-via o1 () t-red)
  (reach-via o1 () site-p)
  (reach-via o1 () alt1)
  ;; 2H2: kept pairing with the receiver only.
  (has-location a2 o2)
  (has-location c2 o2)
  (paired c2 r-red)
  (los-via o2 () r-red)
  (reach-via o2 () site-p)
  ;; 2H3: selected transmitter.
  (has-location a3 o3)
  (holding a3 c3)
  (selected c3 t-red)
  (reach-via o3 () site-p)
  (reach-via o3 () alt3)
  ;; 2H4: selected receiver only.
  (has-location a4 o4)
  (holding a4 c4)
  (selected c4 r-red)
  (reach-via o4 () site-p)
  ;; 2H5: kept pairing with the active connector E.
  (has-location a5 o5)
  (has-location c5 o5)
  (paired c5 e-connector)
  (los-via o5 () site-p)
  (reach-via o5 () site-p)
  (reach-via o5 () alt5)
  ;; 2J: a put-down elsewhere that would make F active beside E.
  (has-location aj oj)
  (holding aj cj)
  (selected cj t-red)
  (selected cj f-connector)
  (los-via altj-seen () t-red)
  (los-via altj-seen () site-p)
  (los-via altj-blind () t-red)
  (reach-via oj () altj-seen)
  (reach-via oj () altj-blind))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; START-STATE CHARACTERIZATION AND GOAL ;;;;


(define-query connector-clash-start-valid ()
  ;; One active connector per location: E at SITE-P, c1 at O1, c5 at O5 (fed by E).
  (and (color e-connector red)
       (not (exists (?h hue)
              (color f-connector ?h)))
       (color c1 red)
       (color c5 red)
       (not (exists (?h hue)
              (color c2 ?h)))
       (not (exists (?l location)
              (connector-location-clash ?l)))))


;;;; GENERATED-CHILD CHARACTERIZATION ;;;;


(define-test-claim connector-clash-kept-pairings
  ;; 2H1: disallowed at SITE-P; control at ALT1.
  (notany (lambda (child)
            (connector-clash-child-matches-p
              child '((has-location c1 site-p)) nil))
          (connector-clash-put-children-after-pickup 'c1))
  (some (lambda (child)
          (connector-clash-child-matches-p
            child '((has-location c1 alt1) (paired c1 t-red)) nil))
        (connector-clash-put-children-after-pickup 'c1))
  ;; 2H2: allowed at SITE-P; c2 stays inactive and E stays red.
  (some (lambda (child)
          (connector-clash-child-matches-p
            child
            '((has-location c2 site-p) (paired c2 r-red) (color e-connector red))
            '((color c2 red))))
        (connector-clash-put-children-after-pickup 'c2))
  ;; 2H5: disallowed at SITE-P; control at ALT5.
  (notany (lambda (child)
            (connector-clash-child-matches-p
              child '((has-location c5 site-p)) nil))
          (connector-clash-put-children-after-pickup 'c5))
  (some (lambda (child)
          (connector-clash-child-matches-p
            child '((has-location c5 alt5) (paired c5 e-connector)) nil))
        (connector-clash-put-children-after-pickup 'c5)))


(define-test-claim connector-clash-selections
  ;; 2H3: disallowed at SITE-P; control at ALT3 pairs and clears the selection.
  (notany (lambda (child)
            (connector-clash-child-matches-p
              child '((has-location c3 site-p)) nil))
          (connector-clash-put-children *start-state*))
  (some (lambda (child)
          (connector-clash-child-matches-p
            child '((has-location c3 alt3) (paired c3 t-red)) '((selected c3 t-red))))
        (connector-clash-put-children *start-state*))
  ;; 2H4: allowed at SITE-P; c4 stays inactive.
  (some (lambda (child)
          (connector-clash-child-matches-p
            child
            '((has-location c4 site-p) (paired c4 r-red) (color e-connector red))
            '((color c4 red))))
        (connector-clash-put-children *start-state*)))


(define-test-claim connector-clash-later-activation
  ;; 2J: lighting F beside E from ALTJ-SEEN is refused; from ALTJ-BLIND F stays inactive.
  (notany (lambda (child)
            (connector-clash-child-matches-p
              child '((has-location cj altj-seen)) nil))
          (connector-clash-put-children *start-state*))
  (some (lambda (child)
          (connector-clash-child-matches-p
            child
            '((has-location cj altj-blind) (paired cj f-connector) (color cj red)
              (color e-connector red))
            '((color f-connector red))))
        (connector-clash-put-children *start-state*)))


(define-test-claim connector-clash-replay-rejected
  ;; The validator refuses a replayed step that the search drops (2H3 at SITE-P).
  (not (nth-value 1 (apply-action-to-state
                      '(put-connector a3 c3 ground site-p) *start-state* nil)))
  (nth-value 1 (apply-action-to-state
                 '(put-connector a3 c3 ground alt3) *start-state* nil)))


(define-test-helper connector-clash-put-children-after-pickup (connector)
  "PUT-CONNECTOR children of the start state's retaining pickup of CONNECTOR."
  (let ((pickup (find-if (lambda (child)
                           (notany (lambda (fact)
                                     (and (eql (first fact) 'has-location)
                                          (eql (second fact) connector)))
                                   (database child)))
                         (connector-clash-action-children
                           *start-state* 'pickup-connector-retaining-pairings))))
    (connector-clash-put-children pickup)))


(define-test-helper connector-clash-put-children (state)
  (connector-clash-action-children state 'put-connector))


(define-test-helper connector-clash-action-children (state action-name)
  "Return STATE's generated children for ACTION-NAME, inconsistent ones dropped."
  (let ((action (find action-name *actions* :key #'action.name))
        (saved-dropped-count *inconsistent-states-dropped*))
    (unwind-protect
      (let ((*actions* (list action)))
        (generate-children
          (make-node :state state :depth 0)))
      (setf *inconsistent-states-dropped* saved-dropped-count))))


(define-test-helper connector-clash-child-matches-p (child required-facts absent-facts)
  (let ((facts (database child)))
    (and (every (lambda (fact)
                  (member fact facts :test #'equal))
                required-facts)
         (notany (lambda (fact)
                   (member fact facts :test #'equal))
                 absent-facts))))


(define-goal
  (connector-clash-start-valid))
