;;; RC relay chain table -- acceptance checks, T16, 2026-09-24.
;;; Run after staging crelay-topo and loading tech/constraint-profile.lisp:
;;;   (stage crelay-topo)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
;;;   (load (merge-pathnames "doc/constraint-method/evidence/rc-relay-chain-checks-2026-09-24.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;; No search, propagation or replay.

(in-package :ww)


(defparameter *rc-check-count* 0)


(defun rc-check (label passed)
  (incf *rc-check-count*)
  (unless passed
    (error "RC check failed: ~A" label))
  (format t "~&  pass  ~A~%" label))


(defun rc-check-s6-agreement ()
  "Every station-to-endpoint hop at a (location, top) S6 also tests agrees with S6's
 512-subset row: same status, same required gates, and exactly 2^(gates - required)
 visible subsets, which is the monotone conjunctive reading RC relies on."
  (let* ((gates (census-type-instances 'gate))
         (records (sightline-visible-records gates))
         (s6-tops (sightline-connector-tops *start-state*))
         (stations (relay-chain-stations *start-state*))
         (compared 0))
    (multiple-value-bind (open-state closed-states) (relay-chain-gate-states gates)
      (dolist (station stations)
        (when (member (second station) s6-tops :test #'=)
          (dolist (endpoint (sightline-fixed-endpoints))
            (let* ((subsets (sightline-visible-subsets (first station) (second station)
                                                       endpoint records))
                   (hop (relay-chain-hop open-state closed-states
                                         (first station) (second station) endpoint
                                         (funcall (symbol-function 'top) open-state endpoint))))
              (incf compared)
              (unless (if (null subsets)
                        (null hop)
                        (and hop
                             (null (set-exclusive-or (rest hop)
                                                     (sightline-required-open-gates subsets)))
                             (= (length subsets)
                                (expt 2 (- (length gates) (length (rest hop)))))))
                (error "RC/S6 disagreement at ~A @ ~A -> ~A" (first station) (second station)
                       endpoint)))))))
    (rc-check (format nil "~D station-to-endpoint rows agree with S6 (status, gates, subset count)"
                      compared)
              (plusp compared))))


(defun rc-check-domain-generality ()
  "C3: no instance name of any declared type appears in the RC block's source text."
  (let* ((path (merge-pathnames "tech/constraint-profile.lisp"
                                (asdf:system-source-directory :wouldwork)))
         (text (with-open-file (stream path)
                 (let ((string (make-string (file-length stream))))
                   (subseq string 0 (read-sequence string stream)))))
         (start (search ";;; T16 -- RC relay chain table" text))
         (end (search "(defun role-class-universe" text :start2 (or start 0)))
         (block (string-downcase (subseq text start end)))
         (hits nil))
    (dolist (type (census-type-names))
      (dolist (object (census-type-instances type))
        (let ((name (string-downcase (symbol-name object))))
          (loop for position = (search name block) then (search name block :start2 (1+ position))
                while position
                do (let ((before (if (plusp position) (char block (1- position)) #\Space))
                         (after (if (< (+ position (length name)) (length block))
                                  (char block (+ position (length name)))
                                  #\Space)))
                     (unless (or (alphanumericp before) (find before "-*")
                                 (alphanumericp after) (find after "-*"))
                       (pushnew name hits :test #'string=)))))))
    (rc-check (format nil "C3: no problem object name in the RC block~@[ (found ~{~A~^, ~})~]" hits)
              (null hits))))


(defun rc-check-classification ()
  "Every EXCLUDED chain holds an S1 exclusion pair; every BOOTSTRAP chain holds none and no
 device its receiver controls; every LATCH chain holds such a device."
  (let* ((gates (census-type-instances 'gate))
         (controls (control-facts))
         (exclusions (relay-chain-exclusion-pairs controls))
         (pressure-plates (census-type-instances 'pressure-plate))
         (stations (relay-chain-stations *start-state*))
         (count 0))
    (multiple-value-bind (open-state closed-states) (relay-chain-gate-states gates)
      (let ((endpoint-links (relay-chain-endpoint-links stations open-state closed-states))
            (station-links (relay-chain-station-links stations open-state closed-states)))
        (dolist (receiver (census-type-instances 'receiver))
          (let ((devices (relay-chain-receiver-devices receiver controls)))
            (dolist (hops (relay-chain-enumerate receiver endpoint-links station-links))
              (let* ((chain (relay-chain-evaluate hops exclusions devices controls pressure-plates))
                     (chain-gates (getf chain :gates))
                     (excluded (some (lambda (pair) (subsetp pair chain-gates)) exclusions)))
                (incf count)
                (ecase (getf chain :class)
                  (:excluded (assert excluded))
                  (:bootstrap (assert (and (not excluded)
                                           (null (intersection chain-gates devices)))))
                  (:latch (assert (and (not excluded) (intersection chain-gates devices))))
                  (:infeasible (assert (eq (getf chain :risers) :infeasible))))))))))
    (rc-check (format nil "~D chains classified consistently" count) (plusp count))))


(defun rc-check-report-runs ()
  (let ((text (with-output-to-string (*standard-output*)
                (report-relay-chain-table))))
    (rc-check "REPORT-RELAY-CHAIN-TABLE prints its section"
              (search "RC  RELAY CHAIN TABLE" text))))


(setf *rc-check-count* 0)
(format t "~&RC relay chain checks, 2026-09-24~%")
(rc-check-s6-agreement)
(rc-check-domain-generality)
(rc-check-classification)
(rc-check-report-runs)
(format t "~&RC checks: ~D passed.~%" *rc-check-count*)
