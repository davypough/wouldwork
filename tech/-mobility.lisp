;;; Filename: -mobility.lisp

;;; Mobility substrate: composes pure traversal providers into a canonical transparent
;;; movement closure.  Providers return normalized segments of the form
;;; (mode source witness destination).  MOBILITY-RESULTS returns
;;; (destination route) pairs, including (source nil) for reflexivity.
;;;
;;; REQUIRES:
;;;   types    : agent, location
;;; PROVIDES:
;;;   queries  : mobility-results, mobility-locations, traversable
;;;   functions: register-mobility-provider and canonical closure helpers

(in-package :ww)


(defparameter *mobility-providers* nil
  "Problem-local query names that return traversal segments from one source.")


(defparameter *mobility-route-keys* (make-hash-table :test #'equal)
  "Printed lexical keys for normalized semantic routes in the staged problem.")


(define-problem-helper register-mobility-provider (provider)
  "Register a pure traversal-provider query for the staged problem."
  (unless (and (symbolp provider)
               (member provider *query-names* :test #'eq))
    (error "Mobility provider must name an installed query: ~S" provider))
  (pushnew provider *mobility-providers* :test #'eq)
  provider)


(define-problem-helper mobility-route-key (route)
  "Return the stable lexical tie-break key for a normalized semantic route."
  (multiple-value-bind (key present)
      (gethash route *mobility-route-keys*)
    (if present
      key
      (let ((*print-case* :upcase)
            (*print-pretty* nil)
            (*package* (find-package :ww)))
        (setf (gethash route *mobility-route-keys*)
              (prin1-to-string route))))))


(define-problem-helper mobility-result-route-key (result)
  "Return RESULT's lexical route key."
  (mobility-route-key (second result)))


(define-problem-helper mobility-segment-route-key (segment)
  "Return SEGMENT's lexical key in its one-segment route representation."
  (mobility-route-key (list segment)))


(define-problem-helper mobility-sort-by-route-key (items key-function)
  "Sort ITEMS lexically after computing each item's route key exactly once."
  (loop for tail on items
        for item = (first tail)
        do (setf (first tail)
                 (cons (funcall key-function item) item)))
  (setf items (sort items #'string< :key #'first))
  (loop for tail on items
        do (setf (first tail) (rest (first tail))))
  items)


(define-problem-helper validate-mobility-segment (segment source)
  "Signal an error unless SEGMENT obeys the normalized provider contract."
  (unless (and (listp segment)
               (= (length segment) 4)
               (symbolp (first segment))
               (eql (second segment) source)
               (member (fourth segment) (gethash 'location *types*) :test #'eq))
    (error "Invalid mobility segment from ~S: ~S" source segment))
  segment)


(define-problem-helper mobility-provider-segments (state agent source)
  "Collect, validate, deduplicate, and canonically order provider segments."
  (let ((segments nil))
    (dolist (provider *mobility-providers*)
      (dolist (segment (funcall (symbol-function provider) state agent source))
        (push (validate-mobility-segment segment source) segments)))
    (mobility-sort-by-route-key
      (remove-duplicates segments :test #'equal)
      #'mobility-segment-route-key)))


(define-problem-helper mobility-expand-frontier (state agent frontier visited)
  "Return every unvisited one-segment extension of the current BFS layer."
  (let ((candidates nil))
    (dolist (result frontier)
      (let ((source (first result))
            (route (second result)))
        (dolist (segment (mobility-provider-segments state agent source))
          (let ((destination (fourth segment)))
            (unless (gethash destination visited)
              (push (list destination
                          (append route (list (copy-tree segment))))
                    candidates))))))
    (mobility-sort-by-route-key
      (remove-duplicates candidates :test #'equal)
      #'mobility-result-route-key)))


(define-problem-helper mobility-select-layer (candidates visited)
  "Retain the canonical shortest route for each new destination in a BFS layer."
  (let ((selected nil))
    (dolist (candidate candidates (nreverse selected))
      (let ((destination (first candidate)))
        (unless (gethash destination visited)
          (setf (gethash destination visited) t)
          (push candidate selected))))))


(define-problem-helper mobility-results-in-state (state agent source)
  "Compute canonical transparent mobility results from SOURCE in STATE."
  (let ((visited (make-hash-table :test #'eq))
        (frontier (list (list source nil)))
        (results (list (list source nil))))
    (setf (gethash source visited) t)
    (loop while frontier
          for candidates = (mobility-expand-frontier state agent frontier visited)
          do (setf frontier (mobility-select-layer candidates visited))
             (setf results (nconc results (copy-tree frontier))))
    results))


(define-query mobility-results (?agent agent ?from location)
  (do (assign $results (mobility-results-in-state state ?agent ?from))
      $results))


(define-query mobility-locations (?agent agent ?from location)
  (mapcar #'first (mobility-results ?agent ?from)))


(define-query traversable (?agent agent ?from location ?to location)
  (not (null (assoc ?to (mobility-results ?agent ?from) :test #'eq))))
