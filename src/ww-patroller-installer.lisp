;;; Filename: ww-patroller-installer.lisp

;;; Installation functions for patroller objects.
;;; patrollers are path-following objects that generate their events automatically
;;; from a path specification, unlike define-happening which requires explicit
;;; event enumeration.

(in-package :ww)


;;; ============================================================================
;;; PATROLLER SLOT PARSING
;;; ============================================================================

(defun parse-patroller-slots (slots)
  "Parse the slot-value pairs from define-patroller into a plist.
   Recognized slots: path, mode, timings, interrupt, rebound, kill, aftereffect"
  (let (plist)
    (iter (for (slot value) on slots by #'cddr)
          (case slot
            (path     (setf (getf plist :path) value))
            (mode     (setf (getf plist :mode) value))
            (timings  (setf (getf plist :timings) value))
            (interrupt (setf (getf plist :interrupt) value))
            (rebound  (setf (getf plist :rebound) value))
            (kill     (setf (getf plist :kill) value))
            (aftereffect (setf (getf plist :aftereffect) value))
            (otherwise (error "Unknown patroller slot: ~A" slot))))
    plist))


;;; ============================================================================
;;; PATROLLER VALIDATION
;;; ============================================================================

(defun check-patroller-spec (object plist)
  "Validate the patroller specification."
  (check-type object symbol)
  (let ((path (getf plist :path))
        (mode (getf plist :mode))
        (timings (getf plist :timings)))
    ;; Path is required and must have at least 2 areas
    (unless path
      (error "Patroller ~A requires a path specification" object))
    (unless (and (listp path) (>= (length path) 2))
      (error "Patroller ~A path must have at least 2 areas: ~A" object path))
    ;; Mode is required and must be :reverse or :cycle
    (unless (member mode '(:reverse :cycle))
      (error "Patroller ~A mode must be :reverse or :cycle, got: ~A" object mode))
    ;; Timings validation if provided
    (when timings
      (let ((expected-length (1- (length path))))
        (unless (= (length timings) expected-length)
          (error "Patroller ~A timings length ~A doesn't match path segments ~A"
                 object (length timings) expected-length))
        (unless (every #'plusp timings)
          (error "Patroller ~A timings must all be positive: ~A" object timings))))))


;;; ============================================================================
;;; PATROLLER EVENT GENERATION
;;; ============================================================================

(defun generate-patroller-events (object path mode timings)
  "Generate the events array for a patroller based on mode.
   Returns a simple-vector compatible with the happening machinery."
  (ecase mode
    (:reverse (generate-patroller-reverse-events object path timings))
    (:cycle   (generate-patroller-cycle-events object path timings))))


(defun generate-patroller-reverse-events (object path timings)
  "Generate events for ping-pong (reverse) mode traversal.
   For path (a1 a2 a3 a4 a5), generates transitions:
   a1→a2→a3→a4→a5→a4→a3→a2→a1, then repeats.
   
   Event format matches define-happening:
   (cumulative-time (not (loc obj from)) (loc obj to))"
  (let* ((n (length path))
         (segment-times (or timings (make-list (1- n) :initial-element 1)))
         (reverse-times (reverse segment-times))
         (events nil)
         (cumulative-time 0))
    ;; Forward pass: a1→a2→...→aN
    (iter (for i from 0 below (1- n))
          (for from-area = (nth i path))
          (for to-area = (nth (1+ i) path))
          (for dt in segment-times)
          (incf cumulative-time dt)
          (push `(,cumulative-time
                  (not (loc ,object ,from-area))
                  (loc ,object ,to-area))
                events))
    ;; Backward pass: aN→a(N-1)→...→a1
    (iter (for i from (1- n) above 0)
          (for from-area = (nth i path))
          (for to-area = (nth (1- i) path))
          (for dt in reverse-times)
          (incf cumulative-time dt)
          (push `(,cumulative-time
                  (not (loc ,object ,from-area))
                  (loc ,object ,to-area))
                events))
    (coerce (nreverse events) 'simple-vector)))


(defun generate-patroller-cycle-events (object path timings)
  "Generate events for cycle mode traversal.
   For path (a1 a2 a3 a4 a5), generates transitions:
   a1→a2→a3→a4→a5→a1, then repeats.
   
   The wrap-around segment (aN→a1) uses 1 time unit by default."
  (let* ((n (length path))
         (segment-times (or timings (make-list (1- n) :initial-element 1)))
         (events nil)
         (cumulative-time 0))
    ;; Forward through all segments
    (iter (for i from 0 below (1- n))
          (for from-area = (nth i path))
          (for to-area = (nth (1+ i) path))
          (for dt in segment-times)
          (incf cumulative-time dt)
          (push `(,cumulative-time
                  (not (loc ,object ,from-area))
                  (loc ,object ,to-area))
                events))
    ;; Wrap-around: aN→a1 (default 1 time unit)
    (incf cumulative-time 1)
    (push `(,cumulative-time
            (not (loc ,object ,(car (last path))))
            (loc ,object ,(first path)))
          events)
    (coerce (nreverse events) 'simple-vector)))


;;; ============================================================================
;;; PATROLLER INSTALLATION
;;; ============================================================================


(defun install-patroller (object slots)
  "Install a patroller as a happening with auto-generated events.
   The patroller's initial location must be specified separately via define-init.
   
   Integration with existing happening machinery:
   - Stores :events as simple-vector (same format as define-happening)
   - Sets :repeat to t (patrollers always cycle)
   - Registers in *happening-names* for compilation
   - Interrupt lambda stored in :interrupt-lambda for compile-all-functions"
  (format t "~&Installing patroller ~A..." object)
  (let ((plist (parse-patroller-slots slots)))
    (check-patroller-spec object plist)
    (let ((path (getf plist :path))
          (mode (getf plist :mode))
          (timings (getf plist :timings))
          (interrupt (getf plist :interrupt))
          (rebound (getf plist :rebound))
          (kill (getf plist :kill))
          (aftereffect (getf plist :aftereffect)))
      ;; Clear any prior settings from previous problem loads
      (setf (symbol-plist object) nil)
      ;; Generate and store events array
      (setf (get object :events)
            (generate-patroller-events object path mode timings))
      ;; patrollers always repeat their path
      (setf (get object :repeat) t)
      ;; Store patroller-specific properties for Phase 2 rebound support
      (setf (get object :patroller-path) path)
      (setf (get object :patroller-mode) mode)
      ;; Register as happening for compile-all-functions
      (push object *happening-names*)
      ;; Handle interrupt (same pattern as define-happening)
      (when interrupt
        (setf (get object :interrupt) interrupt)
        (let (($vars (get-all-nonspecial-vars #'$varp interrupt)))
          (setf (get object :interrupt-lambda)
                `(lambda (state)
                   (let ,$vars
                     ,(when $vars
                        `(declare (ignorable ,@$vars)))
                     ,(translate interrupt 'pre)))))
        (fix-if-ignore '(state) (get object :interrupt-lambda)))
      ;; Handle rebound condition (deferred compilation like interrupt)
      (when rebound
        (setf (get object :rebound) rebound)
        (let (($vars (get-all-nonspecial-vars #'$varp rebound)))
          (setf (get object :rebound-lambda)
                `(lambda (state)
                   (let ,$vars
                     ,(when $vars
                        `(declare (ignorable ,@$vars)))
                     ,(translate rebound 'pre)))))
        (fix-if-ignore '(state) (get object :rebound-lambda)))
      ;; Handle kill condition (checked when happening fires, prunes state)
      (when kill
        (setf (get object :kill) kill)
        (let (($vars (get-all-nonspecial-vars #'$varp kill)))
          (setf (get object :kill-lambda)
                `(lambda (state)
                   (let ,$vars
                     ,(when $vars
                        `(declare (ignorable ,@$vars)))
                     ,(translate kill 'pre)))))
        (fix-if-ignore '(state) (get object :kill-lambda)))
      ;; Handle aftereffect update (executed after happening fires)
      (when aftereffect
        (setf (get object :aftereffect) aftereffect)
        (let (($vars (get-all-nonspecial-vars #'$varp aftereffect)))
          (setf (get object :aftereffect-lambda)
                `(lambda (state)
                   (let (updated-dbs followups ,@$vars)
                     (declare (ignorable updated-dbs followups ,@$vars))
                     ,(translate aftereffect 'pre)
                     updated-dbs))))
  (fix-if-ignore '(state) (get object :aftereffect-lambda))))))


;;; ============================================================================
;;; PATROLLER MACRO
;;; ============================================================================


(defmacro define-patroller (object &rest slots)
  "Define a path-following patroller object.
   
   Syntax:
     (define-patroller <object>
       path <area-list>
       mode <:reverse | :cycle>
       [timings <time-list>]
       [interrupt <condition>]
       [rebound <condition>]
       [kill <condition>]
       [aftereffect <update-form>])
   
   Arguments:
     path        - Required. List of areas defining the path (minimum 2).
     mode        - Required. :reverse for ping-pong, :cycle for wrap-around.
     timings     - Optional. List of durations for each segment. Default: all 1s.
     interrupt   - Optional. Condition that pauses patroller movement.
     rebound     - Optional. Condition that reverses direction.
     kill        - Optional. Condition that prunes the state from search.
     aftereffect - Optional. Update form executed after patroller moves.
   
   The patroller's initial location must be specified via define-init.
   The path traversal begins from that location.
   
   Example:
     (define-patroller buzzer1
       path (area1 area2 area3 area4 area5)
       mode :reverse
       rebound (exists (?c cargo)
                 (and (loc buzzer1 $area)
                      (loc ?c $area)))
       aftereffect (propagate-changes!))"
  `(install-patroller ',object ',slots))
