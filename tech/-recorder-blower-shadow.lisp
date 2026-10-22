;;; Filename: -recorder-blower-shadow.lisp

;;; Recording-side state for the blower drives whose physics support recorder isolation:
;;; mountable wall gears, fixed wall blowers, and fixed floor blowers.  The fixed blower is
;;; not copied; ghost objects merely read this distinct activity state so playback-only
;;; activation cannot alter their recorded motion.
;;;
;;; Ghost-only control and jamming readings derive a distinct turning value, exposed
;;; through -recording-shadow-policy's turning-view hook.
;;;
;;; REQUIRES:
;;;   nested : -recorder-controls-shadow; -recorder-jamming-shadow;
;;;            -recording-shadow-policy (recording-shadow-turning neutral hook);
;;;            -propagation
;;; PROVIDES:
;;;   relation : recording-turning
;;;   query    : recording-shadow-turning override
;;;   update   : update-recording-blower-status!
;;;   lifecycle: reset clause for recording-turning

(include-tech -recorder-controls-shadow)
(include-tech -recorder-jamming-shadow)
(include-tech -recording-shadow-policy)
(include-tech -propagation)

(in-package :ww)


(define-optional-types floor-blower wall-gears wall-blower)


(define-types
  recording-blower-drive (either floor-blower wall-gears wall-blower))


(define-dynamic-relations
  (recording-turning recording-blower-drive))


(define-derived-relations
  recording-turning)


(define-query recording-shadow-turning (?drive)
  (and (recording-blower-drive ?drive)
       (recording-turning ?drive)))


(define-update update-recording-blower-status! ()
  ;; Supported drives evaluate their DNF against recording-side controllers.  A mapped
  ;; ghost jammer forces them stopped, while a mapped live jammer affects only playback.
  ;; The supported controller restriction remains an initialization policy in
  ;; -recorder-init-checks.
  (doall (?drive recording-blower-drive)
    (if (and (recording-control-on ?drive t)
             (not (recording-jammed ?drive)))
      (recording-turning ?drive)
      (not (recording-turning ?drive)))))


(register-recorder-shadow-lifecycle
  'blower-drive
  ;; Reset: clear blower-drive facts inherited from the preceding recording cycle.
  '(doall (?drive recording-blower-drive)
     (not (recording-turning ?drive))))
