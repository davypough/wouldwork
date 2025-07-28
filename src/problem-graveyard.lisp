;;;; Filename: problem-graveyard.lisp


;;; Problem specification for graveyard problem in Dream game.


(in-package :ww)  ;required

(ww-set *problem-name* graveyard)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *depth-cutoff* 10)

(ww-set *progress-reporting-interval* 2000000)


(define-types
  grave (grave1 grave2 grave3 grave4 grave5 grave6 grave7 grave8 grave9 grave10 grave11 grave12))


(define-dynamic-relations
  (on grave))


(define-update toggle! (?graveA ?graveB ?graveC)
  (do (if (on ?graveA)
        (not (on ?graveA))
        (on ?graveA))
      (if (on ?graveB)
        (not (on ?graveB))
        (on ?graveB))
      (if (on ?graveC)
        (not (on ?graveC))
        (on ?graveC))))


(define-action move  ;toggle a grave light on or off
    1
  (?grave grave)
  (always-true)
  (?grave)
  (assert (case ?grave
            (grave1 (toggle! 'grave1 'grave3 'grave10))
            (grave2 (toggle! 'grave2 'grave5 'grave7))
            (grave3 (toggle! 'grave2 'grave3 'grave10))
            (grave4 (toggle! 'grave1 'grave4 'grave11))
            (grave5 (toggle! 'grave5 'grave9 'grave12))
            (grave6 (toggle! 'grave4 'grave6 'grave8))
            (grave7 (toggle! 'grave6 'grave7 'grave11))
            (grave8 (toggle! 'grave1 'grave8 'grave9))
            (grave9 (toggle! 'grave3 'grave9 'grave12))
            (grave10 (toggle! 'grave2 'grave7 'grave10))
            (grave11 (toggle! 'grave4 'grave6 'grave11))
            (grave12 (toggle! 'grave5 'grave8 'grave12)))))


(define-init
  (on grave2) (on grave3) (on grave4) (on grave6) (on grave8) (on grave10))


(define-goal
  (and (on grave1) (on grave2) (on grave3) (on grave4) (on grave5) (on grave6) (on grave7) (on grave8) (on grave9) (on grave10) (on grave11) (on grave12)))
