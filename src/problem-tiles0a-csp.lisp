;;; Filename: problem-tiles0a-csp.lisp

;;; Related to Shifting Mosaic from Islands of Insight (most NW corner island)
;;; Pack tiles in a 5x5 array with no leftover spaces


(in-package :ww)

(ww-set *problem-name* tiles0a-csp)

(ww-set *problem-type* csp)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)


(define-types
  tile (GUN1 GUN2 VERT Z CANE SQ1 SQ2 L HOCK)
  void (EMPTY)  ;the empty object can be thought of as the collection of empty coordinates
  object (either tile void))


(define-dynamic-relations
  (loc tile $list)  ;current reference coord of a tile
  (remaining object $list))   ;list of coords--eg, ((0 2) ...)


(define-query get-remaining (?obj)
  (do (bind (remaining ?obj $coords))
      $coords))


(defun additional-coord (coord drow dcol)
  "Returns a new coord in relation to the input coord."
  (list (+ (first coord) drow) (+ (second coord) dcol)))


(defun remove-coords (coords list)
  "Removes coords from a list of coords, keeping same ordering."
  (remove-if (lambda (x) (member x coords :test #'equal)) list))


;Make one rule for each tile, largest tiles first


(define-action put-Z
  1
  (?Z-coord (get-remaining Z))  ;possible places Z can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?Z-coord $empty-coords :test #'equal)
       (setf $1st-coord (additional-coord ?Z-coord 1 0))
       (member $1st-coord $empty-coords :test #'equal)
       (setf $2nd-coord (additional-coord ?Z-coord 1 1))
       (member $2nd-coord $empty-coords :test #'equal)
       (setf $3rd-coord (additional-coord ?Z-coord 2 1))
       (member $3rd-coord $empty-coords :test #'equal))
  (?Z-coord)
  (assert (setf $taken-coords (list ?Z-coord $1st-coord $2nd-coord $3rd-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc Z ?Z-coord)
          (remaining Z nil)))


(define-action put-CANE
  1
  (?CANE-coord (get-remaining CANE))  ;possible places CANE can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?CANE-coord $empty-coords :test #'equal)
       (setf $1st-coord (additional-coord ?CANE-coord 0 1))
       (member $1st-coord $empty-coords :test #'equal)
       (setf $2nd-coord (additional-coord ?CANE-coord 1 0))
       (member $2nd-coord $empty-coords :test #'equal)
       (setf $3rd-coord (additional-coord ?CANE-coord 2 0))
       (member $3rd-coord $empty-coords :test #'equal))
  (?CANE-coord)
  (assert (setf $taken-coords (list ?CANE-coord $1st-coord $2nd-coord $3rd-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc CANE ?CANE-coord)
          (remaining CANE nil)))


(define-action put-HOCK
  1
  (?HOCK-coord (get-remaining HOCK))  ;possible places HOCK can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?HOCK-coord $empty-coords :test #'equal)
       (setf $1st-coord (additional-coord ?HOCK-coord 1 0))
       (member $1st-coord $empty-coords :test #'equal)
       (setf $2nd-coord (additional-coord ?HOCK-coord 2 -1))
       (member $2nd-coord $empty-coords :test #'equal)
       (setf $3rd-coord (additional-coord ?HOCK-coord 2 0))
       (member $3rd-coord $empty-coords :test #'equal))
  (?HOCK-coord)
  (assert (setf $taken-coords (list ?HOCK-coord $1st-coord $2nd-coord $3rd-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc HOCK ?HOCK-coord)
          (remaining HOCK nil)))


(define-action put-GUN1
  1
  (?GUN1-coord (get-remaining GUN1))  ;possible places GUN1 can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?GUN1-coord $empty-coords :test #'equal)
       (setf $1st-coord (additional-coord ?GUN1-coord 0 1))
       (member $1st-coord $empty-coords :test #'equal)
       (setf $2nd-coord (additional-coord ?GUN1-coord 1 0))
       (member $2nd-coord $empty-coords :test #'equal))
  (?GUN1-coord)
  (assert (setf $taken-coords (list ?GUN1-coord $1st-coord $2nd-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc GUN1 ?GUN1-coord)
          (remaining GUN1 nil)))


(define-action put-GUN2
  1
  (?GUN2-coord (get-remaining GUN2))  ;possible places GUN2 can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?GUN2-coord $empty-coords :test #'equal)
       (setf $1st-coord (additional-coord ?GUN2-coord 0 1))
       (member $1st-coord $empty-coords :test #'equal)
       (setf $2nd-coord (additional-coord ?GUN2-coord 1 0))
       (member $2nd-coord $empty-coords :test #'equal))
  (?GUN2-coord)
  (assert (setf $taken-coords (list ?GUN2-coord $1st-coord $2nd-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc GUN2 ?GUN2-coord)
          (remaining GUN2 nil)))


(define-action put-L
  1
  (?L-coord (get-remaining L))  ;possible places L can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?L-coord $empty-coords :test #'equal)
       (setf $1st-coord (additional-coord ?L-coord 1 0))
       (member $1st-coord $empty-coords :test #'equal)
       (setf $2nd-coord (additional-coord ?L-coord 1 1))
       (member $2nd-coord $empty-coords :test #'equal))
  (?L-coord)
  (assert (setf $taken-coords (list ?L-coord $1st-coord $2nd-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc L ?L-coord)
          (remaining L nil)))


(define-action put-VERT
  1
  (?VERT-coord (get-remaining VERT))  ;possible places VERT can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?VERT-coord $empty-coords :test #'equal)
       (setf $1st-coord (additional-coord ?VERT-coord 1 0))
       (member $1st-coord $empty-coords :test #'equal))
  (?VERT-coord)
  (assert (setf $taken-coords (list ?VERT-coord $1st-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc VERT ?VERT-coord)
          (remaining VERT nil)))


(define-action put-SQ1
  1
  (?SQ1-coord (get-remaining SQ1))  ;possible places SQ1 can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?SQ1-coord $empty-coords :test #'equal))
  (?SQ1-coord)
  (assert (setf $taken-coords (list ?SQ1-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc SQ1 ?SQ1-coord)
          (remaining SQ1 nil)))


(define-action put-SQ2
  1
  (?SQ2-coord (get-remaining SQ2))  ;possible places SQ2 can be put
  (and (bind (remaining EMPTY $empty-coords))
       (member ?SQ2-coord $empty-coords :test #'equal))
  (?SQ2-coord)
  (assert (setf $taken-coords (list ?SQ2-coord))
          (doall (?obj object)
            (do (bind (remaining ?obj $current-coords))
                (remaining ?obj (remove-coords $taken-coords $current-coords))))
          (loc SQ2 ?SQ2-coord)
          (remaining SQ2 nil)))


(define-init  ;possible coords of most-upper most-left corner of a tile
  `(remaining SQ1  ,(loop for row from 0 to 4 append (loop for col from 0 to 4 collect (list row col))))
  `(remaining SQ1  ,(loop for row from 0 to 4 append (loop for col from 0 to 4 collect (list row col))))
  `(remaining VERT ,(loop for row from 0 to 3 append (loop for col from 0 to 4 collect (list row col))))
  `(remaining L    ,(loop for row from 0 to 3 append (loop for col from 0 to 3 collect (list row col))))
  `(remaining GUN1 ,(loop for row from 0 to 3 append (loop for col from 0 to 3 collect (list row col))))
  `(remaining GUN2 ,(loop for row from 0 to 3 append (loop for col from 0 to 3 collect (list row col))))
  `(remaining Z    ,(loop for row from 0 to 2 append (loop for col from 0 to 3 collect (list row col))))
  `(remaining CANE ,(loop for row from 0 to 2 append (loop for col from 0 to 3 collect (list row col))))
  `(remaining HOCK ,(loop for row from 0 to 2 append (loop for col from 1 to 4 collect (list row col))))
  `(remaining EMPTY ,(loop for row from 0 to 4 append (loop for col from 0 to 4 collect (list row col)))))


(define-goal
  (and (bind (remaining EMPTY $empty-coords))
       (null $empty-coords)
       (bind (loc Z $Z-coord))
       (bind (loc HOCK $HOCK-coord))
       (<= (second $HOCK-coord) (second $Z-coord))
       (bind (loc CANE $CANE-coord))
       (< (second $CANE-coord) (second $HOCK-coord))))
       
