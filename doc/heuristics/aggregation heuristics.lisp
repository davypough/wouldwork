;;; Aggregation Heuristics

;;; These apply a heuristic function across a collection and combine results.


(defun h-sum-over (items heuristic-fn)
  "Sum heuristic values over collection."
  (reduce #'+ items :key heuristic-fn :initial-value 0))

(defun h-max-over (items heuristic-fn)
  "Maximum heuristic value over collection."
  (reduce #'max items :key heuristic-fn :initial-value 0))

(defun h-min-over (items heuristic-fn)
  "Minimum heuristic value over collection (for closest target)."
  (reduce #'min items :key heuristic-fn :initial-value most-positive-fixnum))


;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Total Manhattan distance for all tiles
(define-query h-total-manhattan ()
  (do (setq $sum 0)
      (doall (?t tile)
        (incf $sum (h-tile-manhattan ?t)))
      $sum))


;;; Distance to nearest connector
(define-query h-distance-to-nearest-connector ()
  (do (setq $min most-positive-fixnum)
      (bind (loc agent1 $agent-area))
      (mvsetq ($ax $ay $az) (get-fixed-coordinates $agent-area))
      (doall (?c connector)
        (if (not (holds agent1 ?c))
          (do (mvsetq ($cx $cy $cz) (get-coordinates ?c))
              (setq $d (manhattan-distance $ax $ay $cx $cy))
              (if (< $d $min)
                (setq $min $d)))))
      (if (= $min most-positive-fixnum) 0 $min)))