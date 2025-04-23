;;; Filename: problem-daydream.lisp

;;; Problem specification for the daydream problem
;;; in Talos Principle "In the Beginning"


(in-package :ww)  ;required

(ww-set *problem-name* daydream)

(ww-set *problem-type* planning)

(ww-set *depth-cutoff* 15)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)



(define-types
  me          (me1 me2)  ;me1 is primary agent, me2 is the clone of me1
  connector   (connector1 connector2 c-connector1 c-connector2)  ;main connectors plus clone connectors
  gate        (gate1 gate2 gate3)
  barrier     (barrier1)  ;objects cannot pass through, only agent
  trapdoor    (trapdoor1)
  plate       (plate1 plate2)
  fan         (fan1)
  transmitter (transmitter1)
  receiver    (receiver1 receiver2 receiver3 receiver4)
  recorder    (recorder1)
  area        (area1 area2 area3 area4 area5 area6)
  terminus    (either transmitter receiver connector))


(define-dynamic-relations
  (holding me $symbol)  ;$symbol is a connector
  (loc (either me connector plate) $symbol)  ;$symbol is an area
  (on (either me connector) $symbol)  ;$symbol is a plate or fan
  (connects connector connector)
  (active (either connector gate receiver))
  (targets connector (either transmitter receiver))
  (recording)


(define-static-relations
  (adjacent area area)  ;can move unconditionally between areas
  (reachable area area)  ;can move between areas if not holding anything
  (controls (either receiver plate) (either gate fan))
  (separates (either gate barrier) area area)
  (los area (either transmitter receiver))  ;clear los from an area to something


;;; QUERIES ;;;


(define-query reachable? (?area1 ?area2)
  (or (adjacent ?area1 ?area2)
      (exists (?g gate)
        (and (separates ?g ?area1 ?area2)
             (not (active ?g))))))


  

(define-query cleartop? (?plate)
  (not (exists (?b box)
         (on ?b ?plate))))


(define-query open? (?gate ?area1 ?area2)
  (and (separates ?gate ?area1 ?area2)
       (exists (?p plate)
         (and (controls ?p ?gate)
              (exists (?b box)
                (on ?b ?p))))))


;;; UPDATES ;;;


(define-update disable-connector! (?connector)
  (if (bind (hue ?connector $hue))
    (deactivate-connector! ?connector)
    (disconnect-connector! ?connector))


(define-update disconnect-connector! (?connector)
  (do (bind (connects ?connector $targets))
      (not (connects ?connector $targets))))


(define-update deactivate-connector (?connector)
  (do (bind (connects ?connector $targets))
      (ww-loop for $target in $targets


(define-update disable-connector! (?connector)
      
        (if (receiver $target)
          (and 
               (if (bind (hue ?connector $hue))
                 (if (bind (controls $target $gate))
                   (not (active $gate)))))
          (if (transmitter $target)
            
                

          (if (transmitter $t)
                  (receiver


(if (bind (hue ?connector $hue))
        (do (not (hue ?connector $hue))
            (if 

;;;; ACTIONS ;;;;


(define-action move
    1
  (?area2 area)
  (and (if (recording)
         (setq $me me2)
         (setq $me me1))
       (bind (loc $me $area1))
       (different $area1 ?area2)
       (or (not (bind (holding $me $connector)))
           (not (separates barrier1 $area1 ?area2)))
       (reachable? $area1 ?area2))
  ($area1 ?area2)
  (assert (loc $me ?area2)))

        
(define-action pickup
    1
  (?connector connector)
  (and (if (recording)
         (setq $me me2)
         (setq $me me1))
       (not (bind (holding $me $connector))
       (bind (loc $me $area))
       (loc ?connector $area))
  (?connector)
  (assert (holding $me ?connector)
          (not (loc ?connector $area))
          (disable-connector! ?connector)
          (if (bind (hue ?connector $hue))
            (deactivate-connector! ?connector)
            (disconnect-connector! ?connector))


          (if (hue ?connector)  ;if it has a hue, its active
            (not (hue ?connector)))  ;deactivate it
          (doall (?t terminus)
            (if (connecting ?connector $t)
              (and (not connecting ?connector ?t)
                   (if (connector ?t)


            (chain-deactivate! ?connector $hue))
          (doall (?t terminus)
            (if (connecting ?connector ?t)
              (not (connecting ?connector ?t))))))





(define-action drop
    1
  (?box box ?area area)
  (and (loc me ?area)
       (holding me ?box))
  (?box ?area)
  (assert (loc ?box ?area)
          (not (holding me ?box))))


(define-action put
    1
  (?box box ?plate plate ?area area)
  (and (loc me ?area)
       (holding me ?box)
       (loc ?plate ?area)
       (cleartop? ?plate))
  (?box ?plate ?area)
  (assert (loc ?box ?area)
          (not (holding me ?box))
          (on ?box ?plate)))


(define-init
  ;dynamic
  (loc me area1)
  (loc box1 area1)    
  (loc box2 area2)
  ;static
  (loc plate1 area1)
  (loc plate2 area1)
  (loc plate3 area3)
  (controls plate1 gate1)
  (controls plate2 gate2)
  (controls plate3 gate3)
  (separates gate1 area1 area2)
  (separates gate2 area1 area3)
  (separates gate3 area3 area4))


(define-goal
  (loc me area4))
