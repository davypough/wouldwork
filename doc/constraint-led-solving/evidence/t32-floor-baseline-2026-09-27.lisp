("
  contract floor-blower
    controls  its CONTROLS entry; turning == the control aggregate while no jammer jams it (S1 device state axiom), read in each object's own view
    moves     every non-fan occupant resting ON the blower, with its stack, to the AIMED-AT destination while the blower turns in that occupant's view; a fan resting on it is toppled at the source
    requires  an occupant ON the blower (an agent mounts it by step); when no floor drive aimed at the destination turns in its view, an occupant there not ON a support falls back to the blower's location -- keep the lift by leaving through an exit arc, or standing on a support there, before the stream stops
    blower1
      source       location4
      destination  location20
      control      ((switch1)) normal
      exits from location20 (21), by mode; each mode's own predicate is not evaluated
        jumping (2): location5, location6 ((gate2))
        walking (19): location1 ((gate1)), location10 ((gate3)), location11 ((gate3 gate5)), location12 ((gate3 gate5)), location13 ((gate3 gate5)), location14 ((gate3 gate5 gate6 screen1)), location15 ((gate3 gate5 gate7)), location16 ((gate3 gate5 gate7 gate8)), location17 ((gate3 gate5 gate7 gate8)), location18 ((gate3 gate5 gate7 gate8)), location19 ((gate3 gate5 gate7 gate8 gate9)), location2 ((gate1)), location21 ((gate3 gate5 gate7 gate8)), location3, location4, location5, location7, location8 ((gate3)), location9 ((gate3))
"
 ((LIFT)
  ((SWITCH1 (CONTROLS ((SWITCH1)) BLOWER1 NORMAL) LOCATION20 (CONTROLS ((SWITCH1)) GATE2 INVERTED)
    ((TRAVERSE-VIA JUMPING LOCATION20 ((GATE2)) LOCATION6))))))