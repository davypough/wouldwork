# holding-the-trigger — Handoff
Updated 2026-11-12 (fifth session).  Status: analysis in progress, in-game; no spec, no
draw.io diagram.
Record: doc/problems/holding-the-trigger/Briefing.md (layout, timing, rules learned, failed
attempts, plans ruled out).  Photos photo1–photo7 in the same directory; photo7 is a usable
plan view (north up).

## Next step
1. D tests an unrecorded rule (Briefing, *Count argument*, end of *Session 2026-11-12
   (fifth)*): a connector on the platform paired to both transmitters, the blue receiver
   and a top connector that is paired to the red receiver.  Which hue does it carry, and
   what happens when each transmitter's beam is blocked for a moment and released?
   Closed (D, tested 2026-11-13): the red transmitter cannot be seen from anywhere in the
   target gate area.  It can be seen from the platform (open question 2 answered).
   Closed (D, tested 2026-11-13): the agent can land in the target gate area holding the
   box, but the airstream is within reach only at the launch point.
   Closed (D, tested 2026-11-13): the ride works while carrying the box, but the box cannot
   be put on the blower from the roof.
   Closed (D, tested): no red line passes over the platform, so the plan *pick up the
   platform connector as the trigger* is out.  Its record follows.
   Sightline check for the plan *pick up the platform connector as the trigger*, as revised
   in the Briefing, *Session 2026-11-12 (fifth)*: a red roof connector cannot see the blue
   receiver (D), so the platform connector lights the blue receiver itself and the roof
   connector carries red only.  Needed: one platform spot that lies on the line from the
   red transmitter to a roof spot seeing the red receiver, and that also sees the blue
   transmitter and the blue receiver.  The candidate line crosses the platform's south end,
   where the western tops' north columns stand; not settled by camera.  Quickest test is D
   in the game: connector on the roof north of the eastern top beside the slab, paired to
   the red transmitter; see whether its beam passes over the platform.
2. Still unsurveyed: the inside of the enclosed area at the corridor entrance; the lawn
   beyond the target gate.
3. Build `holding-the-trigger.drawio` from photo7 and the survey notes.

## Tested this session (details in the Briefing)
- The east roof cannot be reached from the box on the stopped blower, nor can the red
  receiver's pillar.  The ride is the only way up.
- Rule 6: a connector fed directly by a transmitter takes that hue over a relayed hue.
- Rule 7: blue on the red receiver does not run the blower.
- From the target gate area the agent can pick a connector up from the platform but not
  place one on it.
- A roof spot sees both the red transmitter and the red receiver; from it the agent runs to
  the target gate in about 3 s.

## Open questions for D
1. Can the agent reach the eastern top by riding the blower without the box?
2. Is the red transmitter visible from the platform?
3. Can a box put on the platform from the target gate area be taken down from the main
   area side?

Answered: the corridor is only a passage (two openings; fences stop beams).

## Restore
Game state is D's.  No engine state.

## Open items
- No layout diagram yet.
- Engine gap: no tech yet for a hit-activated target that opens a gate briefly.  Model it as
  one combined action: box on blower + agent in the gate zone + receiver lit by the agent's
  act → agent passes the target gate.  Add a need to doc/open-issues.txt when a spec is
  started.
- Game rules 1–7 in the Briefing.  Check them against tech/angled-blower.lisp and
  tech/beam-relay.lisp (rule 6 differs from "first hue to arrive") before writing a spec.
- Method notes for operations.md: ground and camera controls, and the Claude window taking
  the keyboard (Briefing, fourth session, *Controls*).
