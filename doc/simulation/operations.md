# Talos Problem-Solving Session — Operations

How D and Claude (A) set up and run a session to solve a new Talos puzzle together, with the
game running on D's computer.  First used on *Intersections* (problem `intersect`,
2026-11-10; record in `doc/problems/intersect/Briefing.md`).

This file is the entry point: D points A at it to begin a session on a new problem.  It
holds the method only.  All progress on a problem is recorded in that problem's own
directory, `doc/problems/<problem-name>/`, the same place and layout used for problems
solved by reference to a problem specification (see *Records* below).

A new problem needs more exploring than intersect did on the day it was solved: there the
layout, a spec and most of a plan already existed.  Expect the survey and the
propose-and-try loop below to take most of the session.


## Roles

- **D aims and acts.**  D turns, walks, picks up, selects and places in the game.
- **A watches and suggests.**  A reads the screen by screenshot, keeps the picture of the
  level and of the beams, proposes the next few actions with what should happen, and
  explains what went wrong when something else happens.
- A does not drive the game.  A's key presses arrive (W, A, S, D move; E picks up), but its
  pointer moves mostly do not turn the view, so A cannot aim.  If the game ever offers
  keyboard bindings for looking around, this can be revisited.


## Setup

D:
1. Start *The Talos Principle Reawakened*, enter the puzzle, and leave the game in front.
2. Open a session in the Claude desktop app on the same computer, with the wouldwork folder
   connected and computer use turned on.
3. Tell A the puzzle's name and its problem name: the existing one under `doc/problems/`,
   or the name to use for a new problem.

A:
1. Read this file, the project memory preferences, and any existing records in
   `doc/problems/<problem-name>/` (`Briefing.md`, `Handoff.md`, the spec, the diagram).
   Approach the puzzle as new even when records exist.  For a new problem, create
   `doc/problems/<problem-name>/` with a `Briefing.md` and a `Handoff.md` once D has given
   the name.
2. Load the computer-use skill, then ask for access to the game.  Request both entries: the
   app *The Talos Principle Reawakened* and the running program
   `...\Talos1\Binaries\Win64\Talos1-Win64-Shipping.exe` (the screenshot is masked without
   the second).
3. Take one screenshot, say what it shows, and confirm with D that both see the same thing.
4. Release the computer-use lock at the end of every turn, so D has the mouse and keyboard.


## Session flow

### 1. Survey the level

Goal: a shared description precise enough to reason about beams.

- D walks each area that can be reached and stops at each fixture; A takes a screenshot at
  each stop and writes down what it sees.
- Record, in the problem's own terms: areas and what joins them (gates, ladders, windows);
  walls and window spans; every transmitter and receiver with its colour and the way it
  faces; connectors and other objects with their starting places; plates; what each gate
  is controlled by; the goal.
- Areas behind closed gates are surveyed later, as they open.  Mark them unknown, not
  empty.
- Walk the whole of every area, corners and side strips included.  In intersect the
  solution ran through a strip of the first room that had no named location, and every
  impossibility argument had silently left it out.
- If D has a diagram, A checks it against the screenshots.  If not, D may draw one, or A
  keeps the description as a list.
- Name places as D names them (location1, gate2, window3).  No invented region labels.

### 2. Agree the facts and the goal

A lists the facts as numbered items and D corrects them.  Mark each as *seen* (screenshot),
*told* (D), or *assumed*.  Game rules count as facts too: what blocks a beam, what a carried
connector does, what crossing beams do.  When a rule matters and is only assumed, D tests it
in the level before the plan depends on it.

### 3. Find the forced structure

Before trying actions, A works out what any solution must do: the order in which gates
must be passed, which colour each receiver needs and where that colour can come from, which
objects are reachable when, and which beams must cross.  A states the main difficulty in
one or two sentences and agrees it with D.

### 4. Propose and try, one chunk at a time

- A gives one chunk: three to eight numbered actions that end at a checkable point (a gate
  open, D standing at a named place).  One recommended option, not a list of alternatives.
- Each chunk says what to expect (which gate opens, which connector lights, where beams
  should meet) and what to report if it does not happen.
- D performs the chunk and replies with where things stand ("I'm at location2 holding the
  connector", "gate2 is closed").
- A takes a screenshot when D reports a surprise, when the state is worth confirming, or
  when D asks "can you see why?".  A routine "done" needs no screenshot.
- When something fails, A explains it from what the screen shows, then gives the smallest
  change that tests the explanation.
- For a new problem many chunks are experiments, not plan steps: "put the connector here
  and tell me what lights".  Say which kind each chunk is.

### 5. When every route seems closed

Build a premise ledger: every premise the dead end rests on, each marked seen, told,
measured or assumed.  Examine the weakest first, one at a time, with D testing in the
level.  A premise of the form "only from" or "only via" is unmeasured until every walkable
area has been checked for it.  A solution exists, so an impossibility means a premise or a
rule is wrong.

### 6. Record

During the session A keeps the problem's `Briefing.md` current: facts, each chunk's result,
surprises, and game rules learned.  At the end A also writes the full action list as
played, in a form D can print, and brings `Handoff.md` up to date.  See *Records*.


## Records

Everything about one problem lives in `doc/problems/<problem-name>/`, whether it is being
solved in the game, by the engine, or both.  The files are the ones the advisors already
use (`doc/solving-advisor.md`, *Records and templates*):

| file | holds |
|---|---|
| `Briefing.md` | the running record: survey, agreed facts, each chunk and its result, premise ledgers, game rules learned, the result |
| `Handoff.md` | a short current status, so a fresh session can continue: next step, state, how to restore, open items |
| `<problem-name>-diagram.png` | D's diagram of the level, when there is one |
| `Actions.lisp`, `Validation.txt` | the accepted action list and its engine validation, once a spec exists |
| `problem-analysis.txt`, `spec-analysis.txt`, `search-analysis.txt` | the problem, spec and search advisors' records, when those advisors are used |

The problem spec itself, when one is written, is `probs/problem-<problem-name>.lisp`.

What does not go in the problem's directory:
- differences between the game and the Wouldwork engine: entered as needs in
  `doc/constraint-led-solving/Schema-Gaps.txt` (implementation only with D's approval);
- method lessons that apply to other puzzles: added to this file, or to
  `doc/solving-advisor.md` when they concern engine-based solving.

Keep only the final version of a problem and its solution; lessons stay in the Briefing.


## Reading the screen

- A solid coloured line is a live beam.  A dashed white line is a pairing with no light in
  it: the connector is dark, and the line cuts nothing.
- A bright spark marks two beams cutting each other; both stop there.
- A lit connector glows its beam's colour; a lit receiver glows; an unlit one is dull.
- A gate shows as a coloured field between posts when closed.
- An orange outline of a tripod is the placement preview while D holds a connector.
- Top-left icons show collected sigils, not puzzle state.


## Game behaviour worth knowing (from intersect)

- **Two stable arrangements.**  When a connector's own beam would protect its feed from a
  crossing beam, the level can sit either way: connector lit and protecting itself, or
  dark with its feed cut.  The game keeps whichever came first, so a connector placed into
  an already-cut feed stays dark.  To switch it, interrupt the cutting beam for a moment
  (D stands in it), wait for the connector to light, then step out.
- **A gate held by your own body.**  Standing in a beam can open a gate, but do not plan to
  walk from the beam to that gate: arrange a beam that keeps it open after you step away.
  (What the game does if you try is not yet observed.)
- **Bodies block beams.**  The agent and placed connectors stop a beam that passes through
  them, which is both a tool and a hazard when choosing where to stand or place.
- **Carried connectors** keep their pairings but send no beam until put down.
- **Windows** pass beams but not the agent, reach or objects.
- **Closed gates** block beams but not selection.


## Using the Wouldwork engine alongside (optional)

When a spec exists or D wants one, the engine can check or search for plans between game
trials.  Follow `doc/solving-advisor.md`; set the engine up in the cloud container per
`AGENTS.md`, *Running Wouldwork in a Claude cloud session*.  Keep in mind:
- an engine replay is evidence about the model, not about the game; play decides;
- search sees only the locations in the spec, so a missing location looks like an
  impossible problem;
- a coordinate-driven spec has a limit of 999 planning objects, which about ten locations
  can reach (crossing records);
- the engine chooses between two stable beam arrangements from the state alone, so a plan
  may need an extra beam-interrupting step in the game.


## D's preferences for these sessions

- Concise explanations in the puzzle's own terms.
- One issue at a time, one question at a time, one recommended option.
- Say *selection*, *pairing* or *beam*; never "link".
- Record findings in the Briefing and keep the Handoff current.
- A suggests a fresh session when the context grows long.
