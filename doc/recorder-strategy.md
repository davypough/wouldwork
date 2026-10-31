# Planning a recorder strategy

This tutorial develops a procedure for planning how to use the recorder to
achieve an intermediate goal. We begin by deciding what to try to accomplish;
later steps will address the setup, recording, and coordinated playback needed
to accomplish it.

The procedure is being developed one step at a time. Step 1 identifies
candidate goals. Step 2 makes a candidate's required outcome precise. These
steps do not yet provide a method for choosing among candidates.

## Step 1: What should I try to accomplish next?

Look for an opportunity and consider what you could accomplish by taking
advantage of it. Distinguish three things:

1. **Identify an opportunity.** Recognize something that could become possible.
   For example, opening a gate would allow the live agent to enter another
   region.
2. **Create the opportunity.** Establish the conditions that make it available.
   For example, use the recorder so that a ghost can stand on the plate that
   opens the gate.
3. **Take advantage of the opportunity.** Use those conditions to accomplish
   something. For example, move the live agent through the gate into the newly
   accessible region.

This is a generic gate-and-plate example, not a description of the numbered
gates and plates in the current rumin-topo problem.

### Turn opportunities into candidate goals

Identifying an opportunity is a prelude to using it. Creating the opportunity
allows an action to be taken; taking advantage of it carries the attempt
further. When choosing an intermediate goal, ask:

> What resulting state would represent having taken advantage of this
> opportunity?

In the example, an open gate creates an opportunity to cross. The live agent
being beyond the gate represents having used that opportunity. This gives us
a candidate intermediate goal: get the live agent into the region beyond the
gate.

An opportunity may offer several ways to take advantage of it. Depending on
the starting state and the puzzle, possibilities might include:

- Crossing with the agent alone.
- Carrying a needed object across.
- Reaching one of several newly accessible places where the agent can act.

Each resulting state can be a candidate goal. Describe the intended result
and why it appears relevant to the overall puzzle goal. There is no need at
this step to specify the recording actions or every detail of the resulting
state.

### How far should this first objective extend?

Plan far enough to obtain the intended benefit of the opportunity. If that
benefit is access to another region, crossing the gate is a reasonable
candidate objective. Opening a second gate need not be included merely to
make the first crossing count as progress.

This does not establish that the candidate is the best choice, or that it can
be planned independently of later tasks. It is a promising attempt at
progress: a concrete benefit worth investigating. Later examination may show
that it needs different preparation or must be considered together with a
further objective.

### Result of this step

For each opportunity under consideration, write down:

- **Opportunity:** What could become possible?
- **Conditions to create it:** What would make that opportunity available?
- **Candidate intermediate goals:** What states could result from taking
  advantage of it, and why might they help solve the puzzle?

Keep these as candidates until they have been examined. Generating possible
goals and choosing among them are separate parts of planning.

## Step 2: What must be true for the intended benefit to have been obtained?

Take one candidate goal and state the conditions that would count as achieving
it. These are the **required outcome**. They describe what we want from a plan,
without specifying everything that will be true when the plan finishes.

The **resulting state** is different: it is what actually follows from the
starting state and a particular sequence of actions. We can derive that state
once we have a plan. Different plans can meet the same required outcome while
leaving other things in different conditions.

> Specify the required outcome before planning; derive the resulting state
> from the plan.

### Example: get the live agent through a gate

Continue the generic gate-and-plate example from Step 1. Assume the gate is
open exactly while its pressure plate is occupied. The plate is on the near
side, and the live agent needs to reach a work area on the far side. There is
no available object to leave on the plate, so the proposed assistance is a
ghost standing on it during playback.

For this example, assume the ghost can reach and leave the plate independently,
and that leaving it does not move the live agent back across the gate. We are
comparing playback outcomes here, not yet specifying a complete recording or
how the recorder cycle ends.

The candidate objective is to gain access to the far-side work area. Its
required outcome is:

> The live agent is in the far-side work area.

Now consider two possible playback sequences from the same starting state:

| Sequence | Actions in order | Result at the chosen endpoint |
| --- | --- | --- |
| A | Ghost steps onto the plate; live agent crosses into the work area. | Live agent is in the work area; ghost is on the plate; gate is open. |
| B | Ghost steps onto the plate; live agent crosses into the work area; ghost steps off the plate. | Live agent is in the work area; ghost is off the plate; gate is closed. |

Both sequences achieve the required outcome. Sequence A ends while the
assistance is still being provided. Sequence B includes its withdrawal after
the crossing. The gate's condition follows from those actions; it is not
determined by the objective of getting the live agent across.

In particular, the gate must be open when the crossing requires passage. In
Sequence A it also remains open immediately after the crossing, because
nothing has removed the ghost from the plate. That is a consequence of this
sequence, not a condition every successful plan must leave true.

### When should the gate's final condition be required?

Ask what benefit the chosen objective is intended to provide. If the benefit
is simply getting the live agent into the work area, either gate condition is
acceptable for that objective.

If the intended benefit also includes leaving an open passage for an object
that must subsequently be brought through, then the required outcome needs
to say so. It could require both the live agent in the work area and the gate
open at the endpoint. Keeping the passage open until the object crosses is an
additional requirement over the continuation; a single final-state condition
does not guarantee that duration.

Thus, do not add a condition to the goal merely because one proposed plan
leaves it true. Add it because the intended benefit requires it. Conversely,
leaving a condition unspecified does not mean it has no consequences: the
actual resulting state will matter when we plan what to do next.

### Result of this step

For the candidate under examination, write down:

- **Required outcome:** The conditions that must hold for the intended
  benefit to have been obtained.
- **Conditions left unspecified:** Details, such as the gate's final
  condition in the example, that this objective does not require either way.

These requirements usually describe several acceptable states. Do not choose
an exact location, equipment arrangement, or ghost position unless the
intended benefit calls for it. As planning proceeds, derive the actual
resulting state and reconsider the requirements if it reveals a missing need.
