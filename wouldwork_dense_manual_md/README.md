# Wouldwork Planning Software — Dense Manual (Unofficial Rewrite)

> Source: *Wouldwork Planning Software User Manual (26.7), 5th edition (2025)* by David Alan Brown.

This rewrite aims to be **hands-on and information-dense**. It keeps **all examples** from the original manual, but reorganizes the learning path:

- Start here: **README.md** (what it is + when to use it)
- Then: **QUICKSTART.md** (first successful plan run)
- Then: **TUTORIAL.md** (how to model problems)
- Then: **REFERENCE.md** (all knobs/options)
- Then: **FEATURES.md** (optional features)
- Then: **STRATEGIES.md** (how to solve different problem classes)
- Then: **PROBLEM-COOKBOOK.md** (appendix problems as recipes)

## What Wouldwork is

Wouldwork is a **classical planner / general problem solver**. You describe a world using:

- **Objects & types**
- **Relations (facts) about objects**
- **Actions** with preconditions and effects
- **Initial conditions**
- **Goal conditions** (and optional constraints)

The planner searches for a **plan** (a sequence of actions) that transforms the initial state into a goal state.

## When to use Wouldwork

Use it when your problem is mostly:

- **Discrete and symbolic** (states can be represented by facts/relations)
- **Deterministic** (actions have reliable effects)
- **Constraint-heavy** (you want explainable, rule-based constraints)
- **Explainability matters** (you want the actual plan and trace)

Typical fits:
- Puzzle-like domains (blocks world, jugs, peg solitaire)
- Scheduling with explicit constraints
- Search problems where you can clearly define operators (actions)

## When *not* to use Wouldwork (or when to be cautious)

Be cautious when your problem is dominated by:

- **Continuous control** (robotics control, PID-like problems)
- **Uncertainty / noise** (stochastic outcomes, perception errors)
- **Huge branching without strong structure** (state explosion)
- **Learning-from-data** is the main requirement (then you want ML/optimization)

A quick heuristic:

- If you can write down **actions** and **effects** clearly, and you want a plan: ✅
- If you mostly need to **estimate** unknown quantities from data: ❌ (use Bayesian/ML)
- If you have both: use Wouldwork for the **discrete decision skeleton**, and ML for scores/estimates.

## How to succeed (classical planning realities)

Most failures are modeling failures, not algorithm failures:

- **State explosion**: too many objects/relations/actions => search blows up
- **Underspecified actions**: missing effects/preconditions => planner can’t reach goal
- **Overconstrained goals**: impossible constraints => no solution exists

Debugging loop:
1. Start tiny: smallest objects, fewest actions, easiest goal
2. Validate one action at a time
3. Add constraints gradually (invariants, global constraints)
4. Only then scale up object counts

## Original manual sections (extracted)

The following sections are included verbatim-ish (formatting normalized) as expandable references.
The tutorial/docs above point into these when you need precise wording.

<details>
<summary><strong>Introduction (source extract)</strong></summary>

This is a user manual for the Wouldwork Planner (a.k.a. The I’d Be Pleased If You Would Work Planner, inspired by my grandson Elias). It covers how to download and install the software, how to write a problem specification, how to run the program, and how to interpret the program’s output. This manual is available in booklet or Kindle form for nominal cost at Amazon.com under the same name.

</details>

<details>
<summary><strong>Classical Planning (source extract)</strong></summary>

The typical classical planning problem takes place in a fully specified environment, in which an agent is attempting to plan out a sequence of actions to achieve a complex goal. The planning program, acting on behalf of the agent, analyzes numerous possible paths to the goal, and, if successful, presents a complete action plan from start to finish. Given the potentially large number of actions, environmental objects, and situations, classical planners may discover surprising solutions that elude even careful human analysis.

From a somewhat different perspective, a classical planner can also be viewed as a general problem solver. So it is generally applicable to many state-space search problems not normally regarded as planning problems. For example, Wouldwork can also solve constraint satisfaction problems (CSP) as a special case. The main advantage of using a planner for general problem solving is that the user is relieved of the task of developing a specialized state representation for a problem. A problem state is simply a list of propositions that are true in that state, and the planner reasons about states using predicate logic. Therefore, as long as the user can express potential problem-solving steps in predicate logic, the planner will independently search for a sequence of steps leading to a solution. However, this generality and convenience in specifying a problem comes with a cost. Unlike a specialized, hand-crafted problem solver, a planner cannot take advantage of sophisticated data representations. It can, however, incorporate problem domain specific features to improve efficiency.

The “classic” classical planning problem, called blocks-world, illustrates the basic operation of a planner. There is really no point in using a planner for such a simple problem, but it is suitable for introducing the basic concepts. Three blocks labeled A, B, and C are each resting on a table T. The goal is to stack the blocks so that A is on B is on C is on T. One possible action is ‘put x on y’, where x can be a block and y can be another block or the table. The shortest successful plan then contains only two steps: 1) put B on C, and 2) put A on B.

Background information about the problem is provided to the planner by the user in a problem specification file. In general, the problem specification will include a list of possible actions (eg, put x on y), a list of environmental objects (eg, A, B, and C are blocks, while T is a table), relevant properties and relations between objects (eg, x is on y), a state description for recording individual states between actions (typically a collection of facts holding at a particular time), a starting state (eg, A is on T, B is on T, and C is on T), and a goal condition (eg, C is on T, B is on C, and A is on B).

The Wouldwork Planner is designed to efficiently find any (or every) possible solution to a goal, within bounds provided by the user. Within those bounds the search is potentially exhaustive, if run to completion. This approach to planning makes it possible to find a needle in a haystack, if it exists, but is not feasible for extremely complex or large problems, which may require an inordinate amount of search time. However, since computer memory use grows only gradually, the main limitation for large problems is simply user patience.

</details>

<details>
<summary><strong>Planner Features (source extract)</strong></summary>

The Wouldwork Planner enables users to solve planning and constraint satisfaction problems without extensive programming experience. It is yet one more in a long line of classical planners. A brief listing of some other well-known classical planners would include Fast Downward, LPG, MIPS-XXL, SATPLAN, SGPLAN,
Metric-FF Planner, Optop, SHOP3, and PDDL4j. All of these planners are major developments by small research teams to investigate the performance of a wide variety of planning algorithms. But each has its own limitations in its ability to specify and deal with certain kinds of problems.

In contrast, the Wouldwork Planner was developed not to investigate different planning algorithms, but to extend the baseline capabilities for handling a wider variety of classical problems. (Please see the author’s medium.com article for a gentle introduction to planning problems at: https://medium.com/@davypough/traditional-ai-problem-solving-with-wouldwork-fcb0c4a71226 .) Wouldwork focuses on the problem templates and program interfaces that allow a user to flexibly and conveniently specify a problem of modest size, and perform an efficient search for a solution. The core planning algorithm itself performs a simple depth-first search through state-space, optimized for efficiently examining thousands, or potentially millions, of states per second, optionally in parallel. The program attempts to combine many of the interface capabilities of the other planners into one package. Some of the basic features of the user interface include:

General conformance with the expressive capabilities of the PDDL language for problem specification, with extensions
Arbitrary object type hierarchies
Mixing of object types to allow efficient selection of objects during search
Action rules with preconditions and effects, based on predicate logic
Full nested predicate logic expressiveness in action rules with quantifiers and equality
Specification of initial conditions
Goal specification
Fluents (ie, continuous and discrete variable quantities & qualities)
Durative actions taking time to complete
Exogenous events (ie, happenings occurring independently of the planning agent’s actions)
Temporal plan generation (ie, possible action schedules)
Global constraint specification, independent of action preconditions
User function specification for on-the-fly, possibly recursive, embedded computations
Specification of complementary relations to simplify action rules
Inclusion of arbitrary Common Lisp code in action rules, constraints, and functions
User control over search depth
Identification of shortest length, shortest time, or min/max value plans
Optional parallel processing to speed up search
Output/debugging diagnostics describing details of the search

</details>

<details>
<summary><strong>Disclaimer (source extract)</strong></summary>

The Wouldwork Planner is open-source software. It can be freely copied, distributed, or modified as needed. But there is no warrant or guarantee attached to its use. The user therefore assumes all risk in its use. Furthermore, the software was developed for experimental purposes, and has not undergone formal unit testing. Run-time error checking is piecemeal, and latent software bugs surely remain. The software runs optimally in the Steel Bank Common Lisp (SBCL) programming environment. It is designed to be portable for other common lisp implementations, but only tested with CCL. Search is compute-intensive, so expect significantly longer run times if not using SBCL. Users may send bug reports, suggestions, or other useful comments to Dave Brown at davypough@gmail.com.

</details>
