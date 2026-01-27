# Tutorial: Model a Problem in Wouldwork

This tutorial teaches the **core interface** by walking through the modeling pipeline. Use the appendix cookbook for complete end-to-end examples.

## Mental model

You provide a *problem specification* describing:

1. **Objects & types** (what exists)
2. **Relations** (facts that can be true/false)
3. **Actions** (how facts change)
4. **Initial state** (which facts are initially true)
5. **Goal** (facts you want to make true), plus optional constraints

A planner run is then: **search** over action sequences.

## Step-by-step checklist (printable)

- [ ] Can I name all objects? (or generate them systematically?)
- [ ] Can I express state as boolean facts/relations? (avoid hidden state)
- [ ] For each action: do I have complete preconditions and effects?
- [ ] Do I have a goal that is actually reachable?
- [ ] Do I need optional features (quantifiers, fluents, exogenous events)?

## Core sections (source extracts)

<details>
<summary><strong>Problem Specification</strong></summary>

Most classical planners take as input, a text-like specification of a planning problem provided by the user, and this planner is no exception. The standard problem specification language for classical planning is PDDL, which offers the user a straightforward way to define various types of environmental objects, properties and relations, as well as possible actions, constraints, goals, and initial conditions.

The format of a problem specification file for the Wouldwork Planner is a variation on PDDL to allow some further simplifications and elaborations, but at the expense of being able to represent some very complex planning scenarios falling outside the scope of this planner. The following paragraphs outline the essential sections comprising a problem specification file. The blocks-world problem mentioned previously will serve as a running example. Additional specification sections for more complex problems are left for Part 2 and the Appendix. Advanced planning applications may include arbitrary Common Lisp code along with the standard PDDL sections.

</details>

<details>
<summary><strong>Specifying Environmental Objects & Types</strong></summary>

The first requirement is to specify the various objects and object types
relevant to the problem domain. In the blocks world, there are three blocks (named A, B, C) and a table (named T). The following definition establishes this background information for the planner:

```lisp
(define-types
 block (A B C)
 table (T)
 support (either block table)) ;where a block can be placed

```
To the left are the object types, and to the right, the particular objects of that type appearing in the problem. The last type (namely, support) is a sometimes useful catch-all type signifying a generic entity of some kind, in this case either a block or table, both of which can support blocks. So A, B, C, and T are all supports. Generic types are often useful for simplifying action rules, to be discussed shortly. The ‘either’ construct simply forms the union of its argument types. In this example the object names correspond to actual objects in the blocks world, but in general objects can also be values like 1, 2, or 3 or any other Common Lisp programmatic object. Value objects provide a convenient way to represent some discrete properties, such as location coordinates or scaled discrete quantities, which then allows straightforward enumeration of the values in action rules.

</details>

<details>
<summary><strong>Specifying Object Relations</strong></summary>

The second requirement is to specify the relevant relations (includes properties) which may hold for and between the various object types. Primary relations are used by the planner to generate and analyze various states of the environment during planning. In the blocks world there is only one relation that needs to be considered, namely whether a block is, or is not, on some support, specified as: (on block support). In other words, in this problem it is possible for a block (A, B, or C) to be on some kind of support location (A, B, C, or T), and particular instantiations of this relation will be present in various states during planning. The full relational specification is then:

```lisp
(define-dynamic-relations
 (on block support))

```
The dynamic relation specification indicates that the situation of a block being on a support may change from state to state during the course of planning. (Static—ie, unchanging—relations are discussed later in Part 2. Optionally, all relations can be specified as dynamic, but it is computationally more efficient to separate the two.)

</details>

<details>
<summary><strong>Specifying Possible Actions</strong></summary>

The third specification is for the individual actions that the planning agent can take. In this case the agent can take a block and put it either on the table or on another block (in a stack). Actions are always composed at least of a precondition, specifying the conditions that must be met before the action can be taken, and an effect, specifying changes in the state of the environment after the action is taken. Since the action here is one of putting a block (labeled by a variable like ?block) on a support (like ?support), the precondition must specify that there is not another block on top of ?block, and in addition, that there is not another block on top of ?support, unless ?support is the table. It is conventional, and required in Wouldwork, that typed variables have a question mark (?) prefix. The following precondition expresses these conditions in predicate logic, where ?block signifies the block to be put somewhere, ?b signifies some arbitrary block, ?block-support is the support the block happens to be on currently, and ?support signifies the support on which ?block will be put:

```lisp
(and (not (exists (?b block) ;no block is on ?block
 (on ?b ?block)))
 (on ?block ?block-support) ;?block is on some support
 (not (exists (?b block) ;no block is on the support
 (on ?b ?support)))

```
In plain English, the first condition says that there does not exist a block which is on the block to be moved--ie, that it has a clear top. Later it will be explained how to define auxilliary functions, like cleartop?, which can be used to simplify such action conditions. The second condition verifies that ?block is on some kind of support, labeled ?block-support (either the table or another block); and the third condition checks that that the support place, labeled ?support, itself has a clear top.

It is worth noting that the above action can be expressed much more straightforwardly if fluents and functions are used:

```lisp
(and (cleartop? ?block)))
 (bind (on ?block $block-support))
 (cleartop? $support))

```
These kinds of enhancements to PDDL are discussed in Part 2, and illustrated in the specification for problem-blocks3a.lisp.

Once the action precondition is established, the action effect then must specify what happens if the action is taken by the planning agent. The effect (in this case after an instance of ?block is put on some ?support), is that the block will now be on that support; and also that the block will no longer be on what was previously supporting it. This effect is concisely expressed as:

```lisp
(assert (on ?block ?support)
 (not (on ?block ?block-support)))

```
In other words, ?block now will be on ?support, and ?block is no longer on ?block-support. The assert operator groups multiple changes to the current state together, and automatically arranges and executes them in the proper order to maintain database consistency. Like the precondition, the effect necessarily consists of only one (possibly complex) statement.

The assert operator establishes a scope for propositional updates to the current state. An assert statement, therefore, can include any number of sub-statements (except another assert statement). All of the asserted propositions within its scope will contribute to the next state.

Bringing the precondition and the effect together defines a complete action rule, consisting of six parts. First is the name of the rule (put), second is the duration of the action (where 1 means one unit of time), third is the list of free parameters appearing in the precondition (where a variable, or list of variables alternates with its type, as discussed later), fourth is the precondition, fifth is the free parameters related to the effect, and sixth is the effect itself:

```lisp
(define-action put
 1
 (?block block (?block-support ?support) support)
 (and (not (exists (?b block)
 (on ?b ?block)))
 (on ?block ?block-support)
 (not (exists (?b block)
 (on ?b ?support))))
 (?block ?block-support ?support)
 (assert (on ?block ?support)
 (not (on ?block ?block-support))))

```
On each planning cycle, each trial action is evaluated by the planner in the order presented in the problem specification. For each action during evaluation, all possible nonredundant arrangements of the precondition parameters are considered. For the above rule, the possible instantiations of ?block are A, B, and C, while the possible ?block-support and ?support values are A, B, C, and T. This leads to a set of 18 (?block ?block-support ?support) arrangement triples generated by the precondition parameters: ((A B C) (A B T) (A C B)
(A C T) (A T B) (A T C) (B A C) (B A T) (B C A) (B C T) (B T A) (B T C) (C A B) (C A T) (C B A) (C B T) (C T A) (C T B)). After removing duplicate variable assignments, each is tested against the action preconditions in the current state. If one or more instantiations satisfies the precondition, then the action’s effect is executed with those instantiations, producing an update to the current state. (See Part 2: Parameter Lists, for additional details.)

Each successful instantiation represents a possible next step in the planner’s path to the goal. As discussed in Part 2, the instantiation procedure is the same for local parameters appearing in quantified expressions (ie, expressions headed by exists or forall in the precondition or effect), except then the local parameter combinations are instantiated in the context of the current action parameter instantiations.

In general, logical expressions in an action can be headed by any of the usual logical labels—exists, forall, and, or, not, if, <relation name>, <function name>, <lisp function> (see Part 2 for a full list). An argument in a logical expression can generally be any <typed variable>, <fluent variable> (see Part 2), integer (allows iteration over values in the same way as with typed variables), real number (eg, the value of a fluent), or a <lisp lambda-expression> (which is first evaluated to produce the argument in the logical expression in which it occurs).

It is not unreasonable to complain that writing action rules is often difficult. We normally do not reason with predicate logic, and keeping all the variables straight requires some amount of bookkeeping (not to mention trial and error). However, predicate logic has been shown to be a highly expressive language for representing all manner of technical problems, one of which is planning. Its precision is often particularly useful in highlighting subtle errors in thinking. To better handle complex logical operations, the logical statements can often be usefully grouped into functions or independent actions. For example, another option for expressing the action put above is to break it into two actions, say put-block-on-table and put-block-on-block. This would simplify the logic for each rule, but at the potential cost of added debugging, maintenance, and planner processing.

When writing action rules, it may be helpful to remember that when an action runs, all of the parameters for the precondition first will be instantiated for all possible combinations of the variables. Then, the body of the precondition is run for each combination. Every time an instantiation meets the precondition, that instantiation becomes the context for instantiating the effect. The body of the effect then runs for each precondition instantiation, along with any further effect instantiations, thereby registering the specific effects of the action in the next state. In this way, executing a single action rule may give rise to multiple effects, each of which generates a successor state to the current state.

</details>

<details>
<summary><strong>Specifying Initial Conditions</strong></summary>

The next required specification characterizes the initial state of the environment, from which planning commences. The initial conditions are simply a list of all facts which are true at time = 0. Below are the initial conditions for the blocks world, indicating that all three blocks are on the table:

```lisp
(define-init
 (on A T)
 (on B T)
 (on C T))

```
Note also that the planner bases its analysis of all environmental states, including the initial state, on the usual closed-world assumption. This means every fact is represented as being either true or false, and never unknown. Therefore, if a fact is true, it will appear in a state representation at a particular time—for example, as (on B T) does in the initial state above. If this statement did not appear in the initial state, it would mean that B was not on the table—that is, that (on B T) was false. This way of representing facts (as either present or absent in a state) makes it easy to check for true and false conditions for a state in the action rules—for example, (not (on B T)) expresses the condition in an action rule that B is not on the table. But
(not (on B T)) does not appear in the state itself.

</details>

<details>
<summary><strong>Specifying the Goal Condition</strong></summary>

The last requirement is for a goal condition, which is also expressed in terms of a single predicate logic statement. When the planner encounters a state in which the goal is true, the planner records that state, and the path to it from the initial state, as a solution. Depending on whether the user has requested the shortest possible path (in number of steps or duration), or merely the first solution path encountered, the program either continues searching, or exits, respectively. In the blocks world, the goal is to appropriately stack three blocks, which is satisfied when the following fact is true:

```lisp
(define-goal
 (and (on C T)
 (on B C)
 (on A B)))

```

</details>

<details>
<summary><strong>Operational Considerations</strong></summary>

This completes the basic specification for the blocks world problem, except to note that the user can tell the planner how deeply to search for a solution. Since the shortest solution involves only two steps—namely, (put B C), and (put A B)—the user could set the *depth-cutoff* at 2 steps. Setting the *depth-cutoff* at 2 will end up generating the minimal number of intermediate states, but it is typically not known in advance how many steps will be required to solve complex problems. Choosing a large value will increase the search time due to greater search depth, but the planner can still find the shortest path to the goal among all paths shorter than or equal to that value. But choosing a value too small may lead to finding no solutions. Some experimentation with different depth cutoffs and partial solutions may be needed to solve complex problems. For the blocks-world problem it is not necessary to specify a depth cutoff, since Wouldwork automatically determines when to stop going deeper (ie, when all possibilities are exhausted).

Another viable option for problems where the total number of possible states is not too large (say under 1,000,000) is to leave the depth cutoff at its default value of 10, and stipulate a graph (rather than tree) search. A graph search avoids reanalyzing previously visited states, thereby potentially shortening the search, but incurs additional overhead that a tree search avoids. The depth cutoff can be gradually increased for subsequent runs. For the blocks problem the total number of possible states is relatively small (ie, all combinations of three blocks arranged in different ways), so setting the *depth-cutoff* to 0 (meaning go as deep as necessary) is fine, potentially examining all combinations in the worst case.

From the user’s perspective, interacting with Wouldwork can be thought of as first staging (ie, loading) a problem, and then solving it. When Wouldwork first loads, it automatically stages the blocks3 problem or whatever problem you were last working on. Once staged, a problem can be solved by entering (solve) at the REPL. Entering (stage problem-name) at any time will stage a new problem. Staging and solving will occur together if (run problem-name) is entered. Other commands can also be entered at the REPL—enter (help) to see a list. Note that if you edit and save a problem specification file, you will need to enter (refresh) to let Wouldwork know about the changes before solving again.

</details>
