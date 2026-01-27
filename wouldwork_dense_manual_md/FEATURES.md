# Optional Features

These features extend the basic planner model. Don’t use them unless the core model is working; optional features can multiply the state space.

## Source extracts (Part 2)

<details>
<summary><strong>Object Types</strong></summary>

Every object constant (eg, A or block1) must have a user-specified type (eg, block), in the define-types specification. The type is listed first, followed by a list of object constants of that type. The object constants must all be Common Lisp objects, usually symbols. In general, objects can have more than one type, and types can have subtypes, as a convenience for specifying action rules. For example in the blocks world, A is both a block and a support since it can support other blocks; and support includes both table and block as subtypes. By default, block A is also an instance of the supertype called something, since every object is a something, and every type is a subtype of something. Thus, a generic statement like (something ?obj) is valid for determining if some object is known to Wouldwork.

Specifications of subtypes are distinguished from object constants by the keyword either. For example, support has subtypes (either block table). But block has objects (A B C) or, perhaps in a more perspicuous specification, (block1 block2 block3), where the type is included in the name of the object. To include an object type with no object instances, specify the instances as () or NIL. Instead of listing a large number of objects for a type, you can also use the operator compute, followed by Common Lisp code, to automatically compute the object list (see Problem 8: Triangle Peg Puzzle in the Appendix for an example).

It is also possible to specify dynamic types—ie, types whose instances can vary from state to state. For example in the Triangle Peg Puzzle, the number of pegs left on the board in any state after a jump is one less than in the prior state. If each peg on the board is being checked to see if it can jump any other peg, it makes sense to only check those pegs which are currently on the board in any state, instead of all pegs (including those which have already been removed from the board). To specify such a dynamic type, create a query function to compute the current board pegs (see the following section entitled Query, Update, & Lisp Functions for how to write a function). For the Triangle Peg Puzzle, the dynamic type could look like:

```lisp
(define-types
 peg (peg1 peg2 peg3 …) ;static type (all pegs)
 current-peg (get-current-pegs?) ;dynamic type
 …)

```
where the function get-current-pegs? retrieves the current pegs from the current state. The type current-peg can then be used in action rules to generate pegs in the same way as the type peg will generate all pegs. Alternately, the precondition of the action rule could simply use the dynamic type directly in the parameter list as in (?peg (get-current-pegs?)). The savings in processing time may be significant, since the set of current pegs will always be less than or equal to the set of all pegs.

</details>

<details>
<summary><strong>Object Relations</strong></summary>

A fundamental description of any object necessarily includes the properties it has and the relations it has to other objects. Accordingly, each relevant relation (or property) of every object must be included either in a define-dynamic-relations or a define-static-relations specification. To illustrate the dynamic/static difference, consider a dynamic relation like (on block support), which could be instantiated by a proposition like (on A T). During the course of planning, the planner maintains a local database of propositions that are true for each state. When the status of A subsequently changes from (on A T) to (on A B), the current database is updated for the next state. However, static propositions, like (block A) indicating that A is a block, never change, and do not need to be maintained in every state. Static propositions are more efficiently stored in a separate database, which the planner can take advantage of if the user defines dynamic and static relations separately. Note, in passing, that in the proposition (block A), the term block is being used as a predicate. However, block is also a type. Predicates and types are distinguished by their context of use.

The relations of or between objects are specified according to object type, and serve as a template for the propositions that instantiate them. Binary relations are probably the most common, specifying a relation between two object types. For instance, (on block support) expresses a binary relation between blocks and supports. A unary relation expressing a property like (red block), says that blocks can be red. A trinary relation like (separates gate area area) indicates that gates separate two areas, and so on for other higher order relations specifying relations among an arbitrary number of types. Relations can even have no arguments, such as (raining), simply indicating the proposition that it is raining. The type arguments to a relation can also include the either construct, as in (color (either block table) hue), indicating that both blocks and tables can have a color, assuming the type hue includes object constants like red, blue, green, etc. The current limit on relation arity is four arguments. Thus, a relation like (connected node node node node node) incorporates one argument too many. All relations are fixed arity.

When a relation includes duplicate types like (separates gate area area), it is interpreted by Wouldwork as a symmetric relation. This is a convenience, since the user then does not need to worry about how the symmetric types (ie, the 2nd and 3rd area arguments) are instantiated during planning. The user can simply include (separates ?gate ?area1 ?area2) in an action, constraint, function, etc; leaving out the reverse symmetric test for (separates ?gate ?area2 ?area1), since Wouldwork automatically includes both in any database. Whenever one proposition like (separates gate1 area1 area2) becomes true or false, the reciprocal proposition (separates gate1 area2 area1) also becomes true or false. In those special cases where the relation is not symmetric, the default symmetric assumption can be cancelled by marking the relation with a “>” directional suffix—eg, (separates> ?gate ?area1 ?area2). Now the statement will only match the ?area1 and ?area2 variables in the given order.

Relations may also include fluent types, which act as variable types, most commonly for numbers, but also for other defined user types. For example, a relation like (height block $real) might specify that blocks have a height that is a real number, possibly useful for evaluating the net height of a stack of blocks. Fluent variables are identified by their dollar-sign ($) prefix, much as logical variables like ?support are identified by their question mark (?) prefix. The type label (following the $) must be a valid user-defined or Common Lisp type.

Fluent variables, however, operate differently from their logical variable counterparts in actions. While logical variables generate object instantiations, one at a time, for testing in actions; fluent variables are instantiated by looking in the propositional database of the current state for a matching proposition. In the previous example, the relevant database is queried for a proposition matching the pattern (height block $height). If it finds a proposition like (height A 3.2), it instantiates the variable $height with the value 3.2. This instantiation is then available for further evaluation in the action as the value of the variable $height. Note that a relation with fluents is more usefully characterized as a function (being a special kind of relation). For this reason there should be only one proposition in the database that can match the fluent pattern. Otherwise, the fluent instantiation would be ambiguous among the possible choices.

The returned value of a fluent is not limited to numerical quantities. Any user-defined type (eg, $hue) can also be a fluent (assuming the type hue includes object constants like red, green, blue, etc., and objects can have only one color at a time). A statement like (color block $color) illustrates this way of using a fluent variable. Looking at the actual problem specifications in the Appendix may help clarify the different fluent options and capabilities.

Sometimes, usually for efficiency reasons, a user may specify a relation that includes a lisp container fluent—ie, a list, vector, array, or hash-table. For example, the set of used words in the file problem-crossword5-11.lisp is specified as (used-words $hash-table), which records the words in a set that have been tried so far in the search. In these cases, the usual choice to represent a set will be to use a hash-table rather than a list or vector, because the containers are always tested for equality with the predicate #’equalp. The other choices, in general, will not be #’equalp even if they have the same elements. Only use one of the other choices if the order of elements in containers with the same number of elements is always the same—eg, a lexicographic ordering.

</details>

<details>
<summary><strong>Bijective Relations</strong></summary>

In some domains, a relation may be functional in both directions—ie, the mapping from either argument uniquely determines the other. For example, consider a puzzle where each item occupies exactly one position, and each position holds exactly one item. The relation (at item position) exhibits this one-to-one correspondence: given an item, there is exactly one position it occupies; given a position, there is exactly one item there.

Normally, to support efficient lookup in both directions, the user would need to define two separate relations:

```lisp
(define-dynamic-relations
 (at-by-item item $position) ; lookup position given item
 (at-by-position position $item)) ; lookup item given position

```
This is awkward because the user must then maintain both relations consistently—asserting and retracting both whenever the state changes. Wouldwork provides the :bijective annotation to handle this automatically:

```lisp
(define-dynamic-relations
 (at $item $position :bijective))

```
When a relation is declared bijective:
Both arguments must be fluent (prefixed with $), and there must be exactly two arguments.
Assertions and retractions are automatically mirrored. When you (assert (at box1 pos3)), Wouldwork internally maintains indices for lookup in both directions.
Bind operations work in either direction. At runtime, exactly one of the two $variables should already be bound (non-nil). The other will receive the looked-up value:

```lisp
(and (bind (loc agent1 $area)) ;$area gets bound, say to AREA1
 (bind (at $item $area)) ;$area is bound → lookup $item
 ...)

```
Important restrictions for bijective relations:
The relation must be genuinely one-to-one in both directions. If multiple items can occupy the same position, or an item can be at multiple positions, do not use :bijective.
Do not use NIL as a valid type instance for arguments of bijective relations. Wouldwork uses null-checking at runtime to determine which variable is bound. If you need to represent "no value," omit the proposition from the database rather than storing a NIL value; the bind will return NIL (false) if no matching proposition exists.
Within a single action rule, if you need to rebind the same $variable to a different value via a second bijective bind, first reset it to NIL using (setq $var nil), or use a different variable name.

Example:

```lisp
(define-types
 box (box1 box2 box3)
 slot (slot1 slot2 slot3))

(define-dynamic-relations
 (in $box $slot :bijective)) ; each box in exactly one slot,
 ; each slot holds one box

(define-action swap
 1
 (?slot1 slot ?slot2 slot)
 (and (bind (in $box1 ?slot1)) ; find what's in slot1
 (bind (in $box2 ?slot2))) ; find what's in slot2
 (?slot1 ?slot2 $box1 $box2)
 (assert (in $box1 ?slot2) ; move box1 to slot2
 (in $box2 ?slot1))) ; move box2 to slot1

```

</details>

<details>
<summary><strong>Complementary Relations</strong></summary>

For a given relation R, the complement of R is the relation expressing the negation of R (ie, not R). Complementary relations come in pairs, such that if any propositional instance of one is true, the corresponding propositional instance other is false. For example, (on switch) and (off switch) are complements. That is, for any instantiation of switch, say switch1, whenever (on switch1) is true, then (off switch1) is false, and vice versa.

If the user stipulates which relations are complements by using the define-complementary-relations specification, the planner will automatically keep track of which complementary propositions are true and which are false. Then, if an action rule concludes that switch1 is on by asserting (on switch1), the planner will automatically remove (off switch1) from the current database. Otherwise, the user must also assert (not (off switch1)) to maintain database consistency.

The previous switch example illustrates the simplest kind of complementary relation, namely one in which the arguments of both relations are the same (ie, switch). For simple complements like on/off, the planner can always work out how to deal with the assertion of any associated proposition, since (on switch1) implies (not (off switch1)), (not (on switch1)) implies (off switch1), (off switch1) implies (not (on switch1)), and (not (off switch1)) implies (on switch1).

However, some complements share only some, or even no, arguments. In the expanded blocks world problem, there is a gripper arm that moves blocks around. In this world, it is useful to know when the gripper is holding a block (in which case it cannot pickup another block) and when the gripper is free. The corresponding relations (holding block) and (not (free)) are therefore complements, since (holding block) implies (not (free)), and (not (holding block) implies (free). But the reverse direction is problematic. While (free) does imply (not (holding ?)), where ? matches anything that the gripper is currently holding, if (not (free)) were asserted, it is indeterminate what the gripper would then be holding. For this reason, all complement specifications are limited to the forward direction only. If the reverse direction is warranted, it must be included separately.

Putting all of the above examples into a specification of complementary relations would look like:

```lisp
(define-complementary-relations
 (on switch) -> (not (off switch))
 (off switch) -> (not (on switch))
 (holding block) -> (not (free)))

```

</details>

<details>
<summary><strong>Logical Statements, Quantifiers, & Doall</strong></summary>

Actions, goals, constraints, functions, and other user-defined constructs are composed of logical statements that the planner uses to analyze the current active planning state. All statements must adhere to the previous type and relation definitions as specified. Many statements simply return a true or false value when executed, but others may return a user-defined value (like query function calls), or add and delete propositions from a state (like assert statements) . Simple examples of logical statements appear in Part 1. The complete list of admissible logical statement forms in Wouldwork is included in the later section: The Variety of Logical Statements.

Of particular note are the quantifier statements forsome or exists (existential quantification) and forall or forevery (universal quantification). An existentially quantified statement generates all possible values of the local variables in its parameter list, and executes the statements in its body until some instantiation returns true, at which time the entire statement immediately becomes true. If the body is not true for any instantiation, then the statement returns false. Likewise, a universally quantified statement generates all possible instantiations, but returns true only if every instantiation is true. It returns false immediately if any instantiation is false. In general, quantifier statements, can be nested to an arbitrary level. Additional details about a quantifier’s parameter list are discussed in the subsequent section on Parameter Lists.

Experience has shown that a third “generating” logical statement form is also sometimes useful, in that it guarantees all instantiations will be processed. This is the doall statement, which follows the form of the quantifier statements, for example:

```lisp
(doall (?g gate ?s switch)
 (if (and (controls ?s ?g)
 (off ?s))
 (assert (inactive ?g))
 (assert (active ?g))))

```
This statement tests to see if the switch control for a gate is on or off in the current state, and updates the state accordingly. It always returns true.

Note, however, that in this case, generating and testing all possible values of gate and switch can be expensive. If there are four gates, each of which is controlled by a switch, there are a total of 4x4 = 16 separate instantiations (<gate1,switch1>, <gate1,switch2>, …), each of which is tested in the if statement. Since each gate is controlled by only one switch, and a switch can only be on or off, it would be much more efficient to write a statement like:

```lisp
(doall (?g gate)
 (if (and (controls $s ?g)
 (off $s))
 (assert (inactive ?g))
 (assert (active ?g))))

```
which uses a fluent variable ($s) as discussed later. Here, the fluent variable is bound to whatever switch controls the current generated gate ?g. Now there is only one test for each gate, for a total of 4 instantiations.

</details>

<details>
<summary><strong>Durative Actions</strong></summary>

For many planning problems, in which the required solution only involves the sequence of planning actions, the time to complete each action is irrelevant. In these cases the second argument in an action specification (after the name) can be 0, indicating instantaneous execution. For other problems where the duration of actions is relevant, the second argument will be a positive real number, signifying the time taken to complete the action. The duration can be specified in any arbitrary time units the user chooses, as long as the units are consistent in all of the action rules (and in any other requirements which are part of the problem specification).

One simple durative action might involve the planning agent’s movement from one area to another, where the agent is named me below. Assuming movement only occurs between adjacent areas, and the time to move to any new adjacent area is the same, the move action could be expressed as:

```lisp
(define-action move
 2.5 ;moving takes 2.5 time units
 ((?area1 ?area2) area)
 (and (loc me ?area1)
 (adjacent ?area1 ?area2))
 (?area1 ?area2)
 (assert (not (loc me ?area1))
 (loc me ?area2)))

```
As an aside, note that this action involves two variables which are both areas. Since the Wouldwork Planner always interprets variables as names for unique individuals, two distinct variables of the same type will always be instantiated with different objects. This convention means it is not necessary to check for equality with a statement like (not (eql ?area1 ?area2)) in the precondition above. Although this convention does violate a basic tenet of predicate logic, it is consistent with the intuitive understanding that we usually give different names to different objects in a given context to avoid confusion. It is straightforward to override this behavior, however.

</details>

<details>
<summary><strong>Fluent Variables & Variable Binding</strong></summary>

A fluent variable, indicated by the prefix $ (as in $support), contrasts with a generated variable, indicated by the prefix ? (as in ?support). While generated variables are instantiated in logical statements by generating all possible instances of the variable (eg, all possible supports for the variable ?support), fluent variables are instantiated by looking up a matching proposition in the current database. For example, in a statement like (on ?block $support), a previously generated value for ?block is first consulted (eg, A), and then the current state database is consulted to determine if A is on something. If it is, that something becomes the value of the fluent $support.

Fluent variables, then, provide a very efficient way of evaluating the truth of a statement, since there is no need to generate and test all possible values of a variable—a simple lookup suffices. However, to avoid ambiguity there should be only one, or no, matching proposition in the database. This limits fluents to appearing in functional relations like on—a block can only be on one support at a time. When there is a choice between using a generated or fluent variable, the fluent representation is much more efficient.

The object variables in the blocks world problem (namely, ?block and ?support) are discrete variables, in that they can only take on discrete values, such as A, B, C, or T. However, for other problems it is useful to allow continuous variables as well, perhaps representing the height and weight of a block. As discussed previously, in addition to relations like (on ?block $support), relations like (weight ?block $w) or
(height ?block-support $h) could also be used in action preconditions, say to account for limitations in the agent’s ability to move certain blocks that weigh too much or are stacked too high. The continuous variables $w and $h, distinguished by their required $ prefixes, are technically known as numerical fluents, and in this case can take on positive real number values. Numerical fluents are instantiated by the same lookup procedure as for discrete fluents.

Sometimes there is a choice of whether to represent a variable as a fluent ($) or as a discrete generated (logical) variable. Generally speaking, fluents are most useful for representing data (eg, the height of a block), while logical variables represent symbolic values (eg, individual blocks and supports). But data can also be made discrete if there are a modest number of possible values, say <100. In this case the discrete values must be defined as a type—eg, (define-type height (1 2 3 … 100)) so the action rules can generate the values in parameter lists. Note that processing will slow down somewhat if equality between the values cannot be tested with #’eql (eg, if the values are lists or vectors). Often it is better to simply use a fluent for such values—eg, (define-dynamic-relation (locations $list))—where $list represents a list of coordinates like ((0 0) (3 1) …). Coordinates can then be dynamically added to or removed from the current locations list in the action rules.

As a more complete example, consider the specification of the classic “jugs” problem. Two jugs of different size are used to measure out a specific amount of water taken from a large reservoir. There are no markings on the jugs to indicate graded amounts. A jug can be filled or emptied completely at the reservoir, or emptied into the other jug to the point where the other jug is full. The goal is to get some reservoir water and pour it back and forth between jugs until a specific target amount of water remains. The first part of the problem specification might look like:

```lisp
(define-types
 jug (jug1 jug2))

(define-dynamic-relations
 (contents jug $integer))

(define-static-relations
 (capacity jug $integer))

```
Here, the relations contents (representing the current amount of water in a jug) and capacity (representing the maximum amount of water a jug can hold) take a discrete argument (one of the jugs) and a fluent argument (an integer, indicated by the required $ prefix). And both relations are clearly functional, since a jug has only one contents and capacity at any time. The action of filling a jug with water from the reservoir then takes the form of the following rule:

```lisp
(define-action fill
 1
 (?jug jug)
 (and (bind (contents ?jug $amt))
 (bind (capacity ?jug $cap))
 (< $amt $cap))
 (?jug $cap)
 (assert (contents ?jug $cap)))

```
The action uses the fluents $amt and $cap to represent the current amount in a jug and its capacity, respectively. The rule says that if a jug presently has some amount of water in it (possibly 0), and the current amount is less than its capacity (otherwise it is already full), then after filling, its current amount will be its capacity. Other actions such as emptying a jug completely, or pouring the water in one jug into the other jug until it is either empty or the other jug is full (leaving some remaining in the first jug) are left for the complete Appendix jug problem example. The bind operator basically tells the planner to look up and assign the current values to all fluent variables in a statement. Those values are then available for use in subsequent statements. If you need to lookup (ie, bind) the value of a variable in an action rule, then you must specify the relation with the appropriate fluent variable type. Also, there is no need to use fluent variables when specifying background knowledge with define-static-relations, since they don’t change during search.

Note that any statement which involves database lookup—eg, (bind (loc ?obj $x $y)) in a precondition, query function, if statement, etc—must not include in-line computations: eg, (bind (loc ?obj (1+ $x) $y)). In the former statement the location of an object is being looked up in the database and its retrieved location coordinates assigned to the variables $x and $y. The expression (1+ $x) is not a variable, and thus cannot receive an assignment. But a statement like (assert (loc ?obj (1+ $x) $y)) is fine, since the x coordinate is being computed before adding the proposition to the database.

Other local variables can be introduced in any Wouldwork function using the usual let form from Common Lisp. If they are special variables (accessible from subfunctions), then simply declare them special as discussed later. Note that local variables appearing in pure Common Lisp expressions are not required to have the prefix $, even though it is advisable. Such prefixed variables are definitely required in mixed Common Lisp & Wouldwork statements, and there is no penalty for always using $-prefixed variables. As a general rule, assign fluent variables using the $ prefix notation—for example, with a setq (setq $var 3) or bind (bind (blocks $blocks) or let (let ($counter 0) …) form.

</details>

<details>
<summary><strong>Global Variables</strong></summary>

On some occasions it may be useful to define one or more global variables at the beginning of a problem specification file. Global variables are often convenient, because they are visible from anywhere in the problem specification, and in all threads. Use the normal Lisp operator defparameter for single-threaded programs and for programs in which the value of the parameter does not change. Otherwise, to define a global variable which may change or to enable multi-threaded program runs, put in a declaration like the following:

```lisp
(sb-ext:defglobal var-name val-form doc-string)

```
The global var-name can then be safely updated by using the built in SBCL atomic parallel operations such as

```lisp
(sb-ext:atomic-incf var-name delta) for fixnums, or
(sb-ext:atomic-push object var-name), for lists
(sb-ext:atomic-pop var-name)

```
See the SBCL manual for further details.

</details>

<details>
<summary><strong>Dynamic (aka Special) Variables</strong></summary>

Along with global variables, Common Lisp has a built-in facility for handling dynamic variables. Typically, the variables appearing in a user’s problem specification will be local (ie, non-dynamic) to each action rule or function, and therefore have no meaning to Wouldwork outside of that rule or function. However, user variables declared as dynamic are not so restricted. Wouldwork can use dynamic variables to relay variable values between a parent function and any of its subfunctions. For example, if one function initializes a counter that several other functions subsequently increment depending on various conditions, the incremented values will be available to the initializing function for further processing. Here is a simple example:

```lisp
(define-query bag-item ($item)
 (let ()
 (declare (special $net-price))
 (incf $net-price
 (case $item
 (peas 1.80)
 (carrots 2.25)
 (mangos 3.00)))
 (do-some-other-stuff)))

(define-query purchase-price? ()
 (let (($net-price 0))
 (declare (special $net-price))
 (bag-item ‘peas)
 (bag-item ‘carrots)
 $net-price)) ;returns the total price

```
Declaring $net-price as dynamic (using the Common Lisp keyword special) allows its current value to be shared among the functions that use it. Any special variable must be so declared inside of a let or do statement.

</details>

<details>
<summary><strong>Parameter Lists</strong></summary>

Each action precondition and effect, as well as every quantified logical statement has a parameter list which tells the planner how to process the variables appearing in those statements. In the jugs example above, the fill action has a parameter list for the precondition, namely (?jug jug), and for the effect (?jug $cap). The presentation format in a precondition parameter list is always zero or more variables followed by their type. The presentation format in an effect parameter list is always a simple list of zero or more variables. An empty precondition parameter list means execute the body of the action only once, if the precondition is simply (always-true). The order of variable-type presentations in a precondition or quantified statement is arbitrary, but the order of variables in an effect determines the order of the variable instantiations printed out in the steps of a solution. The printout of a step like (fill jug1 5) means fill jug1 to capacity 5, because that is the user-specified order and meaning of variables in the effect parameter list above. Note that the variables appearing in an effect parameter list can come from either the precondition or the effect. Complex types expressed via the either construct are also allowed in precondition parameter lists—eg, ((?pet1 ?pet2) (either dog cat pig) ?owner owner).

As a general rule, every generated variable (?) must be parameterized before it is subsequently used in a logical statement. Thus, a precondition parameter list establishes a scope in which its variables can appear in an action precondition, effect, or quantified formula. Fluent variables ($) will never appear in a precondition or quantified formula parameter list, since their values are not generated. Their binding is always established through the use of the bind operator. However, fluent variables can appear in action effect parameter lists to control how solution steps are printed, as mentioned above.

In the case of overlapping types, as in a blocks world parameter list like (?block block ?support support), some supports are blocks (all supports except the table T) and all blocks are supports. For this situation, the default is to generate only relevant combinations of a block and a support—eg, (A B) or (A T), but not (T A) or (T T) or (A A)—for checking in an action rule. For example, in the default case, there is no need to include a statement like (different ?block ?support) in the body of a rule.

Whenever the precondition of an action is satisfied for a particular instantiation of its variables, those instantiations are then available for use in the action’s effect. In the jugs example, all of the precondition variable instantiations for ?jug, $amt, and $cap are automatically passed to the effect, and therefore do not need to appear in the effect parameter list, unless needed to appropriately print out solution steps.

Wouldwork has several available strategies for generating variables, depending on the requirements of the problem. The default strategy, previously summarized in Part 1: Specifying Possible Actions, effectively generates a list of all possible instantiations of each variable, but culls the list to include only unique instantiations. That is, no two variables will be instantiated with the same value. This comports with the common intuition that we give different names (in this case variable names) to different objects (as values). For example if the parameter list is (?block block (?block-support ?support) support), there are three generating variables—?block, ?block-support, and ?support—each of which can be instantiated with any of the instances from their respective types—block (A B C), block-support (A B C T), and support (A B C T). Therefore by default, instantiations for the three variables are generated from the full cross-product of (block X block-support X support)minus any duplication of values = ((A B C) (A B T) (A C B) (A C T) (A T B) (A T C) (B A C) (B A T) (B C A) (B C T) (B T A) (B T C) (C A B) (C A T) (C B A) (C B T) (C T A) (C T B)). The action rule or quantified formula is then checked for each such instantiation of the three (?) variables.

For some problems it may be possible to reduce the number of instantiations to be checked by considering only different combinations of the variable types. To generate combinations, insert combination as a keyword at the front of the parameter list like so: (combination ?block1 block ?block2 block). Now only different combinations of instances will be generated—namely, ((A B) (A C) (B C), greatly reducing the number of instantiations to be checked in a rule. Specifying combinations usually only makes sense if there are at least two variables of the same type.

Alternately, the full cross-product of all variables can always be specified with the keyword product at the front of the parameter list like so: (product ?digit1 digit ?digit2 digit). Now, all possible combinations of two digits, including when ?digit1 = ?digit2, will be generated and checked in the rule.

On other occasions, it is useful to simply generate dot-products from the variable types, such that one instance is selected progressively from each type. Progressive generation is often useful for associating items with attributes. Use the keyword dot-product to specify this generation strategy: (dot-product ?obj object ?pos position). Here, each object will be paired with a corresponding position—eg, ((OBJ1 (3 1)) (OBJ2 (1 1) (OBJ3 (2 3)). Note that to generate dot-products the number of objects must be the same as the number of positions, since they are paired up one-to-one.

Here is a summary of the four operators using the basic template for two items (standard (?block1 ?block2) block):

Finally, the user can specify that one or more variables are to be generated dynamically—ie, every time an action rule is invoked. For this case, specify a variable type as a query function call that returns the list of values for that variable. For example, instead of specifying a parameter list as (?field field ?word word), specify it as (?field (get-remaining-fields?) ?word (get-remaining-words?)). Then write define-query functions named get-remaining-fields? and get-remaining-words? which return the remaining fields and words for instantiation in the action rule. This may progressively reduce the number of instantiations for multiple invocations of the rule, but is ineffective if the rule is only executed once.

</details>

<details>
<summary><strong>Goals</strong></summary>

A planning goal is a logical statement that evaluates to true or false (ie, T or NIL) when applied to a state. Often it will simply consist of a conjunction of literals, all of which must be true to satisfy the goal. Alternately, it can be a complex logical statement or query function which expresses the conditions for recognizing when a situation is true. Every state explored by the planner is checked against the goal condition to see if it satisfies the goal. If it does, the planner stores that final state along with the path to that state as a solution. Depending on whether the user is looking for the first solution or additional solutions, planning will either exit or continue the search, respectively.

When solving an optimization problem that searches for a min-value or max-value state, a goal specification may be superfluous, since progressively better solutions are produced whenever a new state’s $objective-value is better than that of any older state. Leave out the goal specification to tell Wouldwork there is no goal in such a case. But if a goal is specified, min or max values will only be stored for goal states. Solutions and best states are recorded separately, and progressively. While solutions are recorded in *solutions*, best states are recorded in *best-states*.

</details>

<details>
<summary><strong>Global Constraint</strong></summary>

The user may optionally specify a global constraint (or multiple constraints AND-ed or OR-ed together). A global constraint places unconditional restrictions (serving as a kill switch) on planning actions. If a global constraint is ever violated in a state encountered during the planning process, it means that state cannot be on a path to a solution, and the planner must keep looking for some other path to the goal.

A constraint violation occurs when the constraint condition evaluates to NIL (false). In other words, define the constraint to evaluate to T (true), when the constraint is not violated. For example, to have an agent avoid any area of toxic gas, the user could include a constraint like:

```lisp
(define-constraint
 (not (exists (?gas gas ?area area)
 (and (loc me ?area)
 (atmosphere ?gas ?area)
 (toxic ?gas)))))

```
Constraints thus use the same format for expressions as goals. Since constraints are evaluated independently of context, any variables in the constraint must be bound (ie, no free variables). In general, it is more efficient to place constraints locally in the preconditions of individual action rules, since a global constraint is checked in every trial state generated by the planner. However, global constraints can avoid excessive redundancy, and are usually simpler to specify and debug than action preconditions.

</details>

<details>
<summary><strong>Query, Update, and Lisp Functions</strong></summary>

One very flexible kind of statement that can appear pretty much anywhere is a function call, used to query or update the current planning state. In the simplest cases, arbitrary built-in Common Lisp function calls may appear along with other Wouldwork logical statements. For example, an expression in a precondition like (< $height1 $height2) evaluates to T (true) if the previously bound value of $height1 is less than $height2. User-defined Common Lisp functions are specified with the usual defun expression. In standard programming style, function calls pass the current values of their arguments on to built-in Common Lisp or user-defined functions, which perform some computation before passing the result back to the calling statement. This result then is available for subsequent use in the action rule.

A Wouldwork query or update function provides a convenient way to encapsulate and hide the details of a complex analysis, thus making the problem specification more readable. And since functions are called from action rules or other functions at run time, they can incorporate recursive calls. As usual, function calls support an arbitrary number of arguments, the values of which are passed to the function in the same ordered sequence. Functions must be defined before they are used in a problem specification file.

Query function calls can be used to assign computed values to fluent arguments like $height1 and $height2. For example, the following blocks world query function takes a support as an argument, and computes the elevation of that support. Since blocks can be stacked, the elevation of a block is recursively computable from the height of that block plus the elevation of the block beneath it. If the expression (setf $elev (elevation? state $support)) appears in the precondition of an action, where $support is already instantiated, then the resulting value of $elev after evaluation will be the elevation of that support. The setf operator simply assigns the fluent variable to the value returned by the function elevation?. This value might then be used subsequently in an expression like (< $elev 10) to check whether the total elevation of that support is less than 10.

```lisp
(define-query elevation? ($support)
 (do (bind (height $support $h))
 (bind (on ?support $s))
 (if (not (support $s))
 $h
 (+ $h (elevation? $s)))))

```
The above function first gets the height ($h) of the support ($support). Next, if there is some other support, $s, which $support is on, then it returns $h plus the elevation of $s as the net elevation of $support. Otherwise, it just returns $h as the elevation of $support. The calculation of elevation thus recurses down a stack of blocks, adding in the height of each, until finally the table’s height is added. The bind statement is used to bind $s to the support that $support is on, if it is currently on a support, or to NIL (meaning the statement is false), if it is not on any support. The do operator simply collects the statements into a sequence. The do operator is required here, because the body of a Wouldwork function must consist of only one statement (in distinction from a Common Lisp function).

The syntax for a function definition must include a name for the function (eg, elevation?), an argument list of $ and ? variables which are passed into the function (eg, $support), and a body consisting of a single logical statement (possibly composed of multiple sub statements) that returns a value when the function is executed.

As alluded to, there are two basic kinds of user function specifications, depending on whether the function call appears in the precondition or the effect part of an action. The define-query specification (eg, elevation?) is for use in preconditions and conditional if statements, and returns a value that can be assigned to a fluent variable. The setf statement above illustrates this kind of use. However, a more common query use is simply for returning a boolean true or false value, which does not get explicitly assigned. A statement like

```lisp
(if (stable? ?support)
 (assert (on ?block ?support)))

```
uses the query function stable? to check if a ?support is stable before putting a block on it, where stable? is defined by the user with define-query.

Alternately, a define-update specification is for functions appearing in an effect, and can be used for analysis leading to changes to the current database. For example, an effect statement like

```lisp
(deactivate-receiver! ?receiver)

```
might consolidate a number of database conditions and updates associated with deactivation. (While query functions are optionally distinguished by the ? suffix, update functions are optionally distinguished by the ! suffix.) Update functions return user-specified values from their body expression just like query functions. The function body's final value becomes the return value, which can be captured using setq or mvsetq for single or multiple values, respectively. Wouldwork automatically collects the implicit changes, and returns them to the update function’s caller. It is up to the action rule to assert all changes. In the following update function, the changes appear in the then part of the if-then statements:

```lisp
(define-update deactivate-receiver! ($receiver)
 (if (active $receiver)
 (do (not (active $receiver)))
 (doall (?g gate)
 (if (controls $receiver ?g)
 (active ?g))))))

```
The do statement simply groups several statements together as the then part of the if-then statement. The propositions (not (active $receiver)) and (active ?g) are updates to the current state.

</details>

<details>
<summary><strong>Next & Finally</strong></summary>

All statements in the effect part of an action rule are executed in order in the context of the variable values established during precondition execution. The order of effect statements therefore normally does not matter, since the state changes resulting from those statements are effectively processed as a group. However, some occasions require a follow-up sequence of effect actions, for which the operators next and finally may be appropriate. A common use is for when the initial actions of a rule can automatically trigger additional follow-up actions. In the blocks world, an example might be triggering the collapse of a stack of blocks after a block has been placed on a stack exceeding a certain height. In this case, the last effect statement could look like (finally (assess-stack-stability! ?support)), whose execution is delayed until the preceding effect statements are completed.

Any number of follow-up actions are allowed, each of which will be executed in strict sequence. The convention for including multiple follow-up actions is to label the initial statements with next, reserving finally for the last statement, although next and finally are internally processed as synonyms. Also, next and finally can only take one argument, which must be an update function call, not a generic logic statement.

</details>

<details>
<summary><strong>Assert Statements</strong></summary>

Propositions are added to (or retracted from) the current state using the assert operator. For example, (assert (on ?block ?support)) will, depending on the current values of ?block and ?support, add a proposition like (on block1 block2). Alternately, a statement like (assert (not (on ?block ?support))) will, depending again on current values, delete a proposition like (on block1 block2) from the current state.

An assert statement establishes a scope for other statements that update the current state. And often there are a number of statements that get asserted by an assert statement in the order specified by the user. Each assert statement generates its own update to the current state, which is then stored by Wouldwork for future follow-on processing. This means that the effect of an action rule can contain several assert statements, each with its own scope, and each of which will generate a follow-on state. Thus multiple follow-on states can be conveniently generated by different effects as well as different preconditions in action rules. See problem-triangle-xyz-one.lisp for an example of multiple effects all condensed into one large action rule (as opposed to many smaller rules). If the problem is one of optimization using either min-value or max-value, make sure the variable $objective-value is calculated in each assert statement.

</details>

<details>
<summary><strong>Exogenous Events</strong></summary>

Exogenous events are events that happen in the planning environment independent of the planning agent’s actions. Typically, the agent must react to or otherwise take into account such happenings along the way to achieving the goal. The user specifies exogenous events before planning begins in a program schedule, which becomes part of the problem specification. As these events are prespecified by the user, they are technically known as timed initial literals. The planner uses the schedule to automatically update the state of the environment at the appropriate time. For example, below is a schedule for an automated sentry, named sentry1, which is continuously patrolling three adjacent areas in sequence.

```lisp
(define-happening sentry1
 :events
 ((1 (not (loc sentry1 area6))
 (loc sentry1 area7))
 (2 (not (loc sentry1 area7))
 (loc sentry1 area6))
 (3 (not (loc sentry1 area6))
 (loc sentry1 area5))
 (4 (not (loc sentry1 area5))
 (loc sentry1 area6)))
 :repeat t)

```
Starting in area6 at time = 0 (which is specified separately in the initial state), the planner updates the sentry’s location to area7 at time 1. Subsequently, the sentry’s location is updated at each time index, to area6 (at time = 2), area5 (at time = 3), back to area6 (at time = 4), area7 (at time = 5), area6 (at time = 6), etc. The keyword :repeat (where t means true) indicates that the sequence is repeated indefinitely. If the keyword :repeat is not included (or is NIL), the exogenous events end with the last event listed. The happenings at each indicated time are simply a list of all state changes occurring at that time.

In this automated sentry problem example (presented fully in the Appendix as the Sentry Problem) the agent must avoid contact with the sentry, but can pass through a patrolled area as long as the sentry is in a different area at the time. Exogenous events often place global constraints on the planning agent, which can be added to the problem specification:

```lisp
(define-constraint ;avoid kill situation
 (not (exists (?s sentry ?a area)
 (and (loc me ?a)
 (loc ?s ?a)
 (not (disabled? ?s)))))

```
This constraint determines whether there is a sentry and an area, such that the agent ( labeled me) and the sentry are both located in that area, and the sentry is not disabled (a kill situation). If the constraint is ever not satisfied during planning (ie, evaluates to NIL in any state), then the constraint is violated, and the planner must backtrack and explore a different path.

There is also a means to temporarily interrupt scheduled happenings, and to dynamically change the sequence of happening events based on certain conditions. The previous constraint shows that whenever a sentry is disabled (eg, by jamming, destruction, or other deactivation), then the constraint is satisfied, and it will be safe to enter the sentry’s current area. But becoming disabled means the sentry’s program schedule is temporarily or permanently suspended during planning. The user can optionally indicate when such an interruption occurs by specifying interrupt conditions, signaled by the keyword :interrupt. Adding this specification to the happenings then yields the full definition:

```lisp
(define-happening sentry1
 :events ((1 (not (loc sentry1 area6))
 (loc sentry1 area7))
 (2 (not (loc sentry1 area7))
 (loc sentry1 area6))
 (3 (not (loc sentry1 area6))
 (loc sentry1 area5))
 (4 (not (loc sentry1 area5))
 (loc sentry1 area6)))
 :repeat t
 :interrupt (exists (?j jammer)
 (jamming ?j sentry1)))

```
Note that if exogenous events are included in a problem specification, then a graph search strategy cannot be used. This is because states cannot be permanently closed. Entering a previously dead state, may no longer be dead, if it is reached at a different time. Use tree search instead.

</details>

<details>
<summary><strong>Exogenous Patrolling Objects</strong></summary>

Wouldwork has a specialization for a common kind of object, called a patroller, programmed to follow a set path in the environment. A patroller uses exogenous events to move from one area to the next, similar to an object defined with define-happening, but allows some optional specifications unique to a patrolling object. A patroller is specified with define-patroller (instead of the more general define-happening). For example, problem-mine1.lisp is a specification patterned after problem-sentry.lisp, but with implicit events, which includes a patrolling mine traversing a repeating path.

```lisp
(define-patroller mine1
 path (area5 area6 area7)
 mode :reverse
 rebound (exists (?c cargo)
 (and (bind (loc mine1 $area))
 (loc ?c $area)))
 interrupt (exists (?j jammer)
 (jamming ?j mine1))
 kill (and (not (exists (?j jammer)
 (jamming ?j mine1)))
 (bind (loc mine1 $area))
 (loc agent1 $area)))

```
Here, path is the sequence of areas that the mine moves through; the mode indicates that the direction should be reversed upon reaching area7 (or area1) with the other option being :cycle (ie, start over when reaching area7); rebound indicates the mine should rebound off of an obstacle in its path, reversing direction; interrupt specifies the condition in which the motion can be arrested; and kill indicates the condition in which this state will be pruned from the search tree.

</details>

<details>
<summary><strong>Waiting</strong></summary>

When there are exogenous events happening in the planning environment, it may sometimes become expedient for an agent to simply wait for the situation to change. For example, in the patrolling sentry problem, the planning agent can wait for a time interval before moving to a patrolled area, to allow the automated sentry to move away from the area. In Wouldwork, waiting is handled automatically by enabling the auto-wait mechanism in the problem specification.

```lisp
(ww-set *tree-or-graph* tree)

(ww-set *auto-wait* t)

```
When auto-wait is enabled, the planner automatically inserts wait actions when needed, without requiring an explicit wait action rule in the problem specification. The wait duration is computed as the exact time until the next relevant exogenous event occurs, rather than iterating one time unit at a time. Note, however, that enabling auto-wait typically increases the size of the search tree, so it us usually best to try to solve a problem without waiting first.

Auto-wait operates through two complementary mechanisms. First, when the agent reaches a state where no actions are applicable (the agent is "stuck"), the planner simulates happenings forward in time until either an action becomes applicable or the goal is reached. Second, when all regular actions from a state have been explored and lead to dead ends, the planner attempts waiting as a "second chance" before the search retreats to explore alternative paths. This enables discovering solutions that require deliberate pauses for synchronization with exogenous events.

Auto-wait requires tree search mode and runs only in serial processing with the depth-first algorithm. This is because states cannot be permanently closed. A state that was a dead end earlier might now have viable successors because an exogenous event occurred. The planner validates these requirements when enabling auto-wait and reports error messages if any are not satisfied. If your problem requires graph search or parallel processing, auto-wait cannot be used.

</details>

<details>
<summary><strong>Initialization Actions</strong></summary>

As already discussed in Part 1, the database for the starting state is initialized via a straightforward listing of true propositions appearing in a define-init specification. Similarly, an initialization action, as defined in a define-init-action specification, is an action which is taken only once, also at initialization. An initialization action is useful for adding numerous default propositions to the starting state, in lieu of listing each one individually in a define-init specification. For example, the following rule specifies that all sentries are active at initialization:

```lisp
(define-init-action activate-sentries
 0
 (?sentry sentry)
 (always-true)
 ()
 (assert (active ?sentry)))

```
The duration in an initialization action is always set to 0 (actually, any other value is ignored), but otherwise it looks and functions like a normal action. Also, the effect parameter list in an initialization action is usually set to (), since Wouldwork does not printout initialization actions as steps in the planning process.

On rare occasions it also useful to execute a normal action rule one or more times, and then delete it. The action then will no longer take part in generating new states. This allows a form of conditional pre-processing—eg, to generate several possible initial states from the *start-state*—which the other actions can subsequently process. For example, assume there is such an action rule, named add-first-peg, that places a peg on a peg board at each possible initial position. (See problem-triangle-backwards.lisp, which solves a triangle peg puzzle backwards.) But after this initial move, the rule is superfluous, since another action takes over placing pegs adjacent to those already on the board. Adding an action rule to delete the now superfluous action will speed up the search:

```lisp
(define-action delete-add-first-peg
 0
 ()
 (always-true)
 ()
 (delete-actions 'add-first-peg 'delete-add-first-peg))

```
This rule will only run once, since it deletes itself as well as the target add-first-peg action. The function named delete-actions is reserved and pre-defined in Wouldwork to be available for this purpose. Its arguments are the names of action rules to be deleted.

</details>

<details>
<summary><strong>Multiple Action Effects</strong></summary>

A typical action rule has one precondition and one effect. That is, if the precondition is satisfied for some instantiation of its variables, the effect assertions are executed with that instantiation, giving rise to the next state. But it may be convenient, and more efficient, on some occasions to specify several possible effects for each instantiation in one action rule. Then if a precondition is satisfied, all of the intended effects will be independently executed with the precondition instantiation. Each of the multiple effects will thus produce a follow-on state.

Multiple effects are useful for consolidating several action rules into one action, when those several actions have the same precondition. The consolidated action specification is more complex, since it now specifies the previous effects as alternates in the consolidated action. But then the separate actions are redundant, and can be removed. Processing efficiency is also enhanced, since the precondition only needs to be checked once to allow all possible alternate effects.

The simplest way to specify the alternates is to list them with a do statement in the consolidated effect like so:

```lisp
(do (if …
 (assert …))
 (if …
 (assert …))
 …)

```
where each set of assertions specifies a state update (if the condition is satisfied). In other words, all update statements within the scope of an assert are processed together, separately from other sets of assertions. But only use assert in action rules, not in update functions called from action rules. An example of a multi-effect action rule is provided in the problem specification file named problem-triangle-heuristic.lisp.

</details>

<details>
<summary><strong>The Variety of Logical Statements</strong></summary>

This section outlines the kinds of logical statement that may appear in actions, goals, initialization actions, functions, exogenous interrupt conditions, and constraints. All planning analysis is done in terms of logic statements, each of which is ultimately translated into the implementation language Common Lisp. Logic statements typically can be nested within other logic statements to an arbitrary extent, although there are some limitations.

```lisp
(loc ?sentry ?area)
```
This is probably the most common form of statement, containing locally generated (?) variables. It deals with the location of a sentry. This basic statement will become instantiated with particular values for ?sentry and ?area during execution (eg, sentry3 and area2) forming a proposition. In a precondition the statement tests whether or not a proposition is present in a state database (returning T or NIL). If true, processing of subsequent precondition statements continues (typically because all the precondition statements are AND-ed together into a conjunction), but if false, the entire conjunction fails. In an effect, such an instantiated statement is instead interpreted as a proposition in the current state database.

```lisp
(assert (loc ?sentry ?area)), or
(assert (not (loc ?sentry ?area))
 (loc me ?area))
```
Assertions are used in action effects to add or retract propositions from the current database. If ?sentry and ?area have been previously instantiated with sentry3 and area2, respectively, then the proposition (loc sentry2 area2) will be either added to or removed from the database. All unconditional statements within the scope of an assert will be asserted.

```lisp
(loc sentry1 ?area)
```
A partially instantiated statement. The operation of partially instantiated statements is as described above. If a statement is fully instantiated, as in (loc sentry1 area3), it is already a proposition, and can be checked in the database directly.

```lisp
(not (loc ?sentry ?area))
```
The negation of a statement in a precondition tests whether an instantiation is explicitly not in the database. In an action effect it means delete the proposition, if present. In the latter case there is no change if the proposition is not present.

```lisp
(loc ?sentry $area)
```
A statement containing fluents (eg, $area), which can be matched with the current variable bindings is in the database. If not found in the database, the statement is false, otherwise true. Note that fluents should only appear in functional relations (ie, relations exhibiting only one instantiation in a database at a time). Location is one such relation, since an object normally cannot be located in more than one place at a time.

```lisp
(bind (loc ?sentry $area1))
(not (bind (loc ?sentry $area1)))
```
The bind operator on a fluent statement binds its fluent variables by looking up their values in the current database. The bindings are then available for subsequent use. If a corresponding proposition is not found in the database, the proposition returns false, otherwise true. It is normally used in a precondition or effect if statement.

```lisp
(assert (if (active ?sentry)
 (dangerous ?sentry)
 (benign ?sentry)))
```
A conditional if statement performs a test and depending on the true/false result selects one or more statements to assert. Its full format consists of three clauses (if <test> <then> <else>), although the <else> clause is optional. In an effect the test is performed on the current successor state, which is being updated. The judicious use of if statements in effects can substantially reduce the total number of action rules required to cover a problem domain.

```lisp
(cleartop? ?block)
```
A statement that calls a query function is indicated by an optional postfix question mark (?) on a predicate or other computation. The sequence of argument variables must correspond to the order of those in the query function’s definition. The statement’s value is the value returned by the function.

```lisp
(ww-loop for ($x $y) in $positions do …)
```
An iterative statement like loop in Common Lisp. Allows binding of local variables and respects all the normal loop keywords and constructs like do, collect, sum, etc. Using ww-loop as opposed to loop allows Wouldwork logical statements to be embedded within the ww-loop statement. Note that, in general, Wouldwork logical statements cannot be embedded in Common Lisp statements.

```lisp
(setf $elevation (elevation? ?support))
```
In this case the query function elevation? takes one argument, ?support, computes the elevation of the support, and returns a value which is stored in the variable $elevation. The setf operator performs fluent variable assignment. Subsequent uses of the variable $elevation will refer to this value.

```lisp
(count #.(1+ *n*))
```
This is an example of a statement with a reader macro, indicated by #. followed by an expression to evaluate. It offers a flexible way to evaluate any Lisp expression immediately upon loading, the results of which are inserted directly into the statement. For example, if the value of *n* has previously been set to 100, then the above statement translates to (count 101). See the define-init section of the problem-knap19.lisp file for an example that initializes the capacity of a knapsack. Note that the variable *n* has to be evaluated earlier with a statement like #.(defparameter *n* 100), so that the value of *n* is available.

```lisp
(climbable> ?ladder ?area1 ?area2)
```
When a statement contains two or more generated variables of the same type (viz, ?area1 and ?area2 are both types of area), the planner assumes by default that the predicate and its arguments express a symmetric relation. A symmetric interpretation of this statement means that you can climb from ?area1 to ?area2 using the ?ladder, and from ?area2 to ?area1 using the same ?ladder. (Here, the ladder goes over a wall separating the two areas.) But this default interpretation is probably incorrect in this case. To specify that climbable is not symmetric, attach the direction marker (>) to the predicate, climbable>. Now the agent can use the ladder to go only from ?area1 to ?area2.

```lisp
(exists (?sentry sentry)
 (and (bind (loc ?sentry $area))
 (active ?sentry)))
```
An existential statement operates much like a scaled down action rule, but stops executing after it finds the first instantiation satisfying its conditional part (ie, the and statement above). The parameter list can have one or more generator variables, and the condition is tested for each instantiation set. If any instantiation of the condition is true, the entire existential statement is true, otherwise it is false.

```lisp
(forall ((?sentry1 ?sentry2))
 (if (and (bind (loc ?sentry1 $area1))
 (bind (loc ?sentry2 $area2))
 (eql $area1 $area2))
 (assert (conflict ?sentry1 ?sentry2))))
```
A universal statement is the quantified counterpart to an existential statement, and stops executing as soon as any instantiation does not satisfy the conditional part. It returns true only if all instantiations of its conditional part are true.

```lisp
(doall (?g gate ?s switch)
 (if (and (controls ?s ?g)
 (off ?s))
 (assert (inactive ?g))
 (assert (active ?g))))
```
A doall statement executes its body for all instantiations of its parameter list. It simply returns true when finished. Note that it is unusual to use forall in the effect of an action rule to make assertions, because the assertions will only be made if the forall returns true. Use doall instead.

```lisp
(do (activate-transmitter! ?transmitter)
 (activate-receiver! ?receiver))
```
The do operator simply combines two or more statements into a single statement. This statement is common in functions, where the body must be expressed as a single statement.

```lisp
(different ?block ?support)
```
The keyword different is a built-in predicate for testing whether two variable instantiations are distinct. Since blocks are also supports, the instantiations for each could be the same—eg, ?block = block1, ?support = block1. In cases like this, it may be useful to test for distinct instantiations using different. Here, different is synonymous with (not (eql ?block ?support)).

```lisp
(setq $objective-value (+ $current-value $additional-value))
```
A statement like this informs the planner what the net value of a state is when the solution-type is either min-value or max-value. Wouldwork will then search for a state minimizing or maximizing that value. Put this statement inside an assert statement, so it becomes part of the next updated state.

```lisp
(delete-actions 'add-first-peg 'delete-add-first-peg))
```
This statement will delete the specified action rules from further consideration, once the statement is executed. The search then continues without those rules.

```lisp
(always-true)
```
This statement, as it indicates, is always true. It can be used to satisfy a precondition for any and all action rule parameters, which are then passed to the effect for producing assertions.

```lisp
(print ?area), (< $height1 $height2), etc.
```
Any valid Common Lisp function or special form can also appear as the relation in a statement. However, the operator’s arguments are limited to other Common Lisp expressions, or previously defined planning parameters.

</details>

<details>
<summary><strong>Plan Monitoring & General Debugging</strong></summary>

Even the best laid plans can go awry for unexpected reasons. And it can be difficult to write a logically valid problem specification free from error. Accordingly, Wouldwork offers several options for tracking down mistakes in logic or programming, or just monitoring the planning process.

The global variable named *debug* (in the :ww package) controls the level of information output to the terminal during planning, and for nonparallel search can take a value of 0, 1, 2, 3, 4, or 5 (initially 0 for no debugging output). Level 1 simply outputs the complete search tree of actions attempted, which may be useful for determining where a plan goes wrong. Level 2 outputs level 1 plus the state after each planning step is taken, which may help determine why a particular action failed. Level 3 outputs information about the sequence of steps. Level 4 outputs detailed information about each step. Level 5 outputs the same as Level 4, but temporarily halts program execution after each step, allowing the user to carefully examine each possible action before continuing to the next step. For parallel search there are only two valid values of *debug*, namely 0 (no output) and 1 (basic thread output). Debugging, naturally, slows down the search process as it gathers information about the individual planning states.

Debugging output is displayed in the SBCL REPL window, along with normal output. Since the output can be lengthy, you can capture it to a file for subsequent analysis in a text editor by executing
(dribble <filename>)—eg, (dribble "D:/temp.txt")—before executing (run problem-name). After the program completes, execute (dribble) to close the file.

To assist with debugging individual actions, constraints, goals, functions, etc, you can also insert arbitrary Common Lisp code among logic expressions—for example to print variable bindings as they are assigned during execution. The following action from the blocks world problem will print the bindings for two variables of interest using the utility (ut::prt <variable names>) during rule execution:

```lisp
(define-action put
 1
 (?block block ?support support)
 (and (cleartop? ?block)
 (cleartop? ?support)
 (ut::prt ?block ?support))
 (?block block ?support support)
 (assert (on ?block ?support)
 (if (bind (on ?block $s))
 (not (on ?block $s)))))

```
Note that a ut::prt statement always returns the value of its last argument, just like print in Common Lisp.

Here is a list of some other potentially useful SBCL commands to inspect the Wouldwork data structures:

```lisp
 (ut::show (first *actions*)) ;show first rule code

 (ut::show *types*)

 (ut::show *relations*)

 (ut::show *db*) ;the initial dynamic database

```
As a final resort, there is also a facility for interrupting the search at a particular step (ie, action) in the program. This allows checking whether that step is being performed correctly. For example, specifying (ww-set *probe* (move (area4 area5) 12)), will interrupt the search just after the action of moving from area4 to area5, where the current depth is 12. This step (ie, state) can then be analyzed for errors. To tell Wouldwork to interrupt after the third such move action encountered, specify (ww-set *probe* (move (area4 area5) 12 3).

</details>

<details>
<summary><strong>Solution Validation</strong></summary>

The Wouldwork Planner includes a solution validation facility that allows users to verify and troubleshoot action sequences. This is particularly useful when a user has obtained or developed an action sequence and wishes to confirm that the sequence is valid according to the problem specification. It also serves as a debugging aid when developing or refining problem specifications, allowing step-by-step verification that a sequence of actions produce the expected state transitions.

The main interface is the validate-solution macro. Given a sequence of actions, the validator executes each action in order starting from *start-state*, checking that preconditions are satisfied and applying effects to produce successive states. The macro accepts action forms using the same syntax as solution output—the action name followed by its effect arguments:

```lisp
(validate-solution
 (move robot area1 area2)
 (pickup robot connector1)
 (connect robot transmitter1 receiver3))

```
If any action fails, the validator reports the action name, the failed action form, the reason for failure, and the state immediately prior to the failure point. For more detailed diagnostic output during validation, include the :verbose keyword before the action list.

</details>

<details>
<summary><strong>Debugging with an Action Trace</strong></summary>

Wouldwork provides an action tracing facility to help debug and understand action specifications during development. This tool intercepts an action execution during normal search and displays exactly what that action does for inspection.

Basic Usage:
(ww-set *debug* 0.5) ; Enable minimal debugging (required)
(trace-action action-name) ; Trace the specified action

The tracer runs a normal search on the current problem, and pauses each time it encounters a unique instantiation of the specified action with a satisfied precondition. For each instantiation, it displays:
The action name with current variable bindings
The complete current state (all propositions)
The resultant state delta showing additions and deletions

After examining the result of the action to make sure it is correct, you can continue on to the next instantiation, if useful.

</details>

<details>
<summary><strong>Debugging with Runtime Invariants</strong></summary>

An invariant in programming is a statement that, when executed, will check that some condition is true, and signal an error if it is not. Including invariants in a program can often detect subtle mistakes in programming that logical or otherwise careful analysis would overlook. For example, if the program involves initializing a lot of data, a typo that inputs 7 instead of 6 may allow the program to run fine to completion, but perform a faulty analysis.

In Wouldwork, there are two ways to use invariants. In the first (action invariants), an invariant can check for its condition in every state generated by a particular action rule during planning. An action invariant is expressed in Wouldwork as an update function (included as a define-update specification in the problem-*.lisp file), and called in a next or finally clause in an action rule. As an example, consider the check-emptys invariant function in the problem-tiles7a-heuristic.lisp file:

```lisp
(define-update check-emptys ()
 (do (bind (emptys $emptys)) ;get the value of $emptys
 (unless (alexandria:setp $emptys :test #'equal)
 (troubleshoot "$emptys is not setp: ~A" $emptys))
 (unless (alexandria:sequence-of-length-p $emptys 9)
 (troubleshoot "$emptys length is not 9: ~A"
 (length $emptys)))))

```
Here, the variable $emptys is always expected to be a list with no duplicates (checked with alexandria:setp) and to have a length of 9 (checked with alexandria:sequence-of-length-p). If either invariant is ever violated, a troubleshooting environment is entered, to help the user determine what led to the error in that action rule. Note that the arguments to troubleshoot will be displayed to explain which error was encountered.

Subsequent to entering the troubleshooting environment, if the user selects option 0, Wouldwork displays the state information for the problematic action, along with the followon states. Seeing the exact error should provide a clue to what led to the specific error. Once all such errors have been eliminated, the next or finally clauses in the action can be removed to allow more efficient processing.

The second use of invariants (global invariants) is for checking an invariant against every stage generated by Wouldwork. The purpose of including a global invariant is to make sure no inconsistent states are ever generated. If Wouldwork ever detects a global invariant violation, the troubleshooting environment is entered, providing information about where the inconsistency was detected. A global invariant is expressed in Wouldwork with a define-invariant specification in the problem-*.lisp file. For example,

```lisp
(define-invariant holds-cargo-location ()
 ;Cargo cannot be both held and have a location simultaneously
 (not (and (bind (holds me1 $cargo))
 (bind (loc $cargo $area)))))

```
In this invariant Wouldwork looks up what the agent is currently holding, if any, and checks to see if it is also located in some area. If both are true, it is a domain inconsistency indicating the invariant does not hold for the current state. A troubleshooting option is then invoked on that state. Again, once all such errors have been eliminated, the invariant can be removed or commented out to allow more efficient processing.

</details>

<details>
<summary><strong>Some Potentially Useful User Commands</strong></summary>

```lisp
sbcl --dynamic-space-size 24000
```
Opens SBCL with more than the default 1K of memory.

```lisp
(run-test-problems) or (test)
```
Runs through a varied collection of problems and their solutions, useful for debugging any changes to Wouldwork itself.

```lisp
(list-all-problems)
```
Displays the names of all problem specifications in the src directory.

```lisp
(ut::show (first *actions*))
```
Prints the lisp code for one of the Wouldwork action rules (for debugging).

```lisp
(pprint (get '*goal* 'fn))
```
Prints the lisp code for a Wouldwork function (for debugging).

```lisp
(ut::show *types*)
```
Prints all current type structures as defined in problem.lisp.

```lisp
(ut::show *relations*)
```
Prints all current dynamic relation structures as defined in problem.lisp

```lisp
(ut::show *static-relations*)
```
Prints all current static relations.

```lisp
(ut::show *db*)
```
Prints the initial dynamic propositional database before solving begins.

```lisp
(ut::show *static-db*)
```
Prints the initial static propositional database.

```lisp
(ut::prt s-expr s-expr …)
```
Prints the values of any lisp variables or s-expressions.

```lisp
(profile)
```
Profiles the solution of a problem, to determine which functions are taking the most time. If the search is interrupted midway (eg, by entering CTRL-C) it will be necessary to execute (sb-profile::report) to retrieve the results.

```lisp
(reset)
```
Removes all previous problem solving history. Potentially useful if Wouldwork should enter an inconsistent state that keeps generating errors.

</details>

<details>
<summary><strong>Some Useful User Functions to Use in Rules</strong></summary>

There are also a few pre-defined Common Lisp functions that the user can include in rules and functions. These functions are simply provided as a convenience so the user doesn’t have to write them, if needed. For example, if you want to use a hash table as a representation for a set of elements, there are functional set operations provided such as union, difference, copy, etc. Others may be added later.

```lisp
(different sym1 sym2)
 "Determines whether two symbols represent different objects
 rather than the same object."

(delete-actions &rest names)
 "Deletes named actions from *actions* at run-time."

(get-state-codes)
 "User calls this after finding backwards *solutions* to a
 Problem in order to generate forward paths to those
 solutions."

(backward-path-exists state)
 "Use in a forward search goal to check for the existence of a
 pre-computed backward path."

(vectorize lists)
 "Turns a list of lists into vector of vectors."

(make-ht-set &rest args
 &key (initial-contents nil initial-contents-p)
 &allow-other-keys)
 "Makes a wouldwork hash-table that works as a set container
 for the user."

(copy-ht-set set-ht)
 "Copy a set hash table (with t values)."

(union-ht-set &rest set-hts)
 "Unions hash tables keys. Assumes values are all t and have
 the same :test function."

(set-difference-ht-set ht1 ht2)
 "Returns a new hash table that represents the set difference
 of ht1 and ht2."
```

</details>

## Extracted tables (for completeness)

### Table 1

| Header | Operation | Result for block = (B1 B2) |
| --- | --- | --- |
| dot-product | Positional zip | ((B1 B1) (B2 B2)) |
| product | Full cross-product | ((B1 B1) (B1 B2)
 (B2 B1) (B2 B2)) |
| standard | Cross-product, distinct only | ((B1 B2) (B2 B1)) |
| combination | Cross-product, distinct, deduplicated by set-equality | ((B1 B2)) or
 (B2 B1)) — one representative |

### Table 2

|  |  |  |  |
| --- | --- | --- | --- |
|  |  |  |  |
|  |  |  |  |
|  |  |  |  |
