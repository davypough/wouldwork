# Wouldwork
CLASSICAL PLANNING WITH THE WOULDWORK PLANNER

The Wouldwork Planner is yet one more in a long line of classical planners.  A brief listing of some other well-known classical planners would include Fast Forward, LPG, MIPS-XXL, SATPLAN, SGPLAN, Metric-FF Planner, Optop, SHOP3 and PDDL4j.  All of these planners are major developments by small research teams to investigate the performance of a wide variety of planning algorithms.  But each has its own limitations in its ability to specify and deal with certain kinds of problems.  In contrast, the Wouldwork Planner was developed by one individual, not to investigate different planning algorithms, but to extend the baseline capabilities for handling a wider variety of classical problems.  It focuses on the data structures and programming interface that allow a user to flexibly and conveniently specify a problem of modest size, and perform an efficient search for a solution.  The core planning algorithm itself performs a simple depth-first search through state-space, optimized for efficiently examining thousands or millions of states.  The program attempts to combine many of the interface capabilities of the other planners into one package.  Some of the basic features of the user interface include:

-General conformance with the expressive capabilities of the PDDL language, plus extensions, for problem specification

-Arbitrary object type hierarchies

-Mixing of object types to allow efficient selection of objects during search

-Action rules with preconditions and effects, based on predicate logic

-Full nested predicate logic expressiveness in action rules with quantifiers and equality

-Specification of initial conditions

-Goal specification

-Fluents (ie, continuous variables, in addition to discrete variables)

-Durative actions taking time to complete

-Exogenous events (ie, happenings occurring independently of the planning agent’s actions)

-Temporal plan generation (ie, action schedules)

-Global constraint specification, independent of action preconditions

-Derived relations for simplifying action preconditions

-Function specification for on-the-fly, possibly recursive, computations

-Inclusion of arbitrary Lisp code in action rules, derived relations, constraints, and functions

-User control over search depth

-Generation of shortest plan found, plus other possible plans

-Optional parallel processing to speed up search

-Output diagnostics describing details of the search

For additional information, please see the Wouldwork User Manual,
or email Dave Brown at davypough@gmail.com


# Quickstart Setup:
1)	Install the SBCL Common Lisp release for your computer from http://www.sbcl.org/platform-table.html
2)	Install Quicklisp from https://www.quicklisp.org/beta/ 
3)	Clone or download the Wouldwork repository to your computer from https://github.com/davypough/quick-wouldwork placing it in your ~/quicklisp/local-projects/ directory (the path to the project files should then be ~/quicklisp/local-projects/quick-wouldwork/)
4)	Start SBCL from your terminal command prompt.
5)	At the SBCL prompt, enter (ql:quickload :wouldwork)
6)	Enter (in-package :ww) to switch the current package from cl-user to wouldwork.
7)	Enter (run-test) to verify everything is loaded and running properly.
8)	Review the printout from any of the test problems to see the format of solutions.
9)	Look at the sample problem specifications (eg, problem-blocks3.lisp) in the src directory to become acquainted with how problems are defined.

# Additional Information

1. INSTALLING WOULDWORK TO A CUSTOM DIRECTORY

After cloning or downloading the quick-wouldwork repo to your chosen local directory,
make sure Quicklisp is installed. Then tell Quicklisp where your directory is with
`(push #p"/path/to/your/directory/quick-wouldwork/" ql:*local-project-directories*)`
and `(ql:register-local-projects)`. You should be able to load Wouldwork with
`(progn (ql:quickload :wouldwork) (in-package :ww))`. 

2. ACCESS MORE HELP

After loading Wouldwork with `(ql:quickload "wouldwork")` and `(in-package :ww)`,
you can get more technical help by entering `(help)` at the REPL prompt.

3. ROSWELL

If you are using roswell, then your `local-projects` folder is usually in 
`~/.roswell/local-projects/`. Also, if you want to use roswell SBCL,
then you have to start with `ros run`

4. PORTABILITY

For efficiency purposes, Wouldwork was originally designed to take advantage of some non-standard features in SBCL.
However, it has since been extended with generic libraries to also run on CCL, although not with parallel multi-threading.
As a result, expect somewhat slower performance with CCL for large problems.
