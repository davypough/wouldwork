# Wouldwork

CLASSICAL PLANNING WITH THE WOULDWORK PLANNER

The Wouldwork Planner enables users to solve planning and constraint satisfaction problems without extensive programming experience.  It is yet one more in a long line of classical planners.  A brief listing of some other well-known classical planners would include Fast Forward, LPG, MIPS-XXL, SATPLAN, SGPLAN, Metric-FF Planner, Optop, SHOP3 and PDDL4j.  All of these planners are major developments by small research teams to investigate the performance of a wide variety of planning algorithms.  But each has its own limitations in its ability to specify and deal with certain kinds of problems.  In contrast, the Wouldwork Planner was developed by one individual, not to investigate different planning algorithms, but to extend the baseline capabilities for handling a wider variety of classical problems.  It focuses on the problem templates and program interfaces that allow a user to flexibly and conveniently specify a problem of modest size, and perform an efficient search for a solution.  The core planning algorithm itself performs a simple depth-first search through state-space, optimized for efficiently examining thousands or, in a few cases, millions of states per second.  The program attempts to combine many of the interface capabilities of the other planners into one package.  Some of the basic features of the user interface include:

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

For an example of a simple planning problem, see my medium.com article at:
https://medium.com/@davypough/traditional-ai-problem-solving-with-wouldwork-fcb0c4a71226 

For additional program details, please see the Wouldwork User Manual,
or email Dave Brown at davypough@gmail.com

# Ultraquickstart Setup:

## If you don't have SBCL already with Quicklisp

Then the easiest path is to install Roswell:

```
# macos
brew install roswell

# ubuntu / debian
sudo apt update && sudo apt install libcurl4-openssl-dev automake
curl -L https://github.com/roswell/roswell/releases/download/v19.08.10.101/roswell_19.08.10.101-1_amd64.deb --output roswell.deb
sudo dpkg -i roswell.deb

# other linux distributions, see: https://roswell.github.io/Installation.html

# windows => windows is unsupported by roswell
# but you can activate WSL2 (windows subsystem for Linux) and install ubuntu into wsl2
# and use ubuntu terminal of windows (see for instructions above)
```

Install SBCL via Roswell:

```
ros install sbcl

# in MacOS, if you get errors, try:
LIBRARY_PATH=$(brew --prefix)/lib CPATH=$(brew --prefix)/include ros install sbcl
```

To run SBCL, you do:
```
ros run
```

Since SBCL doesn't offer a nice user interface, I prefer for quick trying, to install `rlwrap` first:
```
brew install rlwrap

# in linux:
sudo apt install rlwrap
```

And then run:
```
rlwrap ros run
```

`rlwrap` makes your terminal program better approachable - you can use arrow keys as used by the Ubuntu Terminal
to move to the last commands and to move in the line. And `C-a` to go to the beginning of the line or
to the end of the line `C-e`.

You can then install quicklisp:
```
ros install quicklisp
```

and use it from within your SBCL.


You can install using Roswell also from GitHub wouldwork:
```
ros install davypough/wouldwork
```

After this, you can open SBCL and load wouldwork:
```
rlwrap ros run # starts SBCL
```
In SBCL, you can load wouldwork:
```
(ql:quickload :wouldwork)
```

And you can start exploring Wouldwork - the short nickname is `ww`.


## If you have already SBCL and Quicklisp installed

Then you can download/install Wouldwork via Quicklisp directly:

```
(ql:quickload :wouldwork)
```

You could also download/install Wouldwork using Ultralisp:

```
;; install ultralisp
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
;; then install wouldwork (using ultralisp)
(ql:quickload :wouldwork)
```

Follow then from step 6) on in the next (older) Quickstart:


# In the following a more detailed instruction is given

# Quickstart Setup:
1)	Install the SBCL Common Lisp release for your computer from http://www.sbcl.org/platform-table.html
2)	Install Quicklisp from https://www.quicklisp.org/beta/ 
3)	Clone or download the Wouldwork repository to your computer from https://github.com/davypough/quick-wouldwork placing it in your ~/quicklisp/local-projects/ directory (the path to the project files should then be ~/quicklisp/local-projects/quick-wouldwork/)
4)	Start SBCL from your terminal command prompt.
5)	At the SBCL prompt, enter (ql:quickload :wouldwork :force t)
6)	Enter (in-package :ww) to switch the current package from cl-user to wouldwork.
7)	Enter (test) to verify everything is loaded and running properly.
8)	Review the printout from any of the test problems to see the format of solutions.
9)	Look at the sample problem specifications (eg, problem-blocks3.lisp) in the src directory to become acquainted with how problems are defined.

# Additional Information

1. INSTALLING WOULDWORK TO A CUSTOM DIRECTORY

After cloning or downloading the quick-wouldwork repo to your chosen local directory,
make sure Quicklisp is installed. Then tell Quicklisp where your directory is with
`(push #p"/path/to/your/directory/quick-wouldwork/" ql:*local-project-directories*)`
and `(ql:register-local-projects)`. You should be able to load Wouldwork with
`(progn (ql:quickload :wouldwork :force t) (in-package :ww))`. 

2. ACCESS MORE HELP

After loading Wouldwork with `(ql:quickload "wouldwork" :force t)` and `(in-package :ww)`,
you can get more help with REPL commands by entering `(help)` at the REPL prompt.

3. PORTABILITY

For efficiency purposes, Wouldwork was originally designed to take advantage of some non-standard features in SBCL.
However, it has since been extended with generic libraries to also run on CCL, although not with parallel multi-threading.
As a result, expect significantly slower performance with CCL for large problems.

4. WORKFLOW

When you are working on a problem (composing, testing, debugging, etc),
Wouldwork remembers the last state when your common lisp session is closed.
This allows you to pick up from where you left off in a previous session
without having to remember the settings of all the search parameters.
To cancel the current parameters and start working on a new problem,
enter `(stage <new-problem-name>)` to load the new problem into Wouldwork.
Staging assumes that you have created the new `problem-<new-problem-name>.lisp` file
in the src directory, or that it already exists.
After correcting any compile errors or warnings during staging, and changing any parameters,
enter (solve) to direct Wouldwork to solve the currently staged problem.
If there are run-time errors, fix the `problem-<new-problem-name>.lisp` file,
or include diagnostic checkpoints such as `(ut::prt <S-expression>)` in the file,
and then do (refresh) to tell Wouldwork to recompile the changes.
You can also enter `(ww-set *debug* 5)` at the REPL to step through the search one expansion node at a time.

5. TROUBLESHOOTING

Wouldwork runs best in a terminal window, where you can allocate a lot of memory
if needed for long runs--eg, >sbcl --dynamic-space-size 10000. If you get into a
configuration that won't compile or load correctly, try (reset) to reinitialize everything.
If you modify the Wouldwork program itself, occasionally ASDF will not automatically
recompile everything that the modification depends on. If this occurs, simply exit SBCL
and reload. See the User Manual for additional troubleshooting tips.
