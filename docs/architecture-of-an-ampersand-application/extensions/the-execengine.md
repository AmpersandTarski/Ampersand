# The ExecEngine

This chapter is meant for Ampersand users that want to build prototypes that automatically fix violations for specific rules. The idea behind this is quite simple: all it takes is another way of specifying violation texts. In practice, however, it takes some effort to learn how to do this correctly. Therefore, we only encourage you to do this if you are sufficiently motivated to spend this effort, i.e. if you are convinced it is worth your while.

Before studying this chapter, make sure you know how to predict violations. You need to understand how Ampersand computes violations, given a certain population.

## Automated rules

Did you ever wonder how you can make your computer prevent rules from being violated? For that purpose, you must specify what your prototype will do the moment it signals a violation. This chapter tells you how.

In essence, an Ampersand prototype is a database application that helps its users to keep rules satisfied. Keeping a rule satisfied happens in one of the following ways: 1. Your prototype imposes a rule. Violations are not tolerated. The prototype does not accept any change of data that violates the rule. As a result, the rule remains satisfied all the time. Such rules are called _invariants_. 2. Your prototype signals each violation to a designated role. The signal does not go away until the user has eliminated the violation. The rule is called _process rule_ because it prompts users to do some work. Notice that a violation of a process rule may persist. That is because it is meant to be resolved by persons rather than a computer. 3. Your prototype restores a violation the moment it occurs. It does so by means of a built-in robot, which we call the ExecEngine. Such rules are called _automated_. 4. The rule cannot be violated because of the way Ampersand is built. These rules are called _laws_. No effort is needed to maintain them, because they are always true.

This chapter is about the third category: automated rules. The idea is to prevent violations by acting in time, to satisfy the violated rule. This is nice for your users, who have no concern with those violations.

This chapter introduces automated rules by example. We will first create a rule, which a user must keep satisfied. We will then automate that process by adding instructions for the ExecEngine.

### Learn by experimenting

Most of the examples are taken from the demo script [Project Administration Example](https://github.com/AmpersandTarski/ampersand-models/tree/master/Examples/ProjectAdministration). You can compile and run this script, and reproduce several of the examples that follow.

### Example \(`InsPair` and `DelPair`\)

Consider the following example:

```text
RELATION pl[Project*Person]      MEANING "A project can have project leaders."
RELATION member[Project*Person]  MEANING "A person can do actual work within a project."
RELATION coworker[Person*Person] MEANING "Two people are co-workers in a project."
```

The following rule defines coworkers. Two different persons are coworker if they work in the same project. As a person can be either a project leader or a member, we get this rule:

```text
RULE coworker = (pl\/member)~;(pl\/member)-I
```

This rule basically says that `coworker` is shorthand for the much more complicated term `(pl\/member)~;(pl\/member)-I`. Quite useful indeed. Now suppose this rule is satisfied in the system. Then some manager assigns a new person, Harry, to the project Zeus-III. To administer that fact in the system, he adds a pair `("Zeus-III", "Harry")` to the relation `member`. Now there is a problem. The prototype will not accept this input, because our rule is violated. For all present workers in the project now have Harry as a new coworker. That should be administered in the relation `coworker` in order to satisfy the rule.

One way to do that is to allow the manager to edit the relation coworker. This is not very convenient for that manager. He will be irritated, as he is forced to enter a number of pairs into the relation `coworker` that is equal to the number of persons in the project plus the number of projectleaders of that project. This rule is typically a candidate for automation.

We have to consider that whenever a person is added to the project, that person must be added to `coworker` as well. But when a person is discharged from the project, that person must be removed from `coworker`. We can split the rule in two, knowing that `r=s` is always equivalent to both `r|-s` and `s|-r`.

```text
ROLE "ExecEngine" MAINTAINS r1
RULE r1:  (pl\/member)~;(pl\/member)-I |- coworker
VIOLATION (TXT "InsPair;coworker;Person;", SRC I, TXT ";Person;", TGT I)

ROLE "ExecEngine" MAINTAINS r2
RULE r2:  coworker |- (pl\/member)~;(pl\/member)-I
VIOLATION (TXT "DelPair;coworker;Person;", SRC I, TXT ";Person;", TGT I)
```

Let us discuss both rules, starting with the first one. The `ROLE` statement assigns rule `r1` to the ExecEngine. The instruction for the ExecEngine is given in the `VIOLATION` string. It will be executed for each violation of rule `r1`.

Elaborating on this example, just which violations will the ExecEngine resolve? Suppose the project has Alfred and Bob on the team before Harry is assigned. This means that the relation `coworker` contains `("Alfred", "Bob")` and `("Bob", "Alfred")` for starters. When the pair `("Zeus-III", "Harry")` is added to the relation `member`, we get the following violations: `("Alfred", "Harry")`, `("Harry", "Alfred")`, `("Bob", "Harry")`, and `("Harry", "Bob")`. So, the following instructions will be given to the ExecEngine:

```text
"InsPair;coworker;Person;Alfred;Person;Harry"
"InsPair;coworker;Person;Harry;Person;Alfred"
"InsPair;coworker;Person;Bob;Person;Harry"
"InsPair;coworker;Person;Harry;Person;Bob"
```

Note that the violations of rule `r1` are precisely the pairs the ExecEngine must add to `coworker` to satisfy rule `r1`. The function `InsPair` is a predefined ExecEngine function, that adds to the population of a relation. The corresponding function `DelPair` removes pairs from the population of a relation. In the example, it is used to remove people from `coworker` that no longer share a project.

**Notes**:

* The examples use `SRC I` or `TGT I` to produce atoms that are to be inserted or deleted. However, `I` may be any term whose source concept is the same as that of the preceeding `SRC` or `TGT`. 
* The `SRC <term>` and `TGT <term>` is a set of pairs \(a,b\), where a is the source atom or target atom of the violation and b is a set of atoms that is the result of `<term>`. In the examples given, this set of atoms has cardinality 1 \(which is most often the case\). However, if it is empty, that is considered regular behaviour, and this will hence not result in an error. Also, if it has a cardinality &gt; 1, then `InsPair` will insert them all whereas `DelPair` will produce an error. 

## Example \(`InsAtom`\) and \(`{EX}`\)

Consider the following example:

```text
RELATION pl[Project*Person]                     MEANING "A project can have project leaders."
RELATION project[Assignment*Project] [UNI,TOT]  MEANING "Every Assignment must apply to one project"
RELATION assignee[Assignment*Person] [UNI,TOT]  MEANING "Every Assignment must apply to one person"
```

The following rule states that for every project leader, an assignment must exist that applies to one person and one project, basically assigning that person to be a project leader for the Project.

```text
RULE "Require Assignment" : pl |- project~;assignee
```

This calls for two different things: first, the automated creation of an atom in the concept `Assignment`, and second the consecutive population of relations `project` and `assignee` using this newly created atom.

This is specified as follows:

```text
ROLE "ExecEngine" MAINTAINS "Create Assignment"
RULE "Create Assignment" : pl |- project~;assignee
VIOLATION (TXT "{EX} InsAtom;Assignment"
          ,TXT "{EX} InsPair;project;Assignment;_NEW;Project;", SRC I
          ,TXT "{EX} InsPair;assignee;Assignment;_NEW;Person;", TGT I
          )
```

First, note that we have three consecutive statements: an `InsAtom` command followed by two `InsPair`s. Using the phrase `{EX}` in front of each statement allows the interpreter of the violation texts to recognize each individual command and its arguments. In order to ensure that you do not forget about this, you may want to consider habituating yourself to _always_ use {EX} before any function.

The first statement assigns the rule `Create Assignment` to the ExecEngine. The prototype will send all violations of this rule to the ExecEngine. The rule says that for every project with a project leader, there must be an assignment. Without that assignment, the rule is violated. The `VIOLATION` statement specifies that a new `Assignment` must be made for each violation. For that purpose, we use the predefined function `InsAtom`. This function takes a single argument, being the concept within which an atom has to be generated \(`Assignment` in the example\).

The second statement calls the `InsPair` function in order to populate the relation `project`, in the manner we described above. Note that at the position where we want to specify the newly created Assignment atom, we use the phrase `_NEW`. The third statement calls the `InsPair` function in a similar fashion, and thus populates the relation `assignee`.

Note that

* in an `InsPair` \(or `DelPair`\), the source-atom or the target-atom \(or both\) can be the keyword `_NEW`.
* the keyword `_NEW` refers to the last atom that was created by the \(last\) `InsAtom` statement that was executed in the violation.
* when using `_NEW`, the corresponding concept \(obviously\) MUST be the same as the concept as specified in the `InsAtom` statement.

Here is how it works. Suppose the pair `("Zeus-III", "Rhea")` is added to the relation `pl`, meaning that `Rhea` is being made a project leader of project `Zeus-III`. This produces a violation `("Zeus-III", "Rhea")` of the rule `Create Assignment`. The associated VIOLATION statement produces the text

```text
 {EX} InsAtom;Assignment{EX} InsPair;project;Assignment;_New;Project;Zeus-III{EX} InsPair;assignee;Assignment;_NEW;Person;Rhea
```

which is passed to the ExecEngine, which splits the text in three statements

```text
 InsAtom;Assignment
 InsPair;project;Assignment;_New;Project;Zeus-III
 InsPair;assignee;Assignment;_NEW;Person;Rhea
```

and subsequently executes them. Executing the `InsAtom` statement creates a new atom in concept `Assignment` \(let's say it is `Assignment_3495812395`. The keywords `_NEW` in the InsPair statements are then replaced by `Assignment_3495812395`, so that `("Assignment_3495812395", "Zeus-III")` is inserted into relation `project[Assignment*Project]`, and `("Assignment_3495812395", "Rhea")` is inserted into relation `assignee[Assignment*Person]`.

## Example \(`DelAtom`\)

In our example, whenever a project participant is discharged from his task, the corresponding Assignment needs to be deleted. We can do this by means of an automated rule:

```text
ROLE "ExecEngine" MAINTAINS "Delete Assignment"
RULE "Delete Assignment" :  project~;assignee |- pl\/member
VIOLATION ( TXT "DelAtom;Assignment;", SRC I)
```

The function 'DelAtom' is predefined, and takes two arguments: 1. the concept from which an atom is to be deleted; 2. the actual atom to be deleted.

Note that when an atom is deleted, also every pair \(in any relation\) is deleted if either its source atom or target atom is the deleted atom.

## Example \(`_;`\)

When you try to create or delete pairs with atoms that contain texts, you may find that some texts contain the semi-colon. When such a text is used in a violation statement, this will be interpreted as an argument separator, causing all sorts of unexpected results. This can be prevented by using `_;` rather than `;` as an argument separator. However, the ExecEngine must be made aware that this alternative argument separator is used. This is done by mentioning it immediately at the beginning of a function call, as in the below example:

```text
VIOLATION (TXT "{EX}_;InsPair_;r1_;A_;", SRC I, TXT "_;B_;", TGT I)
```

Of course, if the SRC or TGT atom is a text that contains the characters `_;`, the problem still remains...

## Example \(`TransitiveClosure`\)

Consider the `r :: A * A [IRF,ASY]`. In relation algebra, terms such as `r+` or `r*` are allowed, designating the transitive closure of `r`. The `+` and `*` operators are currently not supported in Ampersand.

This section describes a workaround that allows you to use transitive closures.To do so, we simply define a relation `rPlus :: A * A` and/or `rStar :: A * A`, and define the following automated rules to populate these relations:

```text
 ROLE ExecEngine MAINTAINS "Grow rPlus"
 RULE "Grow rPlus": r;rPlus \/ rPlus;r |- rPlus
 VIOLATION (TXT "{EX} InsPair;rPlus;A;", SRC I, TXT ";A;", TGT I)

 ROLE ExecEngine MAINTAINS "Shrink rPlus"
 RULE "Shrink rPlus": rPlus |- r;rPlus \/ rPlus;r
 VIOLATION (TXT "{EX} DelPair;rPlus;A;", SRC I, TXT ";A;", TGT I)

 ROLE ExecEngine MAINTAINS "Grow rStar"
 RULE "Grow rStar": r \/ r;rStar \/ rStar;r |- rStar
 VIOLATION (TXT "{EX} InsPair;rStar;A;", SRC I, TXT ";A;", TGT I)

 ROLE ExecEngine MAINTAINS "Shrink rStar"
 RULE "Shrink rStar": rStar |- r \/ r;rStar \/ rStar;r
 VIOLATION (TXT "{EX} DelPair;rStar;A;", SRC I, TXT ";A;", TGT I)
```

While this works \(certainly in theory\), a practical issue is that it quickly becomes very timeconsuming as the population of `r` grows, up to an unacceptable level. Also, Ampersand prototypes have a time limit \(30 or 60 seconds\) for an ExecEngine run. In order to make transitive closures a bit more practicable \(but certainly not workable for 'real' software\), we can use the predefined ExecEngine function `TransitiveClosure`, as follows:

```text
 rCopy :: A * A
 MEANING "a copy of the relation `r`, needed to detect deletions in `r`"

 rPlus :: A * A 
 ROLE ExecEngine MAINTAINS "Warshall on r"
 RULE "Warshall on r": rCopy = r
 VIOLATION (TXT "{EX} TransitiveClosure;r;A;rCopy;rPlus")
```

What this does is the following. Any time that `r` is being \(de\)populated, the rule `Warshall on r` is violated. This calls the \(predefined\) function `TransitiveClosure` with its four arguments, the result of which is that 1. the relation `rPlus` is computed as the \(smallest\) transitive closure of `r` \(using the Warshall algorithm\); 2. the relation `rCopy` is made to have the same population as `r`, thereby resolving all violations of the rule.

Note that if you want to use \(the equivalent of\) `r*` somewhere in an term, the most practical way is to use the term `(I \/ rPlus)` at that spot.

### HELP! I got errors!

As an Ampersand user, you are used to getting error messages from the compiler. Yet, errors in rules for the Exec-engine are not signalled by the compiler. Instead, you get run-time error message that some inexperienced users find hard to work with, as it requires some knowledge of the backgrounds.

Here are some tips. 1. Most \(all?\) predefined functions check for a valid number of arguments. If the error message relates to the number of arguments, 1. you have missed out on a `;`. The function `NewStruct` is well-known to produce this error, because of the wealth of arguments allowed. Learning and maintaining a strict discipline regarding how you write such \(e.g. `NewStruct`\) statements is a big help in preventing this error from occurring. 2. you may have to many `;`s. Of course, you may just mistakenly having written too many `;`s. Another, less known cause is where a violation occurs on an atom that happens to be a text containing one or more `;` characters. This will cause the ExecEngine to interpret the text as multiple arguments, which \(usually\) results in an illegal number of arguments error. The cure is to use the `_;` separator rather than the `;` separator \(see the appropriate section above\). 2. Most \(all?\) predefined functions that have arguments to specify a relation definition, will check \(at run-time\) whether or not this relation is actually defined \(at define-time\). Misspellings in relation or concept names \(e.g. capitalizations\) often cause this error. 3. You should specify a relation just with its name, e.g. `project` \(not: `project[Assignment*Project]`\). The reason for this is that it \(currently\) is the ExecEngine itself that parses the violation string \(rather than the Ampersand compiler\). This very simple parser does not handle `[Assignment*Project]`-like constructs. 4. For the same reason \(simple ExecEngine parser\), there is no type checking for the ExecEngine functions. This means that you must check yourself whether or not the type of atom\(s\) you want to insert or delete match with the source or target atom of the relation you try to \(de\)populate. 5. Look at the log window to get more information on what is actually happening when the ExecEngine executes. You can turn it on by clicking on the left-most icon of the icon-list that is at the right hand side in the menu bar. There, you turn on the 'Show log window'. In the log window, you can select what you do and do not see. Options you may want to select include 'ExecEngine', 'RuleEngine' and 'Database'. 6. If everything else fails, read the error messages and log lines slowly and carefully, as they \(sometimes unexpectedly\) do provide information that may actually help you to resolve the issue at hand.

For the time that researchers are working on this problem, you will have to live with all this. It makes programming of automated rules initially error-prone and time consuming, but when you get the hang of it, it gets better. Still, the best piece of advice we can currently give here is:

* Keep automated rules simple.
* Test thoroughly.

### Ways to run the ExecEngine \(one or more times\)

The ExecEngine currently is a simple one. Whenever it executes, it evaluates the automated rules one after another. Whenever an automated rule produces violations, the associated violation text is executed for every such violation.

While rules are most often evaluated in the order in which they are defined, you really should not make any assumptions about the evaluation order \(nor about the order in which violations of a rule are processed\). Hence, it may happen that the violations are processed 'out of order', resulting in violations of automated rules that could 'easily' have been fixed.

To overcome this issue, the ExecEngine \(by default\) simply runs itself again, until all automated rules have no violations any more, or until the maximum amount of such reruns has been reached - this limit is set to guarantee that execution terminates. The default number of maximum reruns is 10 \(decimal\). You can modify this setting in `LocalSettings.php`, , by \(including and/or\) modifying the following texts:

```text
 Config::set('maxRunCount', 'execEngine', 10);
```

For research or debugging purposes, it may sometimes be neccesary to have further control over the manner in which the ExecEngine does. There are three possibilities for running the ExecEngine:

1. Automatically. This is the default behaviour. Turning the autoRerun feature off means that the ExecEngine will always run only once when called. You can specify this in the file `LocalSettings.php`, by replacing the text `true` by `false` in the line that contains the text:

   `Config::set('autoRerun', 'execEngine', true);`

2. Manually. This is when you, as a user, clik on the `refresh/reset options` icon in a prototype, and the select `Run execution engine`. Note that if you also use logins, you must have been assigned a role that allows you to do this, or you won't see this option.
3. By using the ExecEngine function `RerunExecEngine`, which takes one argument \(an explanatory text, that is used for logging - see the example below\). Whenever the ExecEngine calls this function, a flag is set requesting a rerun. When, at the end of an ExecEngine run, this flag is set, the ExecEngine will run itself again.

Here is an example of how `RerunExecEngine` can be used to create a transitive closure:

```text
 r :: A * A [ASY]
 rStar :: A * A -- This will contain a transitive closure

 ROLE ExecEngine MAINTAINS "InsPair on rStar"
 RULE "InsPair on rStar": r \/ r;rStar \/ rStar;r |- rStar
 VIOLATION (TXT "{EX} InsPair;rStar;A;", SRC I, TXT ";A;", TGT I
           ,TXT "{EX} RerunExecEngine;InsPair on rStar"
           )
 ROLE ExecEngine MAINTAINS "DelPair on rStar"
 RULE "DelPair on rStar": rStar |- r \/ r;rStar \/ rStar;r
 VIOLATION (TXT "{EX} DelPair;rStar;A;", SRC I, TXT ";A;", TGT I
           ,TXT "{EX} RerunExecEngine;DelPair on rStar"
           )
```

