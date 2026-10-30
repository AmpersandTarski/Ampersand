# Database rights in a system of contexts

This note answers a question that the build of systems of contexts left open:
which rights should the database user of an application have on the databases of other contexts?
It reports what was measured, and ends with the solutions to choose from.

## The question

In a system of contexts every context has a database of its own,
and all databases share one MariaDB server.
The application of a context reaches a table of another context through a view in its own database.
The generated deployment gives all applications one database user with all rights on everything (`GRANT ALL PRIVILEGES ON *.*`).

The semantics says more precisely what an application does.
The specification `AmpersandData/FormalAmpersand/MultiContext.adl` has two relations for it.
A context *reads* the databases of the contexts it reaches by inclusion.
A context *writes* its own database,
and that of another context only where one of its rules is enforced on a relation of that context,
or where it states a classification whose generic concept belongs to that context.

So the rights that the database grants can follow the semantics, or stay wider than the semantics.
Both are legitimate; the difference is what protects the data when something goes wrong.

## What can go wrong

Four things can go wrong, and each solution below is judged on them.

1. **The compiler or the framework writes where the semantics does not allow it.**
   This happened during this research.
   When a user deleted an atom from the desired system,
   the framework also deleted the pairs of that atom in a relation of the existing system.
   With all rights for everybody, nobody noticed.
   With rights per database, the server refused the statement (`UPDATE "old.A" SET "r" = NULL`),
   and the defect was found and repaired.
2. **An application is taken over**, through a defect in a custom extension or in the framework.
   The intruder can do what the database user of that application can do.
3. **A deployment is configured wrongly.**
   An application drops and creates its own database when it is installed.
   If two applications get the same database name by mistake,
   the second installation destroys the data of the first.
4. **An application reads what it has no business with**:
   a database on the same server that its context does not reach.

## What MariaDB allows

The script `spike-database-rights.sh`, next to this note,
starts a MariaDB 10.6 server with the settings of the framework
and tries each of the statements below as a restricted user.
These are its findings.

| What was tried | Outcome |
| --- | --- |
| Create a view on a table of another database, without a right on that table | Refused. So the rights have to be there before an application is installed. |
| The same, with `SELECT` on that table | Allowed, and the view can be read. |
| Insert, update or delete through that view, with `SELECT` only | Refused. |
| Update one column through the view, with `UPDATE` on that column only | Allowed for that column, refused for the other columns. |
| Insert through the view, with `INSERT` on the table | Allowed; a delete is still refused. |
| Drop the database of another context | Refused. |
| Drop and create the own database, with all rights on it | Allowed, and the rights on it remain. |
| Grant a right on a database or a table that does not exist yet | Allowed. |
| Grant a right on a column of a table that does not exist yet | Refused. |
| The owner drops and creates its database | The rights of others on its tables and columns remain, and their views work again as soon as the tables exist. |
| One transaction that writes two databases, rolled back | Nothing remains in either database. |
| A third user reads a view of an application | Allowed, without any right on the table behind it: a view runs with the rights of the user that created it. With `SQL SECURITY INVOKER` it is refused. |

Three conclusions follow.

- MariaDB can express the semantics exactly.
  A relation is stored as a table or as a column of a table,
  and MariaDB grants rights per table and per column.
- Rights per database and per table can be granted before anything is installed,
  from a script that the compiler generates.
  Rights per column need the table to exist,
  so they have to be granted after the owner is installed.
- A refusal comes as error 1356, "view references invalid table(s) or column(s) or definer/invoker of view lack rights to use them".
  The message does not say which right is missing.

## What the framework needs

The scenario `migration` in the prototype framework now runs with one database user per application (`test/multi-context/scenarios/migration/grants.sql`).
The rights follow the semantics at the level of databases:

| Application | Own database | Database of the existing system | Database of the desired system |
| --- | --- | --- | --- |
| Existing system | all rights | | none |
| Desired system | all rights | none | |
| Migration | all rights | `SELECT` | `SELECT`, `INSERT`, `UPDATE`, `DELETE` |

All 23 checks of the scenario pass with these rights, up to the moment of completion.
So the framework needs nothing beyond what the semantics allows.
In particular it needs no right to create or drop anything outside its own database.

## Solutions

Five solutions follow.
The first three differ in what the database grants; the last two can be added to any of them.

### 1. One user with all rights on the databases of the system

All applications share one database user.
Narrowing `*.*` to the databases of the system is a small step that keeps other systems on the same server out of reach.

*Security.*
The database protects nothing inside the system.
A defect in the compiler or the framework, an intruder,
and a wrong database name all have the whole system within reach.
What protects the data is the correctness of the compiler and the framework.

*Manageability.*
One user, one password, one grant statement that never changes.
A new context, a new rule or a new version asks for nothing.

### 2. One user per context, rights per database

Every application has a database user of its own: all rights on its own database,
`SELECT` on the databases its context reaches, and `INSERT`,
`UPDATE` and `DELETE` on the databases its context writes.
The compiler knows all of this, so `ampersand deploy` can generate the users and the grants.

*Security.*
The server refuses a write that the semantics does not allow at the level of contexts,
which is how the defect above was found.
An intruder reaches the databases that the context reaches,
and can change those that the context writes.
A wrong database name cannot destroy the database of another context,
because no application can drop it.
An application cannot read a database that its context does not reach.
Within a database it writes, an application can still change every table.

*Manageability.*
One password per application.
The grants can be issued before anything is installed,
and they change only when an inclusion changes or when a context starts to write another one.
A new version of a context has a new database name,
so the grants are generated with the names of the deployment.

### 3. One user per context, rights per table and column

As solution 2, with the rights to write narrowed to the tables and columns of the relations and concepts that the context writes.
This is the semantics, exactly.

*Security.*
The server refuses every write that the semantics does not allow.
An intruder can change only the relations that the context is specified to write.

*Manageability.*
The grants change with the model:
a new enforced rule on a relation of another context asks for a new grant.
A relation that is stored as a column needs its grant after the owner is installed,
so the installation gets a step that depends on the order.
A refusal does not say which right is missing,
so a deployment with stale grants is hard to diagnose.

### 4. The framework refuses, whatever the database grants

The compiler tells the framework which tables of other contexts the context writes.
The framework refuses any other write to a table of another context,
with a message that names the relation and the rule of the semantics.

*Security.*
It catches the first thing that can go wrong, a defect in the framework's own behaviour,
only as far as the check itself is right.
It does nothing against an intruder, who does not go through the framework's checks.

*Manageability.*
Nothing to deploy and nothing to keep in step: the check travels with the generated model.
It gives the clear message that the database does not give.

### 5. An application writes another context through the API of its owner

The owner of a database is the only one that writes it.
A context that enforces a rule on a relation of another context asks the application of that context to make the change, and that application guards its own rules.

*Security.*
The database rights become simple and strict: all rights on the own database,
`SELECT` on the databases that are read.
Every write passes the rules of the owner.

*Manageability.*
The applications depend on each other at run time,
and a transaction over two contexts is no longer atomic.
The framework needs a way to undo half a transaction.
This is a different architecture, and the specification and the article assume one atomic step.

## The solutions side by side

| | 1. All rights | 2. Per database | 3. Per table and column | 4. Framework refuses | 5. Through the API |
| --- | --- | --- | --- | --- | --- |
| A write outside the semantics is refused | no | between contexts | always | by the framework only | always |
| What an intruder can change | the whole system | the databases the context writes | the relations the context writes | as the database allows | its own database |
| A wrong database name can destroy another database | yes | no | no | as the database allows | no |
| An application reads a database it does not reach | yes | no | no | yes | no |
| Database users to manage | 1 | one per context | one per context | unchanged | one per context |
| When the grants change | never | with the inclusions | with the model | never | with the inclusions |
| Work to build it | none | generate users and grants in `ampersand deploy` | as 2, with a step after installation | owner of every table in the generated model, a check in the plug | a new way of writing in the framework |

## Recommendation

I recommend solution 2 as what `ampersand deploy` generates,
with solution 4 added for its messages,
and solution 1 kept as an option for a developer's own machine.

Solution 2 follows the semantics where it matters most, between contexts.
It costs one password per application and grants that rarely change.
It needs no step after installation.
Its value was measured: it found a defect on the first run.
Solution 3 adds protection inside a database that a context is allowed to write anyway,
and pays for it with grants that change with every model
and an installation that depends on the order.
That is worth considering for a system in
which one context writes a small part of a large database of another organisation.
Solution 5 is the answer if databases move to different servers, and it is a project of its own.

What solution 2 needs:

- the compiler tells which contexts a context writes, next to the contexts it reaches,
  in `system.json`;
- `ampersand deploy` generates one user per context and the grants,
  with the database names from the environment;
- the compose file gives every application its own user and password;
- the views of an application keep running with the rights of that application,
  which is what MariaDB does by default.
