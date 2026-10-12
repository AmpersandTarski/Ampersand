# Demonstration: migrating to a new release without downtime

This demonstration shows how an information system moves to a new release while its users keep working.
The new release has a rule that the data in production does not satisfy yet.
The demonstration follows the method of
[*Data Migration under a Changing Schema in Ampersand*](https://doi.org/10.1007/978-3-031-68279-7_11)
(Joosten and Joosten, RAMiCS 2024),
written as a system of contexts: a migration context that includes the existing and the desired system.
The guide [Systems of contexts](../../docs/guides/systems-of-contexts.md) explains the language behind it.

## Watching it

To see the demonstration without running it, watch the video (7 minutes and 41 seconds, in English):
<https://youtu.be/DUtOYUZfkI8>.
It walks through the example `garden` and shows the three scripts.

## Running it

You need Docker and the image of the prototype framework, version 2.17.0 or later.
That version refuses a transaction that adds a violation of a relaxed invariant, which is what the demonstration shows.

```sh
./demo.sh up garden        # or: ./demo.sh up kurk, ./demo.sh up tuin
```

Then open <http://localhost:8200>.
The first time, the script builds the images of the three systems, which takes a few minutes.
After that, the existing system is ready within a minute.
`./demo.sh down garden` removes everything again: the containers, the network and the data.

One example runs at a time, because the examples use the same two ports, 8200 and 8201.
Other ports are set with `REGIE_PORT` and `APP_PORT`.
The page follows the language of the example: `garden` is in English, `kurk` and `tuin` are in Dutch.
The link in the upper right corner switches the language of the page.

By default the images build on `ampersandtarski/prototype-framework:latest`.
Another image is given with the environment variable `FRAMEWORK_IMAGE`.
To build the framework image from source, with a compiler built from this repository:

```sh
docker buildx build --platform linux/amd64 --load -t ampersandtarski/ampersand:local <this repository>
docker buildx build --platform linux/amd64 --load -t ampersandtarski/prototype-framework:local \
  --build-arg COMPILER_IMAGE=ampersandtarski/ampersand:local <the repository of the prototype framework>
FRAMEWORK_IMAGE=ampersandtarski/prototype-framework:local ./demo.sh up garden
```

## What you see

The page has three buttons at the top.
On the left is a picture of what is running, which is refreshed every second.
On the right is the application as a user sees it.
The user stays at one address; a small proxy, called the flipper, decides which system serves him.

| Moment | What is running | Where the user works | Which button is active |
| --- | --- | --- | --- |
| At the start | the existing system | existing system | Start migration |
| After "Start migration" | the migration system and the desired system as well | existing system | Switch over |
| After "Switch over" | all three | migration system | Clean up migration, once every violation is repaired |
| After "Clean up migration" | the desired system | desired system | none |

Until the switch, the user works in the existing system, and everything that system allows is allowed.
The migration system takes over each change within a few seconds; "Switch over" is grey during those seconds.

After the switch, the user has the desired system in front of him, with one exception.
The violations of the new rule that came along from the existing system are on his screen as work to do.
The migration system refuses a transaction that adds a violation, as the desired system would.
Try it: a new row without a value is refused, and a row that has been repaired cannot lose its value again.
When the last violation is repaired, the rule works as an invariant and "Clean up migration" becomes active.

"Clean up migration" turns the flipper to the desired system.
Then it removes the containers of the migration system and of the existing system, and their two databases.
"Start again" brings the demonstration back to the existing system, with its original data.

## The three examples

`kurk` is the proof of concept of the paper.
The existing system has one relation, `r[A*B]`, and no rule; the atoms a2 and a3 have no B.
The desired system requires `r` to be total.

`garden` tells the same with a story.
A community garden registers which volunteer keeps which plot.
Two plots without a keeper were overgrown, and the board decided that every plot has a keeper.
The new release of the register has that rule as an invariant.
During the migration the gardeners keep working, and they find a keeper for the Bean rows and the Berry corner.
`tuin` is the same example in Dutch.

In `garden` and `tuin` the desired system has other screens than the existing system,
and after the switch the migration system shows those new screens.
The list of plots cannot be edited there.
The three dots behind a row open the screen of that one plot, a `TRANSACTIONAL INTERFACE`,
which collects the changes of the user and stores them together when he presses Save.
So a new plot gets its name and its keeper in one transaction, as the new rule demands.

Each example consists of three scripts: `existing.adl`, `desired.adl` and `migration.adl`.
The script of the desired system contains nothing that serves the migration.
The migration script includes the other two as `old` and `new`, copies the data,
and assigns the new rule to a role, for instance `ROLE Volunteer MAINTAINS new.everyPlotHasKeeper`.

## How it works

Each of the three systems is an application of its own, in a container of its own, with a database of its own.
Their images come from the Dockerfiles that `ampersand deploy` generated in the directory `deploy` of the example.
Nothing in the applications serves the demonstration.

The director (`regie/regie.py`) runs in a container of its own and works Docker.
It serves the page with the buttons, starts and removes the containers, and tells the flipper where the traffic goes.
The flipper is an nginx that forwards every request to one of the three systems.
It only points to a system that has been installed,
because an application without its database answers every request with an error.

The director reads two things from the systems.
Before the switch it compares a checksum of the database of the existing system
with the checksum at the last time the migration system caught up.
After the switch it counts the violations of the new rule, which the migration system keeps in its database.

The three systems share one database server, because the migration system reads two databases in one query.

## Checking that the demonstration works

```sh
./test.sh garden           # or: ./test.sh kurk, ./test.sh tuin
```

The script needs `curl` and `jq`.
It starts the demonstration afresh, presses the three buttons and "Start again" through the director,
and works in the applications through the flipper, as a browser does.
At each step it checks what the audience is told: which containers run, where the traffic goes,
which button is active, that a new violation is refused, and that a repaired violation does not return.
Run it after a change of a script or of the framework.
`KEEP=1 ./test.sh garden` leaves the demonstration running at the end.

## Changing a script

The directory `deploy` of an example is generated.
After a change of a script, generate it again and start the demonstration, which rebuilds the images:

```sh
cd garden && ampersand deploy migration.adl --output-dir deploy
```

The file `demo.json` of an example names which of the three systems is the existing, the desired and the migration system,
which screen the page opens, and which text the page shows under "The story".
