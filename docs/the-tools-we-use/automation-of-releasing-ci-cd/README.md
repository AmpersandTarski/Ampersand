---
description: This page describes the release train for Ampersand.
---

# Automation of releasing \(CI/CD\)

### The purpose of automation.

The release of Ampersand is largely automated because we want:

1. to save ourselves work;
2. to release frequently in a predictable rythm, to bring new functionality to users quickly and predictably;
3. reliable releases, to prevent our mistakes to hit users and to avoid delays caused by the release process;
4. reproducible releases, to allow any team members to step in when the release is due.

### What we want to achieve.

#### Version managment

First of all, we want to be **in control** of our software. We use Git\(hub\) to do version management.  We use git flow strategy. We have a master branch that holds the code of the latest stable release. Then we have a development branch that holds the latest added features and bugfixes. We want this branch to be stable too. It must be buildable at all times, and no half-on-its-way functionality should be in it. For new features or other issues, we use feature branches. These branches are work in progress. They might be buildable, or they might not. Feature branches should be used for work in progress only. This keeps the amount of branches manageable. Tags can be created for all kind of other reference purposes to specific commits.

{% hint style="info" %}
To learn more about Git, head over to [this documentation](https://git-scm.com/).
{% endhint %}

#### Automatic build & test

We use [github actions](https://github.com/features/actions) to build and test the code whenever a commit is done on github. Github actions is pretty well [documented](https://docs.github.com/en/actions). Our specific code can be found in the repository at the designated directory:  `.github/workflows/` . 



