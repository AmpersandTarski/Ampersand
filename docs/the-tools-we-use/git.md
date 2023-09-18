---
description: >-
  We use Git for version control, with the shared Ampersand repository located
  at Github. This chapter provides the information to get to the repository and
  clone it for having your own local copy.
---

# Version control with Git

We use Git for the following purposes:

1. to have a shared repository that contains our source code;
2. to work on different features in parallel without interference and without having to wait for each other;
3. to prevent loss of code because multiple copies exist on multiple sites;
4. to have an accurate registration of changes, their authors, and the time each change has occurred.

We maintain and use the following repositories at Github:

| Repo                                                                                        | purpose                                                                                                                                                                                                                                                                                                                      |
| ------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [Ampersand](https://github.com/AmpersandTarski/Ampersand)                                   | contains the source code of the Ampersand compiler up to the point that all the regular docker images can be generated from this repository.                                                                                                                                                                                 |
| [ampersand-models](https://github.com/AmpersandTarski/ampersand-models)                     | contains a collection of Ampersand models, which are meant for reuse by the public at large.                                                                                                                                                                                                                                 |
| [Ampersand-Language-Support](https://github.com/AmpersandTarski/Ampersand-Language-Support) | contains language support for VS-code, such as syntax coloring, code snippets, and the like.                                                                                                                                                                                                                                 |
| ****[Publications](https://github.com/AmpersandTarski/Publications)                         | This repo is meant for collaborative writing of journal and conference papers about Ampersand.                                                                                                                                                                                                                               |
| [Prototype](https://github.com/AmpersandTarski/Prototype)                                   | contains JavaScript source code of the prototype front-end and back-end generators and runtime code.                                                                                                                                                                                                                         |
| [documentation](https://github.com/AmpersandTarski/documentation)                           | contains the source code of the documentation of Ampersand on GitBook.                                                                                                                                                                                                                                                       |
| [TheToolsWeUse](https://github.com/AmpersandTarski/TheToolsWeUse)                           | contains the source code of the documentation on GitBook of tools used to create Ampersand.                                                                                                                                                                                                                                  |
| [RAP](https://github.com/AmpersandTarski/RAP)                                               | contains the source code of RAP, a tool that lets you analyse Ampersand models, generate functional specifications and make prototypes of information systems. It is the primary tool for students of the Open University of the Netherlands in the course Rule Based Design.                                                |
| [MirrorMe](https://github.com/AmpersandTarski/MirrorMe)                                     | contains the source code for an argument assistance system. It is based on defeasible reasoning along the lines of Toulmin and Hohfeld.                                                                                                                                                                                      |
| [sentinel](https://github.com/AmpersandTarski/sentinel)                                     | (obsolete) Software for automated testing the Ampersand software. Today, most of our testing is done by means of Travis-ci. Before that we heavily used our own sentinel to warn about bugs. Today, Sentinel is only used for issues with known bugs. Things that are known to break, and we didn't find time to solve them. |
| [webFiles](https://github.com/AmpersandTarski/webFiles)                                     | contains all kind of consolidated documents that are related to Ampersand in some way. They are stored here to ensure that we have stable URLs to these documents, so we can hyperlink to them wherever appropriate                                                                                                          |

## Getting started with Git

Understanding the basics of Git is essential for anyone working on Ampersand. Fortunately, there is good help available:

1. [The help at GitHub](https://help.github.com/articles/)

* If you want  write access to the Ampersand repo, create yourself an account at [GitHub](https://www.github.com), if you don't have one already. You don't need that if you just want to read.
  * Ask one of the administrators of Ampersand to add you as member to the team, stating that you want/need write access to the Ampersand repo or any other repo in the project.
* If you need a local copy on your own computer, install [Git for Windows](http://msysgit.github.io/) or [Git for MacOS](https://nl.atlassian.com/git/tutorials/install-git#mac-os-x) to stay abreast with the latest version.
* When installing for Windows:
  * Accept all defaults, except:
  * 4th screen check "Windows Explorer integration" / "Simple context menu" / "Git Bash here"
  * 6th screen "Use Git from the Windows Command Prompt"

## Tools for Git

Git works from the command prompt. There are however some tools that make life easier when you work with Git. Not necessary, but very much appreciated.

* Install [TortoiseGit](https://tortoisegit.org/) to use Git in your Windows Explorer.&#x20;
  * Accept all defaults
* Install [SourceTree](http://www.sourcetreeapp.com) to visualize the history in the most popular Git client.
  * No global ignore, further everything default
  *   When at _Add an account_, select _GitHub_ and supply your credentials.

      Note: there are many other git clients too, many of which are free, e.g. [Github Desktop](https://desktop.github.com/) or [GitKraken](https://www.gitkraken.com/).
* Install [KDiff3](http://sourceforge.net/projects/kdiff3/files/kdiff3/) to get help in merging conflicts. To avoid KDiff3, you can merge conflicts from your editor or IDE as well.
  * To select KDiff3 in SourceTree, go to SourceTree / Tools / Options / Diff and select KDiff3 for External Diff Tool and Merge Tool.
* Pin Git Bash to Start menu to use Git from a command line interface.
* When you use excel-files (\*.xlsx and \*.xls), you can extend git to handle these files properly with [ExcelCompare](https://gist.github.com/PrabhatKJena/0884644ae01a49a9819aebd883e54003). This works for command-line git on Mac. Similar extensions exist for Windows (TBD: which?)

## Configuring Security

To avoid typing a GitHub password for every commit you make, [install an SSH-key](https://help.github.com/articles/connecting-to-github-with-ssh/). Here is the short version:

* **Generate a SSH Key**
  * Start / All programs / Git / Git GUI: Help / Show SSH Key
  * Press Generate Key. You can supply a pass phrase, but that's optional. You should of course keep it for yourself. (it is in _$home/.ssh/id\_rsa_). When you use a pass phrase, you will have to supply it now and then.
* **Access to repositories of AmpersandTarski** (GitHub).&#x20;
  * Go to your [personal settings](https://github.com/settings/profile)
  * press _Add SSH Key_
  * Title: add a description of the machine you currently work on (eg. "Windows Laptop Stef")
  * Key: past your generated key&#x20;
  * Press "Add Key"

## Checking out Ampersand source code

Checking out means to create your own local copy of the repository. By default, the Git directory in your home directory will be used for all local repositories. You have various options to do this

* Use your git client (e.g. [Github Desktop](https://desktop.github.com/), [SourceTree](https://www.sourcetreeapp.com/), or [GitKraken](https://www.gitkraken.com/)) if you want to click instead of typing commands.
* Use your command-line interface:
  *   go to your working directory

      `cd git`
  *   clone the repo with this command:

      `git clone git@github.com:AmpersandTarski/Ampersand.git`
* Use TortoiseGit for integration with your file system
  * First time, you have to configure ssh:&#x20;
    * Start / All Programs / TortoiseGit / Network:
    * At "SSH Client"  fill in:   C:\Program Files (x86)\Git\bin\ssh.exe

## Done?

You are done with this page once you have your local copy of the Ampersand source code on your own computer, under Git. Do this only if you want to change the Ampersand software; not if you only want to use Ampersand.

