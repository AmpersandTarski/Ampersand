---
title: Your working environment
sidebar_label: Your working environment
---

# Your working environment

Whoever works with Ampersand uses three tools at once.
Git keeps the files and their history, an editor such as Visual Studio Code shows them,
and more and more of us let an AI assistant work inside that editor.
Each tool has its own idea of where you are working, and the three ideas have to agree,
or the tools start working against each other.
This page explains the three ideas one layer at a time,
and then gives a set-up that works for a student,
for someone who builds an information system with Ampersand,
and for a contributor to Ampersand itself.
If you only want the set-up, skip to [A set-up that works](#a-set-up-that-works).

## Layer 1: Git, branches and working copies

A Git repository is one history of changes, shared through GitHub.
A branch is a line of development inside that history.
The Ampersand repository has a branch `main` for released code
and a branch `documentation` for this site, next to a branch for each feature under construction.
A working copy is a folder on your disk that holds the files of one branch.
`git clone` makes one, and `git checkout` switches it to another branch.

For most work one working copy is enough.
A contributor who works on two branches at the same time can give each branch its own folder.
For instance, a release branch lives in `~/git/Ampersand-release`
while a feature branch lives in `~/git/Ampersand-feature`,
and both folders share the same repository and the same history.
Such an extra folder is what Git calls a worktree,
and `git worktree add ../Ampersand-feature feature` creates one.
A worktree is an ordinary folder with ordinary files,
so an editor and an assistant can work in it as in any other folder.

Git stops here.
It knows nothing about which folders are open in your editor or which assistant is editing them.

## Layer 2: windows, folders and workspaces in the editor

Visual Studio Code shows files in a window, and a window shows one or more folders in its explorer.
A window with one folder is the common case:
you open `~/git/MyProject` and everything you need is there.
A window with several folders is called a multi-root workspace.
VS Code saves the list of folders in a small file with the extension `.code-workspace`,
and double-clicking that file opens a window with exactly those folders.
Such a file looks like this:

```json
{
  "folders": [
    { "path": "Ampersand" },
    { "path": "Ampersand-feature" }
  ]
}
```

The paths are relative to the location of the file.
Keep the file next to the folders it lists and outside any repository,
because it describes your personal desk and not the project.

The folders of the window matter beyond the explorer.
Extensions such as language servers, which index your code and report errors as you type,
work per folder of the window.
A file that you open from outside those folders gets no proper support,
and an extension may even start a separate instance for each such file.
So the rule for the editor is: let the window show every folder in which you,
or an assistant working for you, edit files.

## Layer 3: sessions of an AI assistant

An AI assistant that works inside the editor, such as Claude Code,
holds a conversation with you that we call a session.
A session has a working directory,
and it keeps a transcript of the conversation so that you can resume it later.
In VS Code the working directory of a session is the first folder of the window,
and the assistant lists the sessions it can resume per working directory.
We verified this for Claude Code.
Other assistants may organise this differently, so check how yours does it.

Two consequences follow.
First, a session can edit any file on your disk,
also a file in a worktree that is not a folder of the window.
That is how the editor loses track, as layer 2 explained.
Second, if you change the first folder of a window,
the sessions that belong to the old first folder disappear from the resume list
until you open a window with that folder in first place again.

## A set-up that works

The three layers agree when the folders you edit are folders of the window,
and the first folder stays the same for as long as you want to resume your sessions.
What that means in practice depends on what you do with Ampersand.

### As a student

Work in RAP, the web environment of the course, and you need none of this: there is no repository,
no window and no session on your own computer.
Do you want to work on your own computer, then make one folder for your scripts,
open that folder in one window, and keep an assistant's session in that same window.
Put the folder under Git when you want a history of your attempts.
`git init` in the folder is enough, and GitHub is optional.

### As a builder of an information system

Give each project its own repository and its own folder, and open each project in its own window.
Your scripts, your population files and the Docker files that run your prototype
all live in that folder,
so one window shows all of it.
Use a branch for an experiment that may fail, such as a redesign of the interfaces.
Merge it when it works and delete it when it does not.
An assistant's session started in that window has the project folder as working directory,
so it finds your scripts and the generated prototype where you expect them.

### As a contributor

A contributor often has several branches of the Ampersand repository in progress: a feature,
a fix for a release, and the documentation.
Give each branch its own worktree, so that a build in one does not disturb the other.
Then open them together in one multi-root workspace, with the main working copy as first folder.
The first folder stays the same, so all your sessions remain in the resume list,
and every worktree is a folder of the window,
so the language server and the assistant agree on where the files are.
When a new worktree comes into use,
add its folder with *File > Add Folder to Workspace* and save the workspace file.
When the branch is merged, remove the folder from the workspace
and remove the worktree with `git worktree remove`.
Keep the list to the worktrees you edit,
because a language server loads the whole project for each folder it serves,
and that costs memory for each folder.
