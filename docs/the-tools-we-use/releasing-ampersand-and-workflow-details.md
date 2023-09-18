---
description: >-
  This page is a work instruction for whoever does the periodic release. We use
  github actions to do a release. The aim is to do a release every 4 weeks, but
  it depends on the need to do so.
---

# How to release an Ampersand upgrade

## Branching strategy

* Use your own feature branch to experiment. You will typically spawn it from the main branch. Commit your branch frequently, so other team members can see what you are doing and help out if needed.
* If you intend to merge your results somewhere in the future, describe your intentions now in the file `ReleaseNotes.md` in the root of the repository (in your own branch, of course). This ensures that these release notes will be merged into the main branch when the time is there.
* Use a pull request to announce that you want to merge your branch into the main branch.
* Take ownership of your branch(es). Ask for help to ensure progress and direction.
* The main branch is used only for code that is ready for release. @hanjoosten is currently the primary guardian of the release process.
* The [Ampersand repository at Github](https://github.com/AmpersandTarski/Ampersand/) contains the authoritative source code of Ampersand.

## Releasing at Github

We release Ampersand in arbitrary time intervals. This is done by attaching a release label to the appropriate commit in the main branch.

## Pre-release steps (what to do)

1. In `development` branch, modify the following files[1](releasing-ampersand-and-workflow-details.md#myfootnote1):
   * `package.yaml` :
     * Change the version number to the next release number.
   * `ReleaseNotes.md` :&#x20;
     * Rename the "unreleased changes" section to the new version
     * Add a new "unreleased changes" section for the next release.
2. Push your modifications to GitHub. This will trigger automated testing.
3. Ensure that the build is succesfully:
   * [Check the build](https://github.com/AmpersandTarski/Ampersand/actions) (this could take up to an hour)
4. Create a pull request to the [master from the compare with development](https://github.com/AmpersandTarski/Ampersand/compare/master...development)

[2](releasing-ampersand-and-workflow-details.md#myfootnote2)

![](.gitbook/assets/create-pull-request.PNG)

Press the green button to create the pull request. We create a pull request so the release leaves a proper administrative trail in GitHub, and it triggers the actual release.

Notes, tips and tricks:

[1](releasing-ampersand-and-workflow-details.md): Looking for `package.yaml` and `ReleaseNotes.md`? Want to know where they are located? Look in Github for the commit of the previous release. It shows changes were made to these files. From there, open their current (!) version. Please make sure your Git is working in the development branch. [2](releasing-ampersand-and-workflow-details.md): This and most of the following actions are done from within the ampersand repository at github. [3](releasing-ampersand-and-workflow-details.md): At the bottom of the page in GitHub, you will find the buttons to merge this release. Please drop a note to describe new features.
