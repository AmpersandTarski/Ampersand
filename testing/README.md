# Different types of test functionality

This folder contains several types of test sets. Goal of these tests is to increase the quality of the Ampersand software.

## Adding or changing a regression test

The test suite discovers its tests at runtime: `stack test` walks every directory under `testing/` and, in each directory that contains a `testinfo.yaml`, runs the commands listed there against the `.adl` (and `.archimate`) files in that directory. So to add, move, or change a test you edit files under `testing/` only; `package.yaml`, `ampersand.cabal`, and the other build files stay untouched. The suite fails when it has executed zero tests, so an empty or misplaced test tree comes out red instead of silently green.

## Some history
Until february 2016, we accumulated all kind of tests for Sentinel. Sentinel does a nightly build of the master branch of Ampersand. It contains a test specification that targets several .adl files in the ampersand-model repository. A run is done of all the tests, and a report is created telling how many tests are run, and how many of them failed. 

There were several problems with this aproach of testing:
  * Because the tests are in another repo than ampersand, we could not do specific tests per branch. 
  * Tests do not pass don't stand out from the large amount of tests that pass every night, and that we keep as regression tests. 

## New approach
Starting february 2016, we split the tests into two major types:
  * *Travis:* Tests that are known to pass, and that we keep to make sure that no changes are committed to the master branch, that break these tests. The invariant is that Travis tests must pass. Travis is run at each commit, and nothing can be merged into the master branch, that results in broken tests.
  * *Sentinel:* Tests that are known to point known bugs in Ampersand. These tests cannot be part of the Travis directory, because that would meen that Travis would hoist a red flag. Sentinel will create a report, and so it signals the Ampersand team that work is to be done.

