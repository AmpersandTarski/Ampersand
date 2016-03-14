# Automatic test functionality

To increase the quality of the Ampersand software, every time a commit is done in the Git repo, the executable is built from scratch, and a couple of tests are performed. The directory testcases contains those tests. 

The tests consist of one or more Ampersand scripts in a directory. All those scripts are tested based on information from the file `testinfo.yaml` in that directory. An Ampersand script must have the extention `.adl` to be recognized as such. This enables having `INCLUDE` files in the same directory, without them being tested on its own. 