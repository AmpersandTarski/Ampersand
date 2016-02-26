# Sentinel test functionality

`Sentinel` will run the tests in this direcory every night. Testcases for Sentinel tipically are the ones that are testcases for unresolved issues. Sentinel wil signal about testcases that do not pass the tests. Once a corresponding issue is fixed, that test will pass. It is recomended to move that testcase to the Travis directory as a regression test. 