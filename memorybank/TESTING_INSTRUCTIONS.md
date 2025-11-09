# Testing Instructions for New Test Set

## Current Setup

You have two test directories:
- `testing/` - Original test set
- `testing_with_populations/` - Enhanced test set with meaningful populations

## Testing the New Test Set

### Option 1: Test New Set Without Replacing Original (Recommended)

To test the new set while keeping the original intact:

```bash
# Backup original
mv testing testing_original

# Use new test set
mv testing_with_populations testing

# Run tests
nice -n 10 time stack test 2>&1 | tee github_regression_test_new.log

# Restore original (if needed)
mv testing testing_with_populations
mv testing_original testing
```

### Option 2: Permanently Replace Original

If you're satisfied with the new test set:

```bash
# Backup original (just in case)
mv testing testing_backup_$(date +%Y%m%d)

# Replace with new test set
mv testing_with_populations testing

# Run tests as usual
nice -n 10 time stack test 2>&1 | tee github_regression_test.log
```

### Option 3: Test Both and Compare

Compare results from both test sets:

```bash
# Test original
nice -n 10 time stack test 2>&1 | tee test_original.log

# Swap directories
mv testing testing_original_tmp
mv testing_with_populations testing

# Test new set
nice -n 10 time stack test 2>&1 | tee test_with_populations.log

# Restore original
mv testing testing_with_populations
mv testing_original_tmp testing

# Compare results
diff test_original.log test_with_populations.log
```

## What to Expect

### Old Test Set Results
- Many tests pass trivially due to empty populations
- Validate tests don't meaningfully test Haskell vs Database semantics

### New Test Set Results
- Same number of tests pass/fail (tests that should pass still pass)
- Better validation coverage due to meaningful populations
- More confidence that Haskell and Database semantics match

## Known Issues

Both test sets have 14 files that trigger compiler bugs (see `COMPILATION_STATUS_REPORT.md`). These are pre-existing issues in Ampersand and unrelated to population additions:

```
testing/Travis/testcases/Bugs/CurrentMultiFile/Bug388_Retina/RETINABUG.adl
testing/Travis/testcases/Misc/ArchiTest3.adl
testing/Travis/testcases/Misc/Kernmodel.adl
testing/Travis/testcases/prototype/shouldSucceed/Issue1281.adl
testing/Travis/testcases/prototype/shouldSucceed/phone.adl
testing/Travis/testcases/prototype/shouldSucceed/try1.adl
testing/Travis/testcases/prototype/shouldSucceed/try10.adl
testing/Travis/testcases/prototype/shouldSucceed/try13b.adl
testing/Travis/testcases/prototype/shouldSucceed/try14.adl
testing/Travis/testcases/prototype/shouldSucceed/try15.adl
testing/Travis/testcases/prototype/shouldSucceed/try18.adl
testing/Travis/testcases/prototype/shouldSucceed/try22.adl
testing/Travis/testcases/prototype/shouldSucceed/try26.adl
testing/Travis/testcases/prototype/shouldSucceed/try7.adl
```

## Quick Test Command

For a quick single-directory test:

```bash
# Test the new populated directory
nice -n 10 time stack test 2>&1 | tee test_new_$(date +%Y%m%d_%H%M%S).log
```

## Recommendation

1. **First**: Test the new set using Option 1 (temporary swap)
2. **Review**: Check that results meet your expectations
3. **Then**: If satisfied, use Option 2 to permanently replace

## Rollback Plan

If you need to roll back:

```bash
# If you have testing_backup_YYYYMMDD
mv testing testing_with_populations_attempted
mv testing_backup_YYYYMMDD testing
```

## Summary

- ✅ You can still use the same command: `nice -n 10 time stack test`
- ✅ Just swap the `testing/` directory before running
- ✅ No changes needed to your test infrastructure
- ✅ All tests are compilable (no parse/fatal errors)
