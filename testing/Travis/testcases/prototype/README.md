# Prototype Tests

This directory contains tests for Ampersand's prototype generation and population functionality.

## Directory Structure

- **ISAtest/** - Tests for ISA (inheritance) relationships in prototypes
- **shouldFail/** - Tests that are expected to fail during prototype generation
- **shouldSucceed/** - Tests that are expected to succeed during prototype generation
  - **WithMeatgrinder/** - Tests specifically for the meatgrinder population functionality

## About Generated Population Files

### What are `*_generated_pop.json` files?

Files with the pattern `*_generated_pop.json` (e.g., `residutest_generated_pop.json`) are **test fixtures** that contain expected population data in JSON format. They are used to verify that the `ampersand population` command produces correct output.

### Why are they committed to Git?

These files are **intentionally versioned** in the repository because:

1. **They are test fixtures, not build artifacts** - They represent the expected output that tests verify against
2. **Ensures test consistency** - All developers and CI environments use the same reference data
3. **Regression testing** - Changes to the meatgrinder can be detected by comparing generated output to these files
4. **Documentation** - They serve as examples of correct population JSON format

### How to update them

If the meatgrinder logic changes intentionally and the output format needs to be updated:

1. Run the affected test to generate new output
2. Verify the new output is correct
3. Update the corresponding `*_generated_pop.json` file
4. Commit the updated file with a clear explanation of why it changed

## Test Configuration

Each test directory contains a `testinfo.yaml` file that specifies:
- Commands to run
- Expected exit codes
- Build recipes to test (Standard, Prototype, Grind, RAP, etc.)

See the parent [testing/Travis/README.md](../../README.md) for more information about the test framework.
