# Ampersand Testing Infrastructure Guide

## Introduction

Ampersand is a domain-specific language (DSL) and compiler for building information systems based on relational algebra and set theory. The Ampersand compiler transforms high-level business rules written in ADL (Ampersand Definition Language) into working software prototypes, including databases, APIs, and user interfaces.

This document provides a comprehensive guide to Ampersand's testing infrastructure for software engineers who want to contribute to the project or understand how quality assurance is implemented.

## Overview of Testing Strategy

Ampersand employs a multi-layered testing approach designed to ensure reliability and prevent regressions:

### Two-Tier Test Classification

1. **Travis Tests** (`testing/Travis/`): Regression tests that **must pass**
   - These are "green" tests that represent the current working state
   - Any failure blocks merging to the main branch
   - Run automatically on every commit via GitHub Actions

2. **Sentinel Tests** (`testing/Sentinel/`): Tests for **known issues**
   - These tests document bugs and incomplete features
   - They are expected to fail and don't block development
   - Provide visibility into technical debt and future work

This separation prevents the common problem where legitimate failures get lost in noise from known issues.

## Project Structure

```
testing/
├── README.md                    # Basic testing overview
├── Travis/                      # Regression tests (must pass)
│   ├── README.md
│   └── testcases/
│       ├── Archimate/          # ArchiMate integration tests
│       ├── Bugs/               # Fixed bug regression tests
│       ├── Check/              # Validation tests
│       ├── FuncSpec/           # Functional specification tests
│       ├── meatgrinder/        # Stress tests
│       ├── Misc/               # Miscellaneous tests
│       ├── Parsing/            # Parser tests
│       ├── Preprocessor/       # Preprocessor tests
│       ├── prototype/          # Prototype generation tests
│       ├── Simple/             # Basic functionality tests
│       └── tutorial/           # Tutorial example tests
├── Sentinel/                   # Known issue tests
└── performance/                # Performance benchmarks
```

## Running Tests

### Full Regression Test Suite

```bash
# Build and run all tests (recommended)
stack test --flag ampersand:buildAll

# Alternative: Build first, then test
stack build --flag ampersand:buildAll
stack test
```

### Running Specific Test Categories

```bash
# Navigate to a specific test directory
cd testing/Travis/testcases/Simple

# Run ampersand on individual test files
ampersand validate DeliverySimple.adl --verbose
```

### Manual Test Execution

Each test directory contains a `testinfo.yaml` file that specifies:
- Commands to run
- Expected exit codes
- Additional test parameters

Example `testinfo.yaml`:
```yaml
testCmds:
  - command: ampersand validate --verbose
    exitcode: 0
```

## Test Configuration Format

### Test Directory Structure

Each test case directory typically contains:
- `*.adl` files: Ampersand source files to test
- `testinfo.yaml`: Test configuration
- `*.ifc` files: Interface definitions (optional)
- `*.css` files: Styling (optional)
- Include files: Supporting ADL modules

### Test Commands

Common test commands include:
- `ampersand validate --verbose`: Parse and validate ADL files
- `ampersand check`: Perform consistency checks
- `ampersand proto`: Generate prototypes
- `ampersand population`: Test population handling

## Adding New Tests

### For Bug Fixes (Travis Tests)

1. Create a new directory in appropriate category under `testing/Travis/testcases/`
2. Add your `.adl` test files
3. Create `testinfo.yaml` with expected behavior
4. Ensure tests pass locally
5. Submit PR - CI will verify tests pass

Example structure:
```
testing/Travis/testcases/Bugs/Issue123/
├── BugReproduction.adl
└── testinfo.yaml
```

### For Known Issues (Sentinel Tests)

1. Create test case under `testing/Sentinel/`
2. Document the expected failure
3. These tests help track progress on known issues

### Best Practices for Test Creation

1. **Make tests atomic**: Each test should verify one specific behavior
2. **Use descriptive names**: Test files should clearly indicate what they test
3. **Include documentation**: Add comments in ADL files explaining the test purpose
4. **Test edge cases**: Include boundary conditions and error scenarios
5. **Keep tests fast**: Avoid unnecessarily complex test cases

## Continuous Integration

### GitHub Actions Workflow

The CI pipeline (`.github/workflows/ci2.yml`) runs tests on:
- **Ubuntu 22.04**: Primary Linux testing environment
- **macOS 13**: Cross-platform compatibility
- **Windows 2022**: Windows support verification
- **Docker**: Containerized environment testing

### CI Process Flow

1. **Code checkout**: Retrieve latest code
2. **Environment setup**: Install dependencies (MariaDB, PHP, etc.)
3. **Build**: Compile with `stack build --flag ampersand:buildAll`
4. **Test execution**: Run `stack test` automatically
5. **Artifact publishing**: Push Docker images (main branch only)

### Dependencies

Tests require:
- **Haskell Stack**: Build system
- **MariaDB 11.5**: Database backend
- **PHP 8.0+**: Runtime for generated prototypes
- **System tools**: Standard POSIX utilities

## Test Categories Explained

### Simple Tests
Basic functionality verification:
- Parser correctness
- Type checker operation
- Basic validation

### FuncSpec Tests
Functional specification validation:
- Business rule interpretation
- Logic verification
- Constraint checking

### Parsing Tests
Language syntax verification:
- Token recognition
- Grammar correctness
- Error handling

### Prototype Tests
Generated code verification:
- Database schema generation
- API endpoint creation
- Interface generation

### Preprocessor Tests
ADL preprocessing verification:
- Include file handling
- Macro expansion
- Text substitution

## Debugging Test Failures

### Local Debugging

1. **Run individual tests**:
   ```bash
   cd testing/Travis/testcases/Specific/TestCase
   ampersand validate TestFile.adl --verbose
   ```

2. **Enable debug output**:
   ```bash
   ampersand validate TestFile.adl --verbose --dev
   ```

3. **Check trace output**: The type checker includes trace statements for debugging disambiguation issues

### CI Debugging

1. **Check GitHub Actions logs**: Review detailed build and test output
2. **Compare environments**: Ensure local environment matches CI
3. **Test incrementally**: Isolate the failing component

## Performance Testing

### Performance Test Suite

Located in `testing/performance/`, these tests:
- Measure compilation time
- Track memory usage
- Verify scalability with large models

### Running Performance Tests

```bash
cd testing/performance
# Follow specific performance test instructions
```

## Contributing to Testing Infrastructure

### Areas for Improvement

1. **Test Coverage**: Add tests for uncovered code paths
2. **Performance Benchmarks**: Expand performance test suite
3. **Error Message Testing**: Verify error message quality
4. **Integration Tests**: End-to-end workflow verification

### Code Quality Gates

All code changes must:
1. Pass existing regression tests
2. Include new tests for new functionality
3. Maintain or improve test coverage
4. Follow established test patterns

## Troubleshooting Common Issues

### Test Environment Setup

**MariaDB Connection Issues**:
```bash
# Check MariaDB is running
systemctl status mariadb
# Or for macOS:
brew services list | grep mariadb
```

**Stack Build Issues**:
```bash
# Clean build
stack clean
stack build --flag ampersand:buildAll
```

**Permission Issues**:
```bash
# Ensure test files are readable
chmod +r testing/Travis/testcases/**/*.adl
```

### Known Limitations

1. **macOS Test Skipping**: Some tests are temporarily disabled on macOS due to MariaDB compatibility issues
2. **Windows Path Handling**: File path tests may behave differently on Windows
3. **Timing Sensitivity**: Some tests may be sensitive to system performance

## Future Development

### Planned Improvements

1. **Test Parallelization**: Running tests in parallel for faster feedback
2. **Property-Based Testing**: Adding QuickCheck-style property tests
3. **Mutation Testing**: Verifying test suite completeness
4. **Docker Test Environment**: Standardized testing environment

### Integration Opportunities

1. **IDE Integration**: Real-time testing in development environments
2. **Git Hooks**: Pre-commit test execution
3. **Incremental Testing**: Only testing changed components

## Resources

- **Main Repository**: https://github.com/AmpersandTarski/Ampersand
- **GitHub Actions**: `.github/workflows/ci2.yml`
- **Test Examples**: `testing/Travis/testcases/`
- **Issue Tracker**: GitHub Issues for bug reports and feature requests

---

This documentation is part of the Ampersand project's contributor guide. For questions or improvements to this guide, please open an issue or submit a pull request.
