# Ampersand Compiler: Complete Build, Deploy & Contribution Process

This document provides comprehensive instructions for contributors who need to understand how changes in the Ampersand repository are built, tested, and deployed into production use.

## Quick Reference

### Essential Commands
```bash
# Local development build
stack build

# Build with all executables
stack build --flag ampersand:buildAll

# Install locally (to ~/.local/bin)
stack install

# Run tests
stack test

# Build Docker image
docker build . --tag ampersandtarski/ampersand:latest
```

### Key Files
- `package.yaml` - Project configuration, version, dependencies
- `stack.yaml` - Stack resolver, build settings, extra dependencies
- `Setup.hs` - Custom build logic (generates build info and static files)
- `Dockerfile` - Multi-stage Docker build
- `.github/workflows/ci2.yml` - Main CI/CD pipeline
- `.github/workflows/release.yml` - Release automation

## Complete Build System Architecture

### 1. Build Tools & Configuration

**Primary Build System:** Haskell Stack with Cabal-style configuration
- **Stack Version:** LTS-22.39 (GHC 9.6.6)
- **Build Type:** Custom (uses Setup.hs for pre-build code generation)
- **Current Version:** 5.5.3 (in package.yaml)

**Key Configuration Files:**
- `package.yaml` - Main project configuration (replaces .cabal file)
- `stack.yaml` - Stack-specific settings, resolver, extra dependencies
- `Setup.hs` - Custom build hooks for code generation

### 2. Pre-Build Code Generation (Setup.hs)

Before each build, Setup.hs automatically generates two critical Haskell modules:

**BuildInfo_Generated.hs:**
- **Location:** `src/Ampersand/Basics/BuildInfo_Generated.hs`
- **Contents:** 
  - Cabal version from package.yaml
  - Git revision info (branch:SHA + dirty flag)
  - Build timestamp
- **Purpose:** Version information available at runtime

**StaticFiles_Generated.hs:**
- **Location:** `src/Ampersand/Prototype/StaticFiles_Generated.hs`  
- **Contents:** Embedded static files as compressed archives
  - Pandoc templates (`outputTemplates/`)
  - Formal Ampersand scripts (`AmpersandData/FormalAmpersand/`)
  - Prototype context (`AmpersandData/PrototypeContext/`)
- **Purpose:** Self-contained executable with all necessary files

**Build Optimization:** Setup.hs compares content before regenerating to avoid unnecessary recompilation.

### 3. Project Structure & Executables

**Main Executables Built:**
- `ampersand` - Main compiler executable
- `ampPreProc` - Preprocessor (built only with `--flag ampersand:buildAll`)

**Library Structure:**
- All source code in `src/Ampersand/` organized by functional area
- Extensive module exports defined in package.yaml
- Test suite in `app/Test/`

**Memory Management:** 
- Stack configured with `jobs: 4` to prevent out-of-memory during compilation
- Specific workaround for issue #1040

## Local Development Workflow

### 1. Development Environment Setup

**Option A: DevContainer (Recommended)**
```bash
# Open in VS Code with devcontainer
# Uses .devcontainer/devcontainer.json
# Includes all dependencies pre-configured
```

**Option B: Manual Stack Setup**
```bash
# Install Stack if not already installed
curl -sSL https://get.haskellstack.org/ | sh

# Install system dependencies (varies by platform)
# MariaDB, GraphViz, PHP (for testing)

# Build dependencies
stack build --dependencies-only
```

### 2. Development Build Process

**Standard Development Build:**
```bash
# Build main executable only
stack build

# Build with all executables  
stack build --flag ampersand:buildAll

# Build with copying to ~/.local/bin
stack build --copy-bins --flag ampersand:buildAll
```

**Development Testing:**
```bash
# Run test suite
stack test

# Run specific test
stack test --ta "--pattern YourPattern"

# Build and test together
stack build && stack test
```

**Debugging Build Issues:**
```bash
# Verbose build output
stack build --verbose

# Clean build (nuclear option)
stack clean && stack build
```

### 3. Working with Generated Files

**Key Points:**
- Never edit `BuildInfo_Generated.hs` or `StaticFiles_Generated.hs` manually because the Ampersand compiler regenerates these files on each build.
- Changes to `outputTemplates/`, `AmpersandData/` trigger regeneration
- Git should ignore these generated files

**Viewing Generated Content:**
```bash
# Check what version info will be embedded
cat src/Ampersand/Basics/BuildInfo_Generated.hs

# See what static files are embedded
grep -A 50 "allStaticFiles" src/Ampersand/Prototype/StaticFiles_Generated.hs
```

## Continuous Integration Pipeline

### 1. CI Trigger Conditions

**Triggers:** Push to any branch (except tags)
**Exclusions:** Changes only to `docs/` directory skip CI

**Matrix Testing:**
- Ubuntu 22.04 (Linux)
- macOS-13 
- Windows 2022
- Docker build

### 2. CI Build Process

**For Each Platform:**
1. **Setup Environment:**
   - Checkout code
   - Install MariaDB 11.5
   - Setup PHP (Windows only)
   - Configure platform-specific dependencies

2. **Build Process:**
   - Uses `freckle/stack-action@v5` for Haskell builds
   - Build arguments: `--copy-bins --flag ampersand:buildAll`
   - Parallel builds with caching
   - Memory optimization on all platforms

3. **Testing:**
   - Full test suite on Ubuntu and Windows
   - macOS tests temporarily disabled due to MariaDB issue (MDEV-35173)

4. **Docker Build (Ubuntu only):**
   - Multi-stage build using Dockerfile
   - Tagged as `ampersandtarski/ampersand:latest`
   - Pushed to DockerHub only for main branch

**Build Artifacts:**
- Platform-specific binaries stored as GitHub Actions artifacts
- Docker image pushed to DockerHub registry

### 3. Quality Assurance Workflows

**Additional CI Workflows:**
- `codeQuality.yml` - Code quality checks
- `ormolu-formatting-code.yml` - Haskell code formatting
- `hadolint.yml` - Dockerfile linting
- `triggerDocsUpdate.yml` - Documentation site updates

## Release Process

### 1. Release Trigger

**Manual Process:** Creating a GitHub release triggers the release workflow

**Automatic Actions:**
1. Build binaries for all platforms (Linux, macOS, Windows)
2. Create Docker image with semantic version tags
3. Attach platform-specific binary archives to release
4. Push versioned Docker images to DockerHub

### 2. Release Artifacts

**Binary Distributions:**
- `ampersand-{version}-Linux-binaries.zip`
- `ampersand-{version}-macOS-binaries.zip` 
- `ampersand-{version}-Windows-binaries.zip`
- `ReleaseNotes.md` attached to release

**Docker Images:**
- `ampersandtarski/ampersand:v{version}` (specific version)
- `ampersandtarski/ampersand:v{major}.{minor}` (minor version)
- `ampersandtarski/ampersand:v{major}` (major version)
- `ampersandtarski/ampersand:latest` (always latest release)

### 3. Version Management

**Version Source:** `package.yaml` version field (currently 5.5.3)
**Version Extraction:** Release workflow extracts version using `ampersand --numeric-version`
**Semantic Versioning:** Follows major.minor.patch pattern

## Docker Deployment

### 1. Docker Build Process

**Multi-Stage Build:**

**Stage 1 (buildstage):**
- Base: `haskell:9.6.6`
- Install system dependencies (GraphViz, build tools, etc.)
- Copy Stack configuration first (for layer caching)
- Build dependencies with `stack build --dependencies-only`
- Copy source code and build with `stack install`

**Stage 2 (runtime):**
- Base: `ubuntu` (minimal)
- Install only GraphViz for runtime
- Copy binary from buildstage: `/root/.local/bin/ampersand` → `/bin/ampersand`
- Configure volume mount point `/scripts`

**Usage Pattern:**
```bash
# Mount current directory and run Ampersand command
docker run -it --name ampersand-container \
           -v ${PWD}:/scripts \
           ampersandtarski/ampersand:latest \
           <ampersand-command>
```

### 2. Docker Registry

**DockerHub Repository:** `ampersandtarski/ampersand`

**Tagging Strategy:**
- `:latest` - Latest main branch build
- `:v{semantic-version}` - Specific release versions
- Version tags created automatically on GitHub releases

**Image Dependencies:** Other projects (prototype framework) depend on this image having the binary at `/bin/ampersand`

## Contributing Changes: Complete Workflow

### 1. Pre-Contribution Checklist

**Development Environment:**
- [ ] DevContainer setup OR manual Stack installation
- [ ] MariaDB/MySQL available for testing
- [ ] PHP available (if testing prototypes)
- [ ] GraphViz installed

**Code Style:**
- [ ] Follow existing Haskell style conventions
- [ ] Use Ormolu for formatting: `stack exec -- ormolu --mode inplace $(find . -name '*.hs')`
- [ ] Add appropriate documentation comments

### 2. Development & Testing Process

**Making Changes:**
1. **Create Feature Branch:**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Develop & Build:**
   ```bash
   # Iterative development
   stack build
   
   # Test your changes
   stack test
   
   # Test specific functionality
   stack exec -- ampersand --help
   ```

3. **Test Integration:**
   ```bash
   # Build with all executables
   stack build --flag ampersand:buildAll
   
   # Install and test locally
   stack install
   ~/.local/bin/ampersand --version
   ```

**Testing Strategy:**
- Unit tests: `stack test`
- Integration tests: Build and run against test cases in `testing/`
- Regression tests: Ensure existing functionality still works
- Documentation: If adding features, update relevant documentation

### 3. Pre-Merge Validation

**Local Quality Checks:**
```bash
# Format code
stack exec -- ormolu --mode inplace $(find . -name '*.hs')

# Build clean
stack clean && stack build --flag ampersand:buildAll

# Run full test suite
stack test

# Check for warnings
stack build --pedantic
```

**Docker Testing (Optional but Recommended):**
```bash
# Build Docker image locally
docker build . --tag ampersand-local:test

# Test Docker functionality
docker run -v ${PWD}:/scripts ampersand-local:test --help
```

### 4. Submission Process

**Create Pull Request:**
1. Push feature branch to fork/origin
2. Create PR against main branch
3. Include description of changes
4. Reference any related issues

**PR Requirements:**
- [ ] All CI checks pass (Linux, macOS, Windows, Docker)
- [ ] Code review approval
- [ ] Up-to-date with main branch
- [ ] No merge conflicts

**Post-Merge:**
- Main branch automatically builds and pushes `:latest` Docker image
- Changes become available in next release
- Monitor CI/CD pipeline for any build failures

## Production Deployment & Distribution

### 1. Distribution Channels

**Primary Distribution Methods:**
- **Docker Hub:** `ampersandtarski/ampersand` (most common for deployment)
- **GitHub Releases:** Platform-specific binary downloads
- **Source Build:** Direct from repository using Stack

**End-User Installation:**

**Docker (Recommended):**
```bash
# Pull latest version
docker pull ampersandtarski/ampersand:latest

# Use in project directory
docker run -v ${PWD}:/scripts ampersandtarski/ampersand:latest check MyProject.adl
```

**Binary Download:**
1. Go to https://github.com/AmpersandTarski/Ampersand/releases
2. Download platform-specific archive
3. Extract binary to PATH location
4. Install dependencies (GraphViz, MariaDB for prototypes)

**Source Installation:**
```bash
git clone https://github.com/AmpersandTarski/Ampersand.git
cd Ampersand
stack install --flag ampersand:buildAll
```

### 2. Versioning & Backward Compatibility

**Version Policy:**
- **Major versions:** Breaking changes (v4.0.0 was major CLI restructure)
- **Minor versions:** New features, backward compatible
- **Patch versions:** Bug fixes only

**Breaking Change Management:**
- Document breaking changes in `ReleaseNotes.md`
- Maintain compatibility shims when possible
- Update `commands.md` for CLI changes
- Coordinate with dependent projects (prototype framework)

**Current Version Status:**
- Version 5.5.3 (stable)
- Major architecture stable since v4.0.0 CLI restructure
- Active development on main branch

## Troubleshooting Build & Deploy Issues

### 1. Common Build Problems

**Stack Memory Issues:**
```bash
# If build runs out of memory
# Edit stack.yaml: jobs: 2  (reduce from 4)
stack clean && stack build
```

**Missing System Dependencies:**
```bash
# Ubuntu/Debian
sudo apt-get install graphviz mysql-server php-cli

# macOS  
brew install graphviz mysql php

# Windows
# Use DevContainer or install via package managers
```

**Generated File Issues:**
```bash
# If BuildInfo_Generated.hs has issues
rm src/Ampersand/Basics/BuildInfo_Generated.hs
rm src/Ampersand/Prototype/StaticFiles_Generated.hs
stack clean && stack build
```

### 2. CI/CD Pipeline Issues

**Common CI Failures:**
- **macOS Tests Disabled:** Known MariaDB issue (MDEV-35173)
- **Windows Path Issues:** Use PowerShell-compatible paths
- **Docker Build Failures:** Check base image availability
- **Cache Issues:** May need manual cache invalidation

**Debugging CI Issues:**
1. Check specific platform build logs
2. Compare working vs failing builds
3. Test locally with same platform/stack version
4. Check for upstream dependency changes

### 3. Docker Deployment Issues

**Common Docker Problems:**
```bash
# Test local Docker build
docker build . --tag ampersand-test:local

# Check if binary works in container
docker run ampersand-test:local --help

# Volume mounting issues (Windows)
docker run -v ${PWD}:/scripts ampersand-test:local
# vs
docker run -v "%cd%":/scripts ampersand-test:local
```

## Critical Path Summary for Contributors

### 1. Development Cycle
```
Code Changes → Local Build (stack build) → Local Test (stack test) → 
Git Commit → Push to Feature Branch → Create PR → 
CI Validation (all platforms) → Code Review → Merge to Main →
Automatic Docker Build (:latest) → Available for Testing
```

### 2. Release Cycle  
```
Version Bump (package.yaml) → Tag Release (GitHub) → 
Release Workflow Triggers → Multi-platform Binary Build → 
Docker Images with Version Tags → Attach Binaries to Release → 
DockerHub Push → Release Available to End Users
```

### 3. Critical Dependencies

**External Dependencies:**
- Haskell Stack ecosystem stability
- DockerHub availability  
- GitHub Actions runner availability
- MariaDB compatibility across platforms

**Internal Dependencies:**
- Setup.hs code generation must complete successfully
- Static file embedding must work (templates, ADL files)
- Cross-platform binary compatibility
- Integration with prototype framework (depends on Docker image structure)

**Monitoring Points:**
- CI build status across all platforms
- DockerHub image size and build time
- Release artifact completeness
- Version information correctness in built binaries

## Emergency Procedures

### 1. Broken Main Branch
```bash
# If main branch build is broken:
1. Identify breaking commit
2. Create hotfix branch from last working commit  
3. Apply minimal fix
4. Emergency PR with expedited review
5. Manual Docker build/push if needed
```

### 2. Failed Release
```bash
# If release build fails:
1. Check GitHub Actions logs for all platforms
2. Fix identified issues on main branch
3. Delete failed release tag and GitHub release
4. Re-tag and re-release
5. Verify all artifacts are correctly attached
```

### 3. DockerHub Issues
```bash
# If DockerHub push fails:
1. Check DockerHub service status
2. Verify credentials (GitHub secrets)
3. Manual build and push if needed:
   docker build . --tag ampersandtarski/ampersand:manual-fix
   docker push ampersandtarski/ampersand:manual-fix
```

This comprehensive guide covers the complete build, test, and deployment pipeline for the Ampersand compiler, providing contributors with all necessary information to understand and work with the project's infrastructure.
