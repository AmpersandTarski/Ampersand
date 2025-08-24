# Enhanced Release Automation - Align with Ampersand Repository Release Policy

## Problem
The Prototype Framework repository has an automated release process that only handles Docker image publishing and Git tag pushes.
This differs from the Ampersand repository's release automation, which includes GitHub releases with downloadable assets, manual release control, and structured artifact management.

## Requirements
I want to align the Prototype Framework repository release process to create uniformity and the same level of professionalism as the Ampersand repo has.

**R1. Manual Release Control**: The system must require intentional GitHub release creation to trigger automation, preventing accidental releases and ensuring proper release management.

**R2. Comprehensive Asset Distribution**: The system must provide both Docker images and downloadable archives, to also serve users who don't use Docker containerization.

**R3. Consistent Versioning Strategy**: The system must use semantic versioning extracted from Git tags to maintain alignment with industry standards and the main Ampersand repository.

**R4. Professional Release Process**: The system must follow enterprise-grade release practices that match the maturity level of the Ampersand repository.

**R5. Backward Compatibility**: The system must preserve existing Docker publishing workflows for development branches to avoid disrupting current processes.

## Design Choices
The solution addresses these requirements by solving the core problem of release process inconsistency between repositories while maintaining professional standards and user accessibility:
* Manual GitHub release creation triggers automation to satisfy R1 (manual control) and R4 (professional process).
* Build both compressed archive formats alongside Docker images to satisfy R2 (comprehensive distribution).
* Extract version information from git tags to satisfy R3 (uniform versioning) and align with GitHub release practices.
* Separate release automation from development branch publishing to satisfy R5 (backward compatibility) and improve maintainability.
* Create optimized archives with production dependencies and exclude development files to ensure distribution quality.

## Solution
**New Release Workflow (`release.yml`)**: The workflow triggers on manual release creation (`release: types: [created]`), mirroring the main repository's approach. It employs a multi-job architecture with separate jobs for Docker publishing, artifact building, and release notes management. Version information is extracted using git tags (`${GITHUB_REF#refs/tags/v}`), ensuring adherence to versioning standards.

**Asset Building and Packaging**: The solution creates production-ready framework archives in multiple compressed formats to accommodate different user preferences and deployment scenarios. The build process installs composer dependencies using production flags to create lean, optimized packages. These clean archives exclude development files such as tests, documentation, development containers, and other non-essential files. The generated packages are automatically uploaded and attached to releases as downloadable assets, providing users with immediate access to framework distributions.

**Release Notes Integration**: The workflow automatically attaches the existing `changelog.md` file to every release, leveraging the already well-maintained changelog structure. This ensures that release notes are reliably available with each release without requiring additional manual effort from maintainers.

**Docker Publishing Strategy**: Container image publishing for releases is now handled within the release workflow, ensuring that release-triggered builds use the same tagging pattern as the main repository (`v{{version}}`, `v{{major}}.{{minor}}`, `v{{major}}`). The build process includes git metadata such as SHA and branch information as build arguments, providing better traceability and debugging capabilities.

**Modified Existing Workflows**: The existing `build-push-to-docker-hub.yml` workflow has been updated to focus solely on `main` branch builds, with tag triggers removed to prevent duplicate Docker builds. This separation of concerns improves maintainability and prevents conflicts between development and release processes. Clear documentation has been added to explain the workflow separation and guide future maintenance efforts.

## Alternatives

I did not consider alternatives.

## Additional context

### Running the Pipeline

To use the new release workflow:
1. Create a git tag (e.g., `v2.1.0`)
2. Push the tag to the repository  
3. Create a GitHub release using that tag
4. The workflow will automatically build and attach assets

## Files Modified/Created

- **Created**: `.github/workflows/release.yml` - New comprehensive release workflow
- **Modified**: `.github/workflows/build-push-to-docker-hub.yml` - Removed tag triggers, updated documentation

### Impact
This enhancement ensures uniform release management across both repositories.
---

**Labels**: `enhancement`, `ci/cd`, `release-automation`
**Assignees**: <!-- Add appropriate maintainers -->
