# Instructions for AI development agents

These instructions apply to all AI development agents working in this repository,
including Claude, ChatGPT, Codex, and any delegated agents.

## Read before working

Read and follow [CLAUDE.md](CLAUDE.md) before planning or changing this repository.
It is the single source of truth for repository-specific development instructions.
Read the relevant architecture documentation, memorybank notes, and recorded
design choices referenced there before working on the affected area.

References to "Claude" as an AI development partner apply to every AI development
agent. Concrete Claude-specific tools and configuration remain tool-specific.

## Working agreement

Follow the development discipline illustrated by
[Ampersand issue #1683](https://github.com/AmpersandTarski/Ampersand/issues/1683):

- Respect recorded design decisions. Discuss proposed departures with the user
  before implementing them.
- Support correctness claims with appropriate proofs and tests, including the
  correspondence between a formal model and the actual implementation.
- Run the required verification and report observed results. Explicitly state
  which checks were not run, why, and which correctness obligations remain open.
- Keep documentation, design decisions, and proof status current in the same change.
- Work on a branch and keep changes within the agreed scope.

The concrete files, branches, commands, and test models in #1683 belong to that
specific task; apply them only where relevant. Follow the current repository
instructions for each new task.

Pass these instructions to any delegated agents. If required instructions cannot
be read or a requirement cannot be met, report that explicitly.
