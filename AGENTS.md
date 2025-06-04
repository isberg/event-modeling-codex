# Instructions for Codex

This repository demonstrates functional programming with a focus on Domain
Modeling Made Functional and Event Sourcing.

## Coding guidelines

1. Keep domain logic in pure functions. Infrastructure concerns (web server,
   event store, etc.) should be separated from business rules.
2. Provide interchangeable event store implementations: in-memory, file-based,
   and EventStoreDB.
3. Use Elm on the front end. The primary back end is Haskell, but alternative
   implementations in F# or Idris2 may appear.
4. Prefer property-based tests using QuickCheck (Haskell) or FsCheck (F#).
5. Before committing, run `scripts/test.sh` and ensure it succeeds.
6. Follow Event Modeling patterns: Command, View, Automation, and Translation. See https://eventmodeling.org/posts/event-modeling-cheatsheet/ for an overview.
