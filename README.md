# event-modeling-codex

This repository hosts a small demo that showcases **functional programming** and
**Domain Modeling Made Functional** using **Event Modeling** and **Event
Sourcing**.

## Technologies

- **Front end:** Elm. Fable with Elmish may be added later for a different take
  on the Elm Architecture.
- **Back end:** Initial implementation in Haskell. Future back ends in F# or
  Idris2 are possible.
- **Event store options:**
  - In-memory store for tests and quick prototypes.
  - Plain file-based store for lightweight persistence.
  - EventStoreDB when a production style database is desired.

## Minimal feature set

The first iteration focuses on a simple **task manager** with three commands and
corresponding events:

1. `CreateTask` – produces `TaskCreated`.
2. `CompleteTask` – produces `TaskCompleted`.
3. `ListTasks` – derived from projecting past events.

Additional events (like `TaskUpdated` or `TaskDeleted`) can be introduced
incrementally as the timeline grows.
## Event Modeling approach

Event Modeling is central to this project. See [Event Modeling Cheatsheet](https://eventmodeling.org/posts/event-modeling-cheatsheet/) for a concise overview.
The demo follows four core patterns: **Command Pattern**, **View Pattern**, **Automation Pattern**, and **Translation Pattern**.


## Running the demo

1. **Local:** run the Haskell server with `stack run` and start the Elm front
   end with `elm reactor` or your preferred build tool.
2. **Docker:** containers will be provided for both front end and back end.
3. **Nix:** optional; a flake may be added later to reproduce the environment.

## Testing

Property-based testing is encouraged:

- **QuickCheck** for Haskell implementations.
- **FsCheck** if an F# back end is added.

Run `scripts/test.sh` to execute the current test suite (placeholder for now).
