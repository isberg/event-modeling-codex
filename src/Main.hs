module Main where

import Domain.Task
import Infrastructure.EventStore

-- | Simple demo that runs a few commands using the in-memory event store

main :: IO ()
main = do
  store <- inMemoryEventStore
  events0 <- loadEvents store
  let state0 = foldl applyEvent [] events0
      newEvents = decide state0 (CreateTask (TaskId 1) "First task")
      state1 = foldl applyEvent state0 newEvents
  appendEvents store newEvents
  putStrLn "Current tasks:" 
  print state1
