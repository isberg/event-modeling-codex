module Infrastructure.EventStore where

import Data.IORef

-- | A very small event store interface

data EventStore m event = EventStore
  { loadEvents   :: m [event]
  , appendEvents :: [event] -> m ()
  }

-- | In-memory implementation using an IORef

inMemoryEventStore :: IO (EventStore IO event)

inMemoryEventStore = do
  ref <- newIORef []
  pure EventStore
    { loadEvents = readIORef ref
    , appendEvents = \evts -> modifyIORef' ref (++ evts)
    }
