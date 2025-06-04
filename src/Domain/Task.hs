module Domain.Task where

-- | Identifier for tasks
newtype TaskId = TaskId Int deriving (Show, Eq)

-- | A task has an id, a title, and a completed flag
data Task = Task
  { taskId    :: TaskId
  , title     :: String
  , completed :: Bool
  } deriving (Show, Eq)

-- | Commands that can be issued to the system
data Command
  = CreateTask TaskId String
  | CompleteTask TaskId
  | ListTasks
  deriving (Show, Eq)

-- | Events produced by handling commands
data Event
  = TaskCreated TaskId String
  | TaskCompleted TaskId
  deriving (Show, Eq)

-- | The state is just a list of tasks for now

type TaskList = [Task]

-- | Apply an event to the current state
applyEvent :: TaskList -> Event -> TaskList
applyEvent tasks (TaskCreated tid ttl) =
  tasks ++ [Task tid ttl False]
applyEvent tasks (TaskCompleted tid) =
  map mark tasks
  where
    mark t | taskId t == tid = t { completed = True }
            | otherwise      = t

-- | Decide which events should be emitted for a given command and state

decide :: TaskList -> Command -> [Event]
decide tasks (CreateTask tid ttl) =
  if any ((== tid) . taskId) tasks
    then [] -- task already exists, no events
    else [TaskCreated tid ttl]

decide tasks (CompleteTask tid) =
  if any (
         \t -> taskId t == tid && not (completed t)) tasks
    then [TaskCompleted tid]
    else []

decide _ ListTasks = []
