{-# OPTIONS_GHC -Wno-orphans #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Domain.Task

instance Arbitrary TaskId where
  arbitrary = TaskId <$> arbitrary

instance Arbitrary Task where
  arbitrary = Task <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "applyEvent" $ do
    prop "appends TaskCreated" prop_applyEvent_create
    prop "marks task completed" prop_applyEvent_complete
  describe "decide" $ do
    prop "creates new task when id missing" prop_decide_create_missing
    prop "no-op when id exists" prop_decide_create_existing
    prop "completes uncompleted tasks" prop_decide_complete_uncompleted
    prop "no-op if task absent or already done" prop_decide_complete_absent_or_done

prop_applyEvent_create :: TaskList -> TaskId -> String -> Bool
prop_applyEvent_create tasks tid ttl =
  applyEvent tasks (TaskCreated tid ttl) == tasks ++ [Task tid ttl False]

prop_applyEvent_complete :: TaskList -> TaskId -> Bool
prop_applyEvent_complete tasks tid =
  applyEvent tasks (TaskCompleted tid) == map mark tasks
  where
    mark t | taskId t == tid = t { completed = True }
           | otherwise      = t

prop_decide_create_missing :: TaskList -> TaskId -> String -> Property
prop_decide_create_missing tasks tid ttl =
  notElem tid (map taskId tasks) ==>
    decide tasks (CreateTask tid ttl) == [TaskCreated tid ttl]

prop_decide_create_existing :: NonEmptyList Task -> String -> Bool
prop_decide_create_existing (NonEmpty tasks) ttl =
  let tid = taskId (head tasks)
  in decide tasks (CreateTask tid ttl) == []

prop_decide_complete_uncompleted :: TaskList -> Property
prop_decide_complete_uncompleted tasks =
  case filter (not . completed) tasks of
    [] -> property True
    (t:_) -> decide tasks (CompleteTask (taskId t)) === [TaskCompleted (taskId t)]

prop_decide_complete_absent_or_done :: TaskList -> TaskId -> Property
prop_decide_complete_absent_or_done tasks tid =
  all (\t -> taskId t /= tid || completed t) tasks ==>
    decide tasks (CompleteTask tid) == []
