{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fly.Interpret where

import Data.Aeson      (Value)
import Data.Text       (Text)
import Dhall
import Dhall.Core      (Chunks (..), Expr (..), Var(..))
import Dhall.Map       (Map, fromList, lookup)
import Dhall.Parser    (Src)
import Dhall.TypeCheck (X)
import Fly.Types

import qualified Dhall.Core
import qualified Dhall.JSON

textPairInDhall = Record (fromList [("mapKey", Text), ("mapValue", Text)])

assocListInDhall = App List textPairInDhall

optionalAssocListInDhall = App Optional assocListInDhall

resourceTypeInDhall =
  Union (fromList [ ("Custom", Record (fromList [ ("name", Text)
                                                , ("source", optionalAssocListInDhall)
                                                , ("type", Text)]))
                  , ("InBuilt", Text)])
resourceInDhall =
  Record (fromList [ ("check_every", App Optional Text)
                   , ("name", Text)
                   , ("params", optionalAssocListInDhall)
                   , ("source", optionalAssocListInDhall)
                   , ("tags", App Optional (App List Text))
                   , ("type", resourceTypeInDhall)
                   , ("version", optionalAssocListInDhall)
                   , ("webhook_token", App Optional Text)])

taskRunConfigInDhall = Record (fromList [ ("args", App Optional (App List Text))
                                        , ("dir", App Optional Text)
                                        , ("path", Text)
                                        , ("user", App Optional Text)])

taskImageResourceInDhall = Record (fromList [ ("params", optionalAssocListInDhall)
                                            , ("source", assocListInDhall)
                                            , ("type", Text)
                                            , ("version", optionalAssocListInDhall)])

taskInputInDhall = Record (fromList [ ("name", Text)
                                    , ("path", App Optional Text)
                                    , ("optional", App Optional Bool)])


taskOutputInDhall = Record (fromList [ ("name", Text)
                                     , ("path", App Optional Text)
                                     ])

taskCacheInDhall = Record (fromList [("path", Text)])

taskConfigInDhall =
  Record (fromList [ ("platform", Text)
                   , ("run", taskRunConfigInDhall)
                   , ("image_resource", App Optional taskImageResourceInDhall)
                   , ("rootfs_uri", App Optional Text)
                   , ("inputs", App Optional (App List taskInputInDhall))
                   , ("outputs", App Optional (App List taskOutputInDhall))
                   , ("caches", App Optional (App List taskCacheInDhall))
                   , ("params", optionalAssocListInDhall)
                   ])
taskSpecInDhall = Union (fromList [ ("Config", taskConfigInDhall)
                                  , ("File", Text)])

getVersionInDhall = Union (fromList [ ("Latest", Text)
                                    , ("Every", Text)
                                    , ("SpecificVersion", assocListInDhall)])

getStepInDhall = Record (fromList [ ("get", App Optional Text)
                                  , ("resource", resourceInDhall)
                                  , ("params", optionalAssocListInDhall)
                                  , ("version", App Optional getVersionInDhall)
                                  , ("passed", App Optional (App List Text))
                                  , ("trigger", App Optional Bool)
                                  ])

putStepInDhall = Record (fromList [ ("put", App Optional Text)
                                  , ("resource", resourceInDhall)
                                  , ("params", optionalAssocListInDhall)
                                  , ("get_params", optionalAssocListInDhall)
                                  ])

taskStepInDhall = Record (fromList [ ("task", Text)
                                   , ("config", taskSpecInDhall)
                                   , ("privileged", App Optional Bool)
                                   , ("params", optionalAssocListInDhall)
                                   , ("image", App Optional Text)
                                   , ("input_mapping", optionalAssocListInDhall)
                                   , ("output_mapping", optionalAssocListInDhall)
                                   ])

stepHooksInDhall :: Expr Src X -> Expr Src X
stepHooksInDhall step = Record (fromList [ ("on_success", App Optional step)
                                        , ("on_failure", App Optional step)
                                        , ("on_abort", App Optional step)
                                        , ("ensure", App Optional step)
                                        ])


stepInDhall = Pi "Step" (Const Dhall.Core.Type)
              (Pi "GetStep" (Pi "_" getStepInDhall (Pi "_" hooks step))
               (Pi "PutStep" (Pi "_" putStepInDhall (Pi "_" hooks step))
                (Pi "TaskStep" (Pi "_" taskStepInDhall (Pi "_" hooks step))
                 (Pi "AggregateStep" (Pi "_" (App List step) (Pi "_" hooks step))
                  (Pi "DoStep" (Pi "_" (App List step) (Pi "_" hooks step))
                   (Pi "TryStep" (Pi "_" step (Pi "_" hooks step)) step))))))
  where step = (Var (V "Step" 0))
        hooks = (stepHooksInDhall step)

jobInDhall = Record (fromList [ ("name", Text)
                              , ("plan", App List stepInDhall)
                              , ("serial", App Optional Bool)
                              , ("build_logs_to_retain", App Optional Natural)
                              , ("serial_groups", App Optional (App List Text))
                              , ("max_in_flight", App Optional Natural)
                              , ("public", App Optional Bool)
                              , ("disable_manual_trigger", App Optional Bool)
                              , ("interruptible", App Optional Bool)
                              , ("on_success", App Optional stepInDhall)
                              , ("on_failure", App Optional stepInDhall)
                              , ("on_abort", App Optional stepInDhall)
                              , ("ensure", App Optional stepInDhall)
                              ])

assocList :: Type Value
assocList = Type{..} where
  expected = assocListInDhall
  extract l@(ListLit _ _) = case Dhall.JSON.dhallToJSON l of
                              Left _  -> Nothing
                              Right v -> pure v
  extract _ = Nothing

textPair :: Type (Text, Text)
textPair = Type{..} where
  expected = textPairInDhall
  extract (RecordLit m) = (,)
                       <$> extractFromMap "mapKey" strictText
                       <*> extractFromMap "mapValue" strictText
                       where extractFromMap = interpretWithType m
  extract _ = Nothing
extractCustomResourceType m = ResourceTypeCustom
    <$> extractFromMap "name" strictText
    <*> extractFromMap "type" strictText
    <*> extractFromMap "source" (Dhall.maybe assocList)
    where extractFromMap = interpretWithType m

interpretResourceType :: Type ResourceType
interpretResourceType = Type{..} where
  expected = resourceTypeInDhall
  extract (UnionLit "Custom" (RecordLit m) _) = extractCustomResourceType m
  extract (UnionLit "InBuilt" (TextLit (Chunks [] t)) _) = pure $ ResourceTypeInBuilt t
  extract _ = Nothing

interpretResource :: Type Resource
interpretResource = Type{..} where
  expected = resourceInDhall
  extract (RecordLit m) =
    Resource
    <$> extractFromMap "name"          strictText
    <*> extractFromMap "type"          interpretResourceType
    <*> extractFromMap "source"        (Dhall.maybe assocList)
    <*> extractFromMap "version"       (Dhall.maybe assocList)
    <*> extractFromMap "params"        (Dhall.maybe assocList)
    <*> extractFromMap "check_every"   (Dhall.maybe strictText)
    <*> extractFromMap "tags"          (Dhall.maybe $ list strictText)
    <*> extractFromMap "webhook_token" (Dhall.maybe strictText)
    where extractFromMap = interpretWithType m
  extract _ = Nothing

interpretTaskRunConfig :: Type TaskRunConfig
interpretTaskRunConfig = Type{..} where
  expected = taskRunConfigInDhall
  extract (RecordLit m) =
    TaskRunConfig
    <$> extractFromMap "path" strictText
    <*> extractFromMap "args" (Dhall.maybe $ list strictText)
    <*> extractFromMap "dir"  (Dhall.maybe strictText)
    <*> extractFromMap "user" (Dhall.maybe strictText)
    where extractFromMap = interpretWithType m
  extract _ = Nothing

interpretTaskImageResource :: Type TaskImageResource
interpretTaskImageResource = Type{..} where
  expected = taskImageResourceInDhall
  extract (RecordLit m) =
    TaskImageResource
    <$> extractFromMap "type" strictText
    <*> extractFromMap "source" assocList
    <*> extractFromMap "params"  (Dhall.maybe assocList)
    <*> extractFromMap "version" (Dhall.maybe assocList)
    where extractFromMap = interpretWithType m
  extract _ = Nothing

interpretTaskInput :: Type TaskInput
interpretTaskInput = Type{..} where
  expected = taskInputInDhall
  extract (RecordLit m) =
    TaskInput
    <$> extractFromMap "name" strictText
    <*> extractFromMap "path" (Dhall.maybe strictText)
    <*> extractFromMap "optional"  (Dhall.maybe bool)
    where extractFromMap = interpretWithType m
  extract _ = Nothing

interpretTaskOutput :: Type TaskOutput
interpretTaskOutput = Type{..} where
  expected = taskOutputInDhall
  extract (RecordLit m) =
    TaskOutput
    <$> extractFromMap "name" strictText
    <*> extractFromMap "path" (Dhall.maybe strictText)
    where extractFromMap = interpretWithType m
  extract _ = Nothing

interpretTaskCache :: Type TaskCache
interpretTaskCache = Type{..} where
  expected = taskCacheInDhall
  extract (RecordLit m) =
    TaskCache
    <$> extractFromMap "path" strictText
    where extractFromMap = interpretWithType m
  extract _ = Nothing

interpretTaskConfig :: Type TaskConfig
interpretTaskConfig = Type{..} where
  expected = taskConfigInDhall
  extract (RecordLit m) =
    TaskConfig
    <$> extractFromMap "platform" strictText
    <*> extractFromMap "run" interpretTaskRunConfig
    <*> extractFromMap "image_resource" (Dhall.maybe interpretTaskImageResource)
    <*> extractFromMap "rootfs_uri" (Dhall.maybe strictText)
    <*> extractFromMap "inputs" (Dhall.maybe $ list interpretTaskInput)
    <*> extractFromMap "outputs" (Dhall.maybe $ list interpretTaskOutput)
    <*> extractFromMap "caches" (Dhall.maybe $ list interpretTaskCache)
    <*> extractFromMap "params" (Dhall.maybe $ list textPair)
    where extractFromMap = interpretWithType m
  extract _ = Nothing

interpretTaskSpec :: Type TaskSpec
interpretTaskSpec = Type{..} where
  expected = taskSpecInDhall
  extract (UnionLit "Config" c _) = TaskSpecConfig <$> Dhall.extract interpretTaskConfig c
  extract (UnionLit "File" (TextLit (Chunks [] t)) _) = pure $ TaskSpecFile t
  extract _ = Nothing

interpretStep :: Type Step
interpretStep = Type{..} where
  expected = stepInDhall
  extract (Lam _ _ -- Step
           (Lam _ _ -- GetStep
            (Lam _ _ -- PutStep
             (Lam _ _ -- TaskStep
              (Lam _ _ -- AggregateStep
               (Lam _ _ -- DoStep
                (Lam _ _ -- TryStep
                 x))))))) = extractStepFromApps x
  extract x = extractStepFromApps x -- While recursive it loses all the `Lam`s

interpretGetVersion :: Type GetVersion
interpretGetVersion = Type{..} where
  expected = getVersionInDhall
  extract (UnionLit "Latest" (TextLit _) _) = pure GetVersionLatest
  extract (UnionLit "Every" (TextLit _) _) = pure GetVersionEvery
  extract (UnionLit "SpecificVersion" l@(ListLit _ _) _) =
    case Dhall.JSON.dhallToJSON l of
        Left _  -> Nothing
        Right v -> pure $ GetVersionSpecific v

interpretGetStep :: Type GetStep
interpretGetStep = Type{..} where
  expected = getStepInDhall
  extract (RecordLit m) =
    GetStep
    <$> extractFromMap "get" (Dhall.maybe strictText)
    <*> extractFromMap "resource" interpretResource
    <*> extractFromMap "params" (Dhall.maybe assocList)
    <*> extractFromMap "version" (Dhall.maybe interpretGetVersion)
    <*> extractFromMap "passed" (Dhall.maybe $ list strictText)
    <*> extractFromMap "trigger" (Dhall.maybe bool)
    where extractFromMap = interpretWithType m

interpretPutStep :: Type PutStep
interpretPutStep = Type{..} where
  expected = putStepInDhall
  extract (RecordLit m) =
    PutStep
    <$> extractFromMap "put" (Dhall.maybe strictText)
    <*> extractFromMap "resource" interpretResource
    <*> extractFromMap "params" (Dhall.maybe assocList)
    <*> extractFromMap "get_params" (Dhall.maybe assocList)
    where extractFromMap = interpretWithType m

interpretTaskStep :: Type TaskStep
interpretTaskStep = Type{..} where
  expected = taskStepInDhall
  extract (RecordLit m) =
    TaskStep
    <$> extractFromMap "task" strictText
    <*> extractFromMap "config" interpretTaskSpec
    <*> extractFromMap "privileged" (Dhall.maybe bool)
    <*> extractFromMap "params" (Dhall.maybe $ list textPair)
    <*> extractFromMap "image" (Dhall.maybe strictText)
    <*> extractFromMap "input_mapping" (Dhall.maybe $ list textPair)
    <*> extractFromMap "output_mapping" (Dhall.maybe $ list textPair)
    where extractFromMap = interpretWithType m

interpretStepHooks :: Expr Src X -> Type StepHooks
interpretStepHooks step = Type{..} where
  expected = stepHooksInDhall step
  extract (RecordLit m) =
    StepHooks
    <$> extractFromMap "on_success" (Dhall.maybe interpretStep)
    <*> extractFromMap "on_failure" (Dhall.maybe interpretStep)
    <*> extractFromMap "on_abort" (Dhall.maybe interpretStep)
    <*> extractFromMap "ensure" (Dhall.maybe interpretStep)
    where extractFromMap = interpretWithType m
  extract _ = Nothing

extractStepFromApps :: Expr Src X -> Maybe Step
extractStepFromApps (App (App (Var (V "_" n)) s) hooks) =
  stepFn <*> Dhall.extract (interpretStepHooks (Var (V "_" 6))) hooks
  where stepFn = case n of
                   5 -> Get <$> Dhall.extract interpretGetStep s
                   4 -> Put <$> Dhall.extract interpretPutStep s
                   3 -> Task <$> Dhall.extract interpretTaskStep s
                   2 -> Aggregate <$> Dhall.extract (list interpretStep) s
                   1 -> Do <$> Dhall.extract (list interpretStep) s
                   0 -> Try <$> Dhall.extract interpretStep s
                   _ -> Nothing
extractStepFromApps _ = Nothing

interpretJob :: Type Job
interpretJob = Type{..} where
  expected = jobInDhall
  extract (RecordLit m) =
    Job
    <$> extractFromMap "name" strictText
    <*> extractFromMap "plan" (list interpretStep)
    <*> extractFromMap "serial" (Dhall.maybe bool)
    <*> extractFromMap "build_logs_to_retain" (Dhall.maybe natural)
    <*> extractFromMap "serial_groups" (Dhall.maybe $ list strictText)
    <*> extractFromMap "max_in_flight" (Dhall.maybe natural)
    <*> extractFromMap "public" (Dhall.maybe bool)
    <*> extractFromMap "disable_manual_trigger" (Dhall.maybe bool)
    <*> extractFromMap "interruptible" (Dhall.maybe bool)
    <*> extractFromMap "on_success" (Dhall.maybe interpretStep)
    <*> extractFromMap "on_failure" (Dhall.maybe interpretStep)
    <*> extractFromMap "on_abort" (Dhall.maybe interpretStep)
    <*> extractFromMap "ensure" (Dhall.maybe interpretStep)
    where extractFromMap = interpretWithType m

  extract _ = Nothing

interpretWithType :: Map Text (Expr Src X) -> Text -> Type a -> Maybe a
interpretWithType m key t = Dhall.extract t =<< Dhall.Map.lookup key m
