{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fly.Types where

import Data.Aeson      (Value)
import Data.Text       (Text)
import Dhall
import Dhall.Core      (Chunks (..), Expr (..))
import Dhall.Map       (Map, fromList, lookup)
import Dhall.Parser    (Src)
import Dhall.TypeCheck (X)

import qualified Dhall.Core
import qualified Dhall.JSON

data ResourceType = ResourceTypeInBuilt Text
                  | ResourceTypeCustom { rtcName :: Text
                                       , rtcType :: Text
                                       , source  :: Maybe Value
                                       }
                    deriving (Show, Generic)

data Resource = Resource { resourceName         :: Text
                         , resourceType         :: ResourceType
                         , resourceSource       :: Maybe Value
                         , resourceVersion      :: Maybe Value
                         , resourceParams       :: Maybe Value
                         , resourceCheckEvery   :: Maybe Text
                         , resourceTags         :: Maybe [Text]
                         , resourceWebhookToken :: Maybe Text}
              deriving (Show, Generic)

data TaskRunConfig = TaskRunConfig { trcPath :: Text
                                   , trcArgs :: Maybe [Text]
                                   , trcDir  :: Maybe Text
                                   , trcUser :: Maybe Text
                                   }
                   deriving (Show, Generic)

data TaskImageResource = TaskImageResource { tirType    :: Text
                                           , tirSource  :: Value
                                           , tirParams  :: Maybe Value
                                           , tirVersion :: Maybe Value
                                           }
                       deriving (Show, Generic)

data TaskInput = TaskInput { tiName     :: Text
                           , tiPath     :: Maybe Text
                           , tiOptional :: Maybe Bool
                           }
               deriving (Show, Generic)

data TaskOutput = TaskOutput { toName :: Text, toPath :: Maybe Text }
                deriving (Show, Generic)

data TaskCache = TaskCache { taskCachePath :: Text}
               deriving (Show, Generic)

data TaskConfig = TaskConfig { tcPlatform      :: Text
                             , tcRun           :: TaskRunConfig
                             , tcImageResource :: Maybe TaskImageResource
                             , tcRootfsURI     :: Maybe Text
                             , tcInputs        :: Maybe [TaskInput]
                             , tcOutputs       :: Maybe [TaskOutput]
                             , tcCaches        :: Maybe [TaskCache]
                             , tcParams        :: Maybe [(Text, Text)]
                             }
                deriving (Show, Generic)

data TaskSpec = TaskSpecFile Text | TaskSpecConfig TaskConfig
              deriving (Show, Generic)

data StepHooks = StepHooks { hookOnSuccess :: Maybe Step
                           , hookOnFailure :: Maybe Step
                           , hookOnAbort   :: Maybe Step
                           , hookEnsure    :: Maybe Step
                           }
               deriving (Show, Generic)

data GetVersion = GetVersionLatest
                | GetVersionEvery
                | GetVersionSpecific Value
                deriving (Show, Generic)

data GetStep = GetStep { getName     :: Maybe Text
                       , getResource :: Resource
                       , getParams   :: Maybe Value
                       , getVersion  :: Maybe GetVersion
                       , getPassed   :: Maybe [Text]
                       , getTrigger  :: Maybe Bool
                       }
             deriving (Show, Generic)

data PutStep = PutStep { putName      :: Maybe Text
                       , putResource  :: Resource
                       , putParams    :: Maybe Value
                       , putGetParams :: Maybe Value
                       }
             deriving (Show, Generic)
data TaskStep = TaskStep { taskName       :: Text
                         , taskSpec       :: TaskSpec
                         , taskPrivileged :: Maybe Bool
                         , taskParams     :: Maybe [(Text, Text)]
                         , image          :: Maybe Text
                         , input_mapping  :: Maybe [(Text, Text)]
                         , output_mapping :: Maybe [(Text, Text)]
                         }
              deriving (Show, Generic)

data Step = Get { stepGet :: GetStep, stepHooks :: StepHooks }
          | Put { stepPut :: PutStep, stepHooks :: StepHooks }
          | Task { stepTask :: TaskStep, stepHooks :: StepHooks }
          | Aggregate { aggregatedSteps :: [Step], stepHooks :: StepHooks }
          | Do { doSteps :: [Step], stepHooks :: StepHooks  }
          | Try { tryStep :: Step, stepHooks :: StepHooks  }
          deriving (Show, Generic)

data Job = Job { jobName                 :: Text
               , jobPlan                 :: [Step]
               , jobSerial               :: Maybe Bool
               , jobBuildLogsToRetain    :: Maybe Natural
               , jobSerialGroups         :: Maybe [Text]
               , jobMaxInFlight          :: Maybe Natural
               , jobPublic               :: Maybe Bool
               , jobDisableManualTrigger :: Maybe Bool
               , jobInterruptible        :: Maybe Bool
               , jobOnSuccess            :: Maybe Step
               , jobOnFailure            :: Maybe Step
               , jobOnAbort              :: Maybe Step
               , jobEnsure               :: Maybe Step
               }
         deriving (Show, Generic)

