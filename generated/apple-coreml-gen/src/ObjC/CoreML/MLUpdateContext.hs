{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides context for the update process when the progress or completion handlers are invoked.
--
-- Generated bindings for @MLUpdateContext@.
module ObjC.CoreML.MLUpdateContext
  ( MLUpdateContext
  , IsMLUpdateContext(..)
  , task
  , model
  , event
  , metrics
  , parameters
  , eventSelector
  , metricsSelector
  , modelSelector
  , parametersSelector
  , taskSelector

  -- * Enum types
  , MLUpdateProgressEvent(MLUpdateProgressEvent)
  , pattern MLUpdateProgressEventTrainingBegin
  , pattern MLUpdateProgressEventEpochEnd
  , pattern MLUpdateProgressEventMiniBatchEnd

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- task@
task :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO (Id MLUpdateTask)
task mlUpdateContext =
  sendMessage mlUpdateContext taskSelector

-- | @- model@
model :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO (Id MLModel)
model mlUpdateContext =
  sendMessage mlUpdateContext modelSelector

-- | @- event@
event :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO MLUpdateProgressEvent
event mlUpdateContext =
  sendMessage mlUpdateContext eventSelector

-- | @- metrics@
metrics :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO (Id NSDictionary)
metrics mlUpdateContext =
  sendMessage mlUpdateContext metricsSelector

-- | @- parameters@
parameters :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO (Id NSDictionary)
parameters mlUpdateContext =
  sendMessage mlUpdateContext parametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @task@
taskSelector :: Selector '[] (Id MLUpdateTask)
taskSelector = mkSelector "task"

-- | @Selector@ for @model@
modelSelector :: Selector '[] (Id MLModel)
modelSelector = mkSelector "model"

-- | @Selector@ for @event@
eventSelector :: Selector '[] MLUpdateProgressEvent
eventSelector = mkSelector "event"

-- | @Selector@ for @metrics@
metricsSelector :: Selector '[] (Id NSDictionary)
metricsSelector = mkSelector "metrics"

-- | @Selector@ for @parameters@
parametersSelector :: Selector '[] (Id NSDictionary)
parametersSelector = mkSelector "parameters"

