{-# LANGUAGE PatternSynonyms #-}
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
  , taskSelector
  , modelSelector
  , eventSelector
  , metricsSelector
  , parametersSelector

  -- * Enum types
  , MLUpdateProgressEvent(MLUpdateProgressEvent)
  , pattern MLUpdateProgressEventTrainingBegin
  , pattern MLUpdateProgressEventEpochEnd
  , pattern MLUpdateProgressEventMiniBatchEnd

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- task@
task :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO (Id MLUpdateTask)
task mlUpdateContext  =
  sendMsg mlUpdateContext (mkSelector "task") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- model@
model :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO (Id MLModel)
model mlUpdateContext  =
  sendMsg mlUpdateContext (mkSelector "model") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- event@
event :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO MLUpdateProgressEvent
event mlUpdateContext  =
  fmap (coerce :: CLong -> MLUpdateProgressEvent) $ sendMsg mlUpdateContext (mkSelector "event") retCLong []

-- | @- metrics@
metrics :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO (Id NSDictionary)
metrics mlUpdateContext  =
  sendMsg mlUpdateContext (mkSelector "metrics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parameters@
parameters :: IsMLUpdateContext mlUpdateContext => mlUpdateContext -> IO (Id NSDictionary)
parameters mlUpdateContext  =
  sendMsg mlUpdateContext (mkSelector "parameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @task@
taskSelector :: Selector
taskSelector = mkSelector "task"

-- | @Selector@ for @model@
modelSelector :: Selector
modelSelector = mkSelector "model"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

-- | @Selector@ for @metrics@
metricsSelector :: Selector
metricsSelector = mkSelector "metrics"

-- | @Selector@ for @parameters@
parametersSelector :: Selector
parametersSelector = mkSelector "parameters"

