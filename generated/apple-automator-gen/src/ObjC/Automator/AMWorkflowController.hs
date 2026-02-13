{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMWorkflowController@.
module ObjC.Automator.AMWorkflowController
  ( AMWorkflowController
  , IsAMWorkflowController(..)
  , run
  , stop
  , pause
  , step
  , reset
  , workflow
  , setWorkflow
  , workflowView
  , setWorkflowView
  , delegate
  , setDelegate
  , canRun
  , running
  , paused
  , canRunSelector
  , delegateSelector
  , pauseSelector
  , pausedSelector
  , resetSelector
  , runSelector
  , runningSelector
  , setDelegateSelector
  , setWorkflowSelector
  , setWorkflowViewSelector
  , stepSelector
  , stopSelector
  , workflowSelector
  , workflowViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Automator.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- run:@
run :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
run amWorkflowController sender =
  sendMessage amWorkflowController runSelector sender

-- | @- stop:@
stop :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
stop amWorkflowController sender =
  sendMessage amWorkflowController stopSelector sender

-- | @- pause:@
pause :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
pause amWorkflowController sender =
  sendMessage amWorkflowController pauseSelector sender

-- | @- step:@
step :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
step amWorkflowController sender =
  sendMessage amWorkflowController stepSelector sender

-- | @- reset:@
reset :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
reset amWorkflowController sender =
  sendMessage amWorkflowController resetSelector sender

-- | @- workflow@
workflow :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO (Id AMWorkflow)
workflow amWorkflowController =
  sendMessage amWorkflowController workflowSelector

-- | @- setWorkflow:@
setWorkflow :: (IsAMWorkflowController amWorkflowController, IsAMWorkflow value) => amWorkflowController -> value -> IO ()
setWorkflow amWorkflowController value =
  sendMessage amWorkflowController setWorkflowSelector (toAMWorkflow value)

-- | @- workflowView@
workflowView :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO (Id AMWorkflowView)
workflowView amWorkflowController =
  sendMessage amWorkflowController workflowViewSelector

-- | @- setWorkflowView:@
setWorkflowView :: (IsAMWorkflowController amWorkflowController, IsAMWorkflowView value) => amWorkflowController -> value -> IO ()
setWorkflowView amWorkflowController value =
  sendMessage amWorkflowController setWorkflowViewSelector (toAMWorkflowView value)

-- | @- delegate@
delegate :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO RawId
delegate amWorkflowController =
  sendMessage amWorkflowController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
setDelegate amWorkflowController value =
  sendMessage amWorkflowController setDelegateSelector value

-- | @- canRun@
canRun :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO Bool
canRun amWorkflowController =
  sendMessage amWorkflowController canRunSelector

-- | @- running@
running :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO Bool
running amWorkflowController =
  sendMessage amWorkflowController runningSelector

-- | @- paused@
paused :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO Bool
paused amWorkflowController =
  sendMessage amWorkflowController pausedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @run:@
runSelector :: Selector '[RawId] ()
runSelector = mkSelector "run:"

-- | @Selector@ for @stop:@
stopSelector :: Selector '[RawId] ()
stopSelector = mkSelector "stop:"

-- | @Selector@ for @pause:@
pauseSelector :: Selector '[RawId] ()
pauseSelector = mkSelector "pause:"

-- | @Selector@ for @step:@
stepSelector :: Selector '[RawId] ()
stepSelector = mkSelector "step:"

-- | @Selector@ for @reset:@
resetSelector :: Selector '[RawId] ()
resetSelector = mkSelector "reset:"

-- | @Selector@ for @workflow@
workflowSelector :: Selector '[] (Id AMWorkflow)
workflowSelector = mkSelector "workflow"

-- | @Selector@ for @setWorkflow:@
setWorkflowSelector :: Selector '[Id AMWorkflow] ()
setWorkflowSelector = mkSelector "setWorkflow:"

-- | @Selector@ for @workflowView@
workflowViewSelector :: Selector '[] (Id AMWorkflowView)
workflowViewSelector = mkSelector "workflowView"

-- | @Selector@ for @setWorkflowView:@
setWorkflowViewSelector :: Selector '[Id AMWorkflowView] ()
setWorkflowViewSelector = mkSelector "setWorkflowView:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @canRun@
canRunSelector :: Selector '[] Bool
canRunSelector = mkSelector "canRun"

-- | @Selector@ for @running@
runningSelector :: Selector '[] Bool
runningSelector = mkSelector "running"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

