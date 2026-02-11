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
  , runSelector
  , stopSelector
  , pauseSelector
  , stepSelector
  , resetSelector
  , workflowSelector
  , setWorkflowSelector
  , workflowViewSelector
  , setWorkflowViewSelector
  , delegateSelector
  , setDelegateSelector
  , canRunSelector
  , runningSelector
  , pausedSelector


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

import ObjC.Automator.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- run:@
run :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
run amWorkflowController  sender =
    sendMsg amWorkflowController (mkSelector "run:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stop:@
stop :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
stop amWorkflowController  sender =
    sendMsg amWorkflowController (mkSelector "stop:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- pause:@
pause :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
pause amWorkflowController  sender =
    sendMsg amWorkflowController (mkSelector "pause:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- step:@
step :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
step amWorkflowController  sender =
    sendMsg amWorkflowController (mkSelector "step:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- reset:@
reset :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
reset amWorkflowController  sender =
    sendMsg amWorkflowController (mkSelector "reset:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- workflow@
workflow :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO (Id AMWorkflow)
workflow amWorkflowController  =
    sendMsg amWorkflowController (mkSelector "workflow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWorkflow:@
setWorkflow :: (IsAMWorkflowController amWorkflowController, IsAMWorkflow value) => amWorkflowController -> value -> IO ()
setWorkflow amWorkflowController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg amWorkflowController (mkSelector "setWorkflow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- workflowView@
workflowView :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO (Id AMWorkflowView)
workflowView amWorkflowController  =
    sendMsg amWorkflowController (mkSelector "workflowView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWorkflowView:@
setWorkflowView :: (IsAMWorkflowController amWorkflowController, IsAMWorkflowView value) => amWorkflowController -> value -> IO ()
setWorkflowView amWorkflowController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg amWorkflowController (mkSelector "setWorkflowView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO RawId
delegate amWorkflowController  =
    fmap (RawId . castPtr) $ sendMsg amWorkflowController (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsAMWorkflowController amWorkflowController => amWorkflowController -> RawId -> IO ()
setDelegate amWorkflowController  value =
    sendMsg amWorkflowController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- canRun@
canRun :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO Bool
canRun amWorkflowController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg amWorkflowController (mkSelector "canRun") retCULong []

-- | @- running@
running :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO Bool
running amWorkflowController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg amWorkflowController (mkSelector "running") retCULong []

-- | @- paused@
paused :: IsAMWorkflowController amWorkflowController => amWorkflowController -> IO Bool
paused amWorkflowController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg amWorkflowController (mkSelector "paused") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @run:@
runSelector :: Selector
runSelector = mkSelector "run:"

-- | @Selector@ for @stop:@
stopSelector :: Selector
stopSelector = mkSelector "stop:"

-- | @Selector@ for @pause:@
pauseSelector :: Selector
pauseSelector = mkSelector "pause:"

-- | @Selector@ for @step:@
stepSelector :: Selector
stepSelector = mkSelector "step:"

-- | @Selector@ for @reset:@
resetSelector :: Selector
resetSelector = mkSelector "reset:"

-- | @Selector@ for @workflow@
workflowSelector :: Selector
workflowSelector = mkSelector "workflow"

-- | @Selector@ for @setWorkflow:@
setWorkflowSelector :: Selector
setWorkflowSelector = mkSelector "setWorkflow:"

-- | @Selector@ for @workflowView@
workflowViewSelector :: Selector
workflowViewSelector = mkSelector "workflowView"

-- | @Selector@ for @setWorkflowView:@
setWorkflowViewSelector :: Selector
setWorkflowViewSelector = mkSelector "setWorkflowView:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @canRun@
canRunSelector :: Selector
canRunSelector = mkSelector "canRun"

-- | @Selector@ for @running@
runningSelector :: Selector
runningSelector = mkSelector "running"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

