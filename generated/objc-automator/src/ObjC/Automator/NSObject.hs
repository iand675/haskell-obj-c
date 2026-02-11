{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.Automator.NSObject
  ( NSObject
  , IsNSObject(..)
  , workflowControllerWillRun
  , workflowControllerWillStop
  , workflowControllerDidRun
  , workflowControllerDidStop
  , workflowController_willRunAction
  , workflowController_didRunAction
  , workflowController_didError
  , workflowControllerWillRunSelector
  , workflowControllerWillStopSelector
  , workflowControllerDidRunSelector
  , workflowControllerDidStopSelector
  , workflowController_willRunActionSelector
  , workflowController_didRunActionSelector
  , workflowController_didErrorSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- workflowControllerWillRun:@
workflowControllerWillRun :: (IsNSObject nsObject, IsAMWorkflowController controller) => nsObject -> controller -> IO ()
workflowControllerWillRun nsObject  controller =
withObjCPtr controller $ \raw_controller ->
    sendMsg nsObject (mkSelector "workflowControllerWillRun:") retVoid [argPtr (castPtr raw_controller :: Ptr ())]

-- | @- workflowControllerWillStop:@
workflowControllerWillStop :: (IsNSObject nsObject, IsAMWorkflowController controller) => nsObject -> controller -> IO ()
workflowControllerWillStop nsObject  controller =
withObjCPtr controller $ \raw_controller ->
    sendMsg nsObject (mkSelector "workflowControllerWillStop:") retVoid [argPtr (castPtr raw_controller :: Ptr ())]

-- | @- workflowControllerDidRun:@
workflowControllerDidRun :: (IsNSObject nsObject, IsAMWorkflowController controller) => nsObject -> controller -> IO ()
workflowControllerDidRun nsObject  controller =
withObjCPtr controller $ \raw_controller ->
    sendMsg nsObject (mkSelector "workflowControllerDidRun:") retVoid [argPtr (castPtr raw_controller :: Ptr ())]

-- | @- workflowControllerDidStop:@
workflowControllerDidStop :: (IsNSObject nsObject, IsAMWorkflowController controller) => nsObject -> controller -> IO ()
workflowControllerDidStop nsObject  controller =
withObjCPtr controller $ \raw_controller ->
    sendMsg nsObject (mkSelector "workflowControllerDidStop:") retVoid [argPtr (castPtr raw_controller :: Ptr ())]

-- | @- workflowController:willRunAction:@
workflowController_willRunAction :: (IsNSObject nsObject, IsAMWorkflowController controller, IsAMAction action) => nsObject -> controller -> action -> IO ()
workflowController_willRunAction nsObject  controller action =
withObjCPtr controller $ \raw_controller ->
  withObjCPtr action $ \raw_action ->
      sendMsg nsObject (mkSelector "workflowController:willRunAction:") retVoid [argPtr (castPtr raw_controller :: Ptr ()), argPtr (castPtr raw_action :: Ptr ())]

-- | @- workflowController:didRunAction:@
workflowController_didRunAction :: (IsNSObject nsObject, IsAMWorkflowController controller, IsAMAction action) => nsObject -> controller -> action -> IO ()
workflowController_didRunAction nsObject  controller action =
withObjCPtr controller $ \raw_controller ->
  withObjCPtr action $ \raw_action ->
      sendMsg nsObject (mkSelector "workflowController:didRunAction:") retVoid [argPtr (castPtr raw_controller :: Ptr ()), argPtr (castPtr raw_action :: Ptr ())]

-- | @- workflowController:didError:@
workflowController_didError :: (IsNSObject nsObject, IsAMWorkflowController controller, IsNSError error_) => nsObject -> controller -> error_ -> IO ()
workflowController_didError nsObject  controller error_ =
withObjCPtr controller $ \raw_controller ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsObject (mkSelector "workflowController:didError:") retVoid [argPtr (castPtr raw_controller :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @workflowControllerWillRun:@
workflowControllerWillRunSelector :: Selector
workflowControllerWillRunSelector = mkSelector "workflowControllerWillRun:"

-- | @Selector@ for @workflowControllerWillStop:@
workflowControllerWillStopSelector :: Selector
workflowControllerWillStopSelector = mkSelector "workflowControllerWillStop:"

-- | @Selector@ for @workflowControllerDidRun:@
workflowControllerDidRunSelector :: Selector
workflowControllerDidRunSelector = mkSelector "workflowControllerDidRun:"

-- | @Selector@ for @workflowControllerDidStop:@
workflowControllerDidStopSelector :: Selector
workflowControllerDidStopSelector = mkSelector "workflowControllerDidStop:"

-- | @Selector@ for @workflowController:willRunAction:@
workflowController_willRunActionSelector :: Selector
workflowController_willRunActionSelector = mkSelector "workflowController:willRunAction:"

-- | @Selector@ for @workflowController:didRunAction:@
workflowController_didRunActionSelector :: Selector
workflowController_didRunActionSelector = mkSelector "workflowController:didRunAction:"

-- | @Selector@ for @workflowController:didError:@
workflowController_didErrorSelector :: Selector
workflowController_didErrorSelector = mkSelector "workflowController:didError:"

