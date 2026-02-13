{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMWorkflowView@.
module ObjC.Automator.AMWorkflowView
  ( AMWorkflowView
  , IsAMWorkflowView(..)
  , editable
  , setEditable
  , workflowController
  , setWorkflowController
  , editableSelector
  , setEditableSelector
  , setWorkflowControllerSelector
  , workflowControllerSelector


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

-- | @- editable@
editable :: IsAMWorkflowView amWorkflowView => amWorkflowView -> IO Bool
editable amWorkflowView =
  sendMessage amWorkflowView editableSelector

-- | @- setEditable:@
setEditable :: IsAMWorkflowView amWorkflowView => amWorkflowView -> Bool -> IO ()
setEditable amWorkflowView value =
  sendMessage amWorkflowView setEditableSelector value

-- | @- workflowController@
workflowController :: IsAMWorkflowView amWorkflowView => amWorkflowView -> IO (Id AMWorkflowController)
workflowController amWorkflowView =
  sendMessage amWorkflowView workflowControllerSelector

-- | @- setWorkflowController:@
setWorkflowController :: (IsAMWorkflowView amWorkflowView, IsAMWorkflowController value) => amWorkflowView -> value -> IO ()
setWorkflowController amWorkflowView value =
  sendMessage amWorkflowView setWorkflowControllerSelector (toAMWorkflowController value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @workflowController@
workflowControllerSelector :: Selector '[] (Id AMWorkflowController)
workflowControllerSelector = mkSelector "workflowController"

-- | @Selector@ for @setWorkflowController:@
setWorkflowControllerSelector :: Selector '[Id AMWorkflowController] ()
setWorkflowControllerSelector = mkSelector "setWorkflowController:"

