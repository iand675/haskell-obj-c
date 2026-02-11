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
  , workflowControllerSelector
  , setWorkflowControllerSelector


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

-- | @- editable@
editable :: IsAMWorkflowView amWorkflowView => amWorkflowView -> IO Bool
editable amWorkflowView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg amWorkflowView (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsAMWorkflowView amWorkflowView => amWorkflowView -> Bool -> IO ()
setEditable amWorkflowView  value =
  sendMsg amWorkflowView (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- workflowController@
workflowController :: IsAMWorkflowView amWorkflowView => amWorkflowView -> IO (Id AMWorkflowController)
workflowController amWorkflowView  =
  sendMsg amWorkflowView (mkSelector "workflowController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWorkflowController:@
setWorkflowController :: (IsAMWorkflowView amWorkflowView, IsAMWorkflowController value) => amWorkflowView -> value -> IO ()
setWorkflowController amWorkflowView  value =
withObjCPtr value $ \raw_value ->
    sendMsg amWorkflowView (mkSelector "setWorkflowController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @workflowController@
workflowControllerSelector :: Selector
workflowControllerSelector = mkSelector "workflowController"

-- | @Selector@ for @setWorkflowController:@
setWorkflowControllerSelector :: Selector
setWorkflowControllerSelector = mkSelector "setWorkflowController:"

