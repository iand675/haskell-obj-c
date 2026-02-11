{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMWorkspace@.
module ObjC.Automator.AMWorkspace
  ( AMWorkspace
  , IsAMWorkspace(..)
  , runWorkflowAtPath_withInput_error
  , sharedWorkspace
  , runWorkflowAtPath_withInput_errorSelector
  , sharedWorkspaceSelector


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

-- | @- runWorkflowAtPath:withInput:error:@
runWorkflowAtPath_withInput_error :: (IsAMWorkspace amWorkspace, IsNSString path, IsNSError error_) => amWorkspace -> path -> RawId -> error_ -> IO RawId
runWorkflowAtPath_withInput_error amWorkspace  path input error_ =
withObjCPtr path $ \raw_path ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap (RawId . castPtr) $ sendMsg amWorkspace (mkSelector "runWorkflowAtPath:withInput:error:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr (unRawId input) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ sharedWorkspace@
sharedWorkspace :: IO (Id AMWorkspace)
sharedWorkspace  =
  do
    cls' <- getRequiredClass "AMWorkspace"
    sendClassMsg cls' (mkSelector "sharedWorkspace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runWorkflowAtPath:withInput:error:@
runWorkflowAtPath_withInput_errorSelector :: Selector
runWorkflowAtPath_withInput_errorSelector = mkSelector "runWorkflowAtPath:withInput:error:"

-- | @Selector@ for @sharedWorkspace@
sharedWorkspaceSelector :: Selector
sharedWorkspaceSelector = mkSelector "sharedWorkspace"

