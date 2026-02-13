{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Automator.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- runWorkflowAtPath:withInput:error:@
runWorkflowAtPath_withInput_error :: (IsAMWorkspace amWorkspace, IsNSString path, IsNSError error_) => amWorkspace -> path -> RawId -> error_ -> IO RawId
runWorkflowAtPath_withInput_error amWorkspace path input error_ =
  sendMessage amWorkspace runWorkflowAtPath_withInput_errorSelector (toNSString path) input (toNSError error_)

-- | @+ sharedWorkspace@
sharedWorkspace :: IO (Id AMWorkspace)
sharedWorkspace  =
  do
    cls' <- getRequiredClass "AMWorkspace"
    sendClassMessage cls' sharedWorkspaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runWorkflowAtPath:withInput:error:@
runWorkflowAtPath_withInput_errorSelector :: Selector '[Id NSString, RawId, Id NSError] RawId
runWorkflowAtPath_withInput_errorSelector = mkSelector "runWorkflowAtPath:withInput:error:"

-- | @Selector@ for @sharedWorkspace@
sharedWorkspaceSelector :: Selector '[] (Id AMWorkspace)
sharedWorkspaceSelector = mkSelector "sharedWorkspace"

