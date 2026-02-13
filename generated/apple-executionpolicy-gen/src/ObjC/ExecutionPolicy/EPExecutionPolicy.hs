{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EPExecutionPolicy@.
module ObjC.ExecutionPolicy.EPExecutionPolicy
  ( EPExecutionPolicy
  , IsEPExecutionPolicy(..)
  , init_
  , addPolicyExceptionForURL_error
  , addPolicyExceptionForURL_errorSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ExecutionPolicy.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsEPExecutionPolicy epExecutionPolicy => epExecutionPolicy -> IO (Id EPExecutionPolicy)
init_ epExecutionPolicy =
  sendOwnedMessage epExecutionPolicy initSelector

-- | @- addPolicyExceptionForURL:error:@
addPolicyExceptionForURL_error :: (IsEPExecutionPolicy epExecutionPolicy, IsNSURL url, IsNSError error_) => epExecutionPolicy -> url -> error_ -> IO Bool
addPolicyExceptionForURL_error epExecutionPolicy url error_ =
  sendMessage epExecutionPolicy addPolicyExceptionForURL_errorSelector (toNSURL url) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id EPExecutionPolicy)
initSelector = mkSelector "init"

-- | @Selector@ for @addPolicyExceptionForURL:error:@
addPolicyExceptionForURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
addPolicyExceptionForURL_errorSelector = mkSelector "addPolicyExceptionForURL:error:"

