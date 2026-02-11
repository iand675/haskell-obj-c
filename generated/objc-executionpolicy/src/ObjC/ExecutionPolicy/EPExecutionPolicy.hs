{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EPExecutionPolicy@.
module ObjC.ExecutionPolicy.EPExecutionPolicy
  ( EPExecutionPolicy
  , IsEPExecutionPolicy(..)
  , init_
  , addPolicyExceptionForURL_error
  , initSelector
  , addPolicyExceptionForURL_errorSelector


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

import ObjC.ExecutionPolicy.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsEPExecutionPolicy epExecutionPolicy => epExecutionPolicy -> IO (Id EPExecutionPolicy)
init_ epExecutionPolicy  =
  sendMsg epExecutionPolicy (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- addPolicyExceptionForURL:error:@
addPolicyExceptionForURL_error :: (IsEPExecutionPolicy epExecutionPolicy, IsNSURL url, IsNSError error_) => epExecutionPolicy -> url -> error_ -> IO Bool
addPolicyExceptionForURL_error epExecutionPolicy  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg epExecutionPolicy (mkSelector "addPolicyExceptionForURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @addPolicyExceptionForURL:error:@
addPolicyExceptionForURL_errorSelector :: Selector
addPolicyExceptionForURL_errorSelector = mkSelector "addPolicyExceptionForURL:error:"

