{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccountAuthenticationModificationController@.
module ObjC.AuthenticationServices.ASAccountAuthenticationModificationController
  ( ASAccountAuthenticationModificationController
  , IsASAccountAuthenticationModificationController(..)
  , performRequest
  , performRequestSelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Perform an upgrade request, one at a time. Any requests initiated with a request already in progress will fail immediately.
--
-- ObjC selector: @- performRequest:@
performRequest :: (IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController, IsASAccountAuthenticationModificationRequest request) => asAccountAuthenticationModificationController -> request -> IO ()
performRequest asAccountAuthenticationModificationController  request =
withObjCPtr request $ \raw_request ->
    sendMsg asAccountAuthenticationModificationController (mkSelector "performRequest:") retVoid [argPtr (castPtr raw_request :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @performRequest:@
performRequestSelector :: Selector
performRequestSelector = mkSelector "performRequest:"

