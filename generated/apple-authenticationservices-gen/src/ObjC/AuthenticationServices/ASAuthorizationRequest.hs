{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationRequest@.
module ObjC.AuthenticationServices.ASAuthorizationRequest
  ( ASAuthorizationRequest
  , IsASAuthorizationRequest(..)
  , new
  , init_
  , provider
  , newSelector
  , initSelector
  , providerSelector


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

-- | @+ new@
new :: IO (Id ASAuthorizationRequest)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationRequest asAuthorizationRequest => asAuthorizationRequest -> IO (Id ASAuthorizationRequest)
init_ asAuthorizationRequest  =
    sendMsg asAuthorizationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The provider object that is being used to service this request
--
-- ObjC selector: @- provider@
provider :: IsASAuthorizationRequest asAuthorizationRequest => asAuthorizationRequest -> IO RawId
provider asAuthorizationRequest  =
    fmap (RawId . castPtr) $ sendMsg asAuthorizationRequest (mkSelector "provider") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @provider@
providerSelector :: Selector
providerSelector = mkSelector "provider"

