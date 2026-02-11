{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialParameters@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialParameters
  ( ASAuthorizationPublicKeyCredentialParameters
  , IsASAuthorizationPublicKeyCredentialParameters(..)
  , initWithAlgorithm
  , new
  , init_
  , algorithm
  , initWithAlgorithmSelector
  , newSelector
  , initSelector
  , algorithmSelector


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

-- | @- initWithAlgorithm:@
initWithAlgorithm :: IsASAuthorizationPublicKeyCredentialParameters asAuthorizationPublicKeyCredentialParameters => asAuthorizationPublicKeyCredentialParameters -> CLong -> IO (Id ASAuthorizationPublicKeyCredentialParameters)
initWithAlgorithm asAuthorizationPublicKeyCredentialParameters  algorithm =
  sendMsg asAuthorizationPublicKeyCredentialParameters (mkSelector "initWithAlgorithm:") (retPtr retVoid) [argCLong (fromIntegral algorithm)] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationPublicKeyCredentialParameters)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPublicKeyCredentialParameters asAuthorizationPublicKeyCredentialParameters => asAuthorizationPublicKeyCredentialParameters -> IO (Id ASAuthorizationPublicKeyCredentialParameters)
init_ asAuthorizationPublicKeyCredentialParameters  =
  sendMsg asAuthorizationPublicKeyCredentialParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A COSE algorithm indentifier.
--
-- ObjC selector: @- algorithm@
algorithm :: IsASAuthorizationPublicKeyCredentialParameters asAuthorizationPublicKeyCredentialParameters => asAuthorizationPublicKeyCredentialParameters -> IO CLong
algorithm asAuthorizationPublicKeyCredentialParameters  =
  sendMsg asAuthorizationPublicKeyCredentialParameters (mkSelector "algorithm") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAlgorithm:@
initWithAlgorithmSelector :: Selector
initWithAlgorithmSelector = mkSelector "initWithAlgorithm:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @algorithm@
algorithmSelector :: Selector
algorithmSelector = mkSelector "algorithm"

