{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput
  ( ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput
  , IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput(..)
  , new
  , init_
  , readData
  , didWrite
  , didWriteSelector
  , initSelector
  , newSelector
  , readDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput => asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
init_ asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput =
  sendOwnedMessage asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput initSelector

-- | @- readData@
readData :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput => asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput -> IO (Id NSData)
readData asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput =
  sendMessage asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput readDataSelector

-- | @- didWrite@
didWrite :: IsASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput => asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput -> IO Bool
didWrite asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput =
  sendMessage asAuthorizationPublicKeyCredentialLargeBlobAssertionOutput didWriteSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @readData@
readDataSelector :: Selector '[] (Id NSData)
readDataSelector = mkSelector "readData"

-- | @Selector@ for @didWrite@
didWriteSelector :: Selector '[] Bool
didWriteSelector = mkSelector "didWrite"

