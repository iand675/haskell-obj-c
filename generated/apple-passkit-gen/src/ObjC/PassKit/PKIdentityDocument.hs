{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents the response of a request for an identity document.
--
-- Generated bindings for @PKIdentityDocument@.
module ObjC.PassKit.PKIdentityDocument
  ( PKIdentityDocument
  , IsPKIdentityDocument(..)
  , init_
  , new
  , encryptedData
  , encryptedDataSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKIdentityDocument pkIdentityDocument => pkIdentityDocument -> IO (Id PKIdentityDocument)
init_ pkIdentityDocument =
  sendOwnedMessage pkIdentityDocument initSelector

-- | @+ new@
new :: IO (Id PKIdentityDocument)
new  =
  do
    cls' <- getRequiredClass "PKIdentityDocument"
    sendOwnedClassMessage cls' newSelector

-- | An encrypted data blob containing the requested document information and associated metadata. This is encrypted to the public key on-file with the Developer portal for the calling app, and should be passed to the server holding the corresponding private key for decryption. This data is not intended to be read on-device.
--
-- ObjC selector: @- encryptedData@
encryptedData :: IsPKIdentityDocument pkIdentityDocument => pkIdentityDocument -> IO (Id NSData)
encryptedData pkIdentityDocument =
  sendMessage pkIdentityDocument encryptedDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKIdentityDocument)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKIdentityDocument)
newSelector = mkSelector "new"

-- | @Selector@ for @encryptedData@
encryptedDataSelector :: Selector '[] (Id NSData)
encryptedDataSelector = mkSelector "encryptedData"

