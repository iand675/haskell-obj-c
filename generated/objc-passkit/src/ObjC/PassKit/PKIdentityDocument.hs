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
  , initSelector
  , newSelector
  , encryptedDataSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKIdentityDocument pkIdentityDocument => pkIdentityDocument -> IO (Id PKIdentityDocument)
init_ pkIdentityDocument  =
  sendMsg pkIdentityDocument (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKIdentityDocument)
new  =
  do
    cls' <- getRequiredClass "PKIdentityDocument"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An encrypted data blob containing the requested document information and associated metadata. This is encrypted to the public key on-file with the Developer portal for the calling app, and should be passed to the server holding the corresponding private key for decryption. This data is not intended to be read on-device.
--
-- ObjC selector: @- encryptedData@
encryptedData :: IsPKIdentityDocument pkIdentityDocument => pkIdentityDocument -> IO (Id NSData)
encryptedData pkIdentityDocument  =
  sendMsg pkIdentityDocument (mkSelector "encryptedData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @encryptedData@
encryptedDataSelector :: Selector
encryptedDataSelector = mkSelector "encryptedData"

