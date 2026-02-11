{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAddSecureElementPassConfiguration@.
module ObjC.PassKit.PKAddSecureElementPassConfiguration
  ( PKAddSecureElementPassConfiguration
  , IsPKAddSecureElementPassConfiguration(..)
  , init_
  , new
  , issuerIdentifier
  , setIssuerIdentifier
  , localizedDescription
  , setLocalizedDescription
  , initSelector
  , newSelector
  , issuerIdentifierSelector
  , setIssuerIdentifierSelector
  , localizedDescriptionSelector
  , setLocalizedDescriptionSelector


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
init_ :: IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration => pkAddSecureElementPassConfiguration -> IO (Id PKAddSecureElementPassConfiguration)
init_ pkAddSecureElementPassConfiguration  =
  sendMsg pkAddSecureElementPassConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKAddSecureElementPassConfiguration)
new  =
  do
    cls' <- getRequiredClass "PKAddSecureElementPassConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- issuerIdentifier@
issuerIdentifier :: IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration => pkAddSecureElementPassConfiguration -> IO (Id NSString)
issuerIdentifier pkAddSecureElementPassConfiguration  =
  sendMsg pkAddSecureElementPassConfiguration (mkSelector "issuerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIssuerIdentifier:@
setIssuerIdentifier :: (IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration, IsNSString value) => pkAddSecureElementPassConfiguration -> value -> IO ()
setIssuerIdentifier pkAddSecureElementPassConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddSecureElementPassConfiguration (mkSelector "setIssuerIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localizedDescription@
localizedDescription :: IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration => pkAddSecureElementPassConfiguration -> IO (Id NSString)
localizedDescription pkAddSecureElementPassConfiguration  =
  sendMsg pkAddSecureElementPassConfiguration (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedDescription:@
setLocalizedDescription :: (IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration, IsNSString value) => pkAddSecureElementPassConfiguration -> value -> IO ()
setLocalizedDescription pkAddSecureElementPassConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddSecureElementPassConfiguration (mkSelector "setLocalizedDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @issuerIdentifier@
issuerIdentifierSelector :: Selector
issuerIdentifierSelector = mkSelector "issuerIdentifier"

-- | @Selector@ for @setIssuerIdentifier:@
setIssuerIdentifierSelector :: Selector
setIssuerIdentifierSelector = mkSelector "setIssuerIdentifier:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

