{-# LANGUAGE DataKinds #-}
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
  , issuerIdentifierSelector
  , localizedDescriptionSelector
  , newSelector
  , setIssuerIdentifierSelector
  , setLocalizedDescriptionSelector


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
init_ :: IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration => pkAddSecureElementPassConfiguration -> IO (Id PKAddSecureElementPassConfiguration)
init_ pkAddSecureElementPassConfiguration =
  sendOwnedMessage pkAddSecureElementPassConfiguration initSelector

-- | @+ new@
new :: IO (Id PKAddSecureElementPassConfiguration)
new  =
  do
    cls' <- getRequiredClass "PKAddSecureElementPassConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- issuerIdentifier@
issuerIdentifier :: IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration => pkAddSecureElementPassConfiguration -> IO (Id NSString)
issuerIdentifier pkAddSecureElementPassConfiguration =
  sendMessage pkAddSecureElementPassConfiguration issuerIdentifierSelector

-- | @- setIssuerIdentifier:@
setIssuerIdentifier :: (IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration, IsNSString value) => pkAddSecureElementPassConfiguration -> value -> IO ()
setIssuerIdentifier pkAddSecureElementPassConfiguration value =
  sendMessage pkAddSecureElementPassConfiguration setIssuerIdentifierSelector (toNSString value)

-- | @- localizedDescription@
localizedDescription :: IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration => pkAddSecureElementPassConfiguration -> IO (Id NSString)
localizedDescription pkAddSecureElementPassConfiguration =
  sendMessage pkAddSecureElementPassConfiguration localizedDescriptionSelector

-- | @- setLocalizedDescription:@
setLocalizedDescription :: (IsPKAddSecureElementPassConfiguration pkAddSecureElementPassConfiguration, IsNSString value) => pkAddSecureElementPassConfiguration -> value -> IO ()
setLocalizedDescription pkAddSecureElementPassConfiguration value =
  sendMessage pkAddSecureElementPassConfiguration setLocalizedDescriptionSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKAddSecureElementPassConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKAddSecureElementPassConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @issuerIdentifier@
issuerIdentifierSelector :: Selector '[] (Id NSString)
issuerIdentifierSelector = mkSelector "issuerIdentifier"

-- | @Selector@ for @setIssuerIdentifier:@
setIssuerIdentifierSelector :: Selector '[Id NSString] ()
setIssuerIdentifierSelector = mkSelector "setIssuerIdentifier:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector '[Id NSString] ()
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

