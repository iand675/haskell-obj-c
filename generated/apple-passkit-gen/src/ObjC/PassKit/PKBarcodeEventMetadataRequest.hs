{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKBarcodeEventMetadataRequest@.
module ObjC.PassKit.PKBarcodeEventMetadataRequest
  ( PKBarcodeEventMetadataRequest
  , IsPKBarcodeEventMetadataRequest(..)
  , deviceAccountIdentifier
  , lastUsedBarcodeIdentifier
  , deviceAccountIdentifierSelector
  , lastUsedBarcodeIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- deviceAccountIdentifier@
deviceAccountIdentifier :: IsPKBarcodeEventMetadataRequest pkBarcodeEventMetadataRequest => pkBarcodeEventMetadataRequest -> IO (Id NSString)
deviceAccountIdentifier pkBarcodeEventMetadataRequest =
  sendMessage pkBarcodeEventMetadataRequest deviceAccountIdentifierSelector

-- | @- lastUsedBarcodeIdentifier@
lastUsedBarcodeIdentifier :: IsPKBarcodeEventMetadataRequest pkBarcodeEventMetadataRequest => pkBarcodeEventMetadataRequest -> IO (Id NSString)
lastUsedBarcodeIdentifier pkBarcodeEventMetadataRequest =
  sendMessage pkBarcodeEventMetadataRequest lastUsedBarcodeIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceAccountIdentifier@
deviceAccountIdentifierSelector :: Selector '[] (Id NSString)
deviceAccountIdentifierSelector = mkSelector "deviceAccountIdentifier"

-- | @Selector@ for @lastUsedBarcodeIdentifier@
lastUsedBarcodeIdentifierSelector :: Selector '[] (Id NSString)
lastUsedBarcodeIdentifierSelector = mkSelector "lastUsedBarcodeIdentifier"

