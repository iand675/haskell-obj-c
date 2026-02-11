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

-- | @- deviceAccountIdentifier@
deviceAccountIdentifier :: IsPKBarcodeEventMetadataRequest pkBarcodeEventMetadataRequest => pkBarcodeEventMetadataRequest -> IO (Id NSString)
deviceAccountIdentifier pkBarcodeEventMetadataRequest  =
  sendMsg pkBarcodeEventMetadataRequest (mkSelector "deviceAccountIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastUsedBarcodeIdentifier@
lastUsedBarcodeIdentifier :: IsPKBarcodeEventMetadataRequest pkBarcodeEventMetadataRequest => pkBarcodeEventMetadataRequest -> IO (Id NSString)
lastUsedBarcodeIdentifier pkBarcodeEventMetadataRequest  =
  sendMsg pkBarcodeEventMetadataRequest (mkSelector "lastUsedBarcodeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceAccountIdentifier@
deviceAccountIdentifierSelector :: Selector
deviceAccountIdentifierSelector = mkSelector "deviceAccountIdentifier"

-- | @Selector@ for @lastUsedBarcodeIdentifier@
lastUsedBarcodeIdentifierSelector :: Selector
lastUsedBarcodeIdentifierSelector = mkSelector "lastUsedBarcodeIdentifier"

