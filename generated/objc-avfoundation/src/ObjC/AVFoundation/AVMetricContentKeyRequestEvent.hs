{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event associated with a HLS content key resource request.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricContentKeyRequestEvent@.
module ObjC.AVFoundation.AVMetricContentKeyRequestEvent
  ( AVMetricContentKeyRequestEvent
  , IsAVMetricContentKeyRequestEvent(..)
  , init_
  , new
  , contentKeySpecifier
  , mediaType
  , isClientInitiated
  , mediaResourceRequestEvent
  , initSelector
  , newSelector
  , contentKeySpecifierSelector
  , mediaTypeSelector
  , isClientInitiatedSelector
  , mediaResourceRequestEventSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO (Id AVMetricContentKeyRequestEvent)
init_ avMetricContentKeyRequestEvent  =
  sendMsg avMetricContentKeyRequestEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricContentKeyRequestEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricContentKeyRequestEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the content key specifier for the request.
--
-- ObjC selector: @- contentKeySpecifier@
contentKeySpecifier :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO (Id AVContentKeySpecifier)
contentKeySpecifier avMetricContentKeyRequestEvent  =
  sendMsg avMetricContentKeyRequestEvent (mkSelector "contentKeySpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the media type. If the value cannot be determined, returns AVMediaTypeMuxed.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO (Id NSString)
mediaType avMetricContentKeyRequestEvent  =
  sendMsg avMetricContentKeyRequestEvent (mkSelector "mediaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns whether the content key resource request was initiated by the client.
--
-- ObjC selector: @- isClientInitiated@
isClientInitiated :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO Bool
isClientInitiated avMetricContentKeyRequestEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricContentKeyRequestEvent (mkSelector "isClientInitiated") retCULong []

-- | Returns the media resource request event which was used to satisfy the content key.
--
-- ObjC selector: @- mediaResourceRequestEvent@
mediaResourceRequestEvent :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEvent avMetricContentKeyRequestEvent  =
  sendMsg avMetricContentKeyRequestEvent (mkSelector "mediaResourceRequestEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @contentKeySpecifier@
contentKeySpecifierSelector :: Selector
contentKeySpecifierSelector = mkSelector "contentKeySpecifier"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @isClientInitiated@
isClientInitiatedSelector :: Selector
isClientInitiatedSelector = mkSelector "isClientInitiated"

-- | @Selector@ for @mediaResourceRequestEvent@
mediaResourceRequestEventSelector :: Selector
mediaResourceRequestEventSelector = mkSelector "mediaResourceRequestEvent"

