{-# LANGUAGE DataKinds #-}
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
  , contentKeySpecifierSelector
  , initSelector
  , isClientInitiatedSelector
  , mediaResourceRequestEventSelector
  , mediaTypeSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO (Id AVMetricContentKeyRequestEvent)
init_ avMetricContentKeyRequestEvent =
  sendOwnedMessage avMetricContentKeyRequestEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricContentKeyRequestEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricContentKeyRequestEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the content key specifier for the request.
--
-- ObjC selector: @- contentKeySpecifier@
contentKeySpecifier :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO (Id AVContentKeySpecifier)
contentKeySpecifier avMetricContentKeyRequestEvent =
  sendMessage avMetricContentKeyRequestEvent contentKeySpecifierSelector

-- | Returns the media type. If the value cannot be determined, returns AVMediaTypeMuxed.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO (Id NSString)
mediaType avMetricContentKeyRequestEvent =
  sendMessage avMetricContentKeyRequestEvent mediaTypeSelector

-- | Returns whether the content key resource request was initiated by the client.
--
-- ObjC selector: @- isClientInitiated@
isClientInitiated :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO Bool
isClientInitiated avMetricContentKeyRequestEvent =
  sendMessage avMetricContentKeyRequestEvent isClientInitiatedSelector

-- | Returns the media resource request event which was used to satisfy the content key.
--
-- ObjC selector: @- mediaResourceRequestEvent@
mediaResourceRequestEvent :: IsAVMetricContentKeyRequestEvent avMetricContentKeyRequestEvent => avMetricContentKeyRequestEvent -> IO (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEvent avMetricContentKeyRequestEvent =
  sendMessage avMetricContentKeyRequestEvent mediaResourceRequestEventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricContentKeyRequestEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricContentKeyRequestEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @contentKeySpecifier@
contentKeySpecifierSelector :: Selector '[] (Id AVContentKeySpecifier)
contentKeySpecifierSelector = mkSelector "contentKeySpecifier"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] (Id NSString)
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @isClientInitiated@
isClientInitiatedSelector :: Selector '[] Bool
isClientInitiatedSelector = mkSelector "isClientInitiated"

-- | @Selector@ for @mediaResourceRequestEvent@
mediaResourceRequestEventSelector :: Selector '[] (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEventSelector = mkSelector "mediaResourceRequestEvent"

