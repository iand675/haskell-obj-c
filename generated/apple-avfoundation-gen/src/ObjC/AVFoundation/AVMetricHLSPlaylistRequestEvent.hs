{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event associated with a HLS playlist resource request.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricHLSPlaylistRequestEvent@.
module ObjC.AVFoundation.AVMetricHLSPlaylistRequestEvent
  ( AVMetricHLSPlaylistRequestEvent
  , IsAVMetricHLSPlaylistRequestEvent(..)
  , init_
  , new
  , url
  , isMultivariantPlaylist
  , mediaType
  , mediaResourceRequestEvent
  , initSelector
  , isMultivariantPlaylistSelector
  , mediaResourceRequestEventSelector
  , mediaTypeSelector
  , newSelector
  , urlSelector


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
init_ :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO (Id AVMetricHLSPlaylistRequestEvent)
init_ avMetricHLSPlaylistRequestEvent =
  sendOwnedMessage avMetricHLSPlaylistRequestEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricHLSPlaylistRequestEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricHLSPlaylistRequestEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the URL of the playlist. If no value is available, returns nil.
--
-- ObjC selector: @- url@
url :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO (Id NSURL)
url avMetricHLSPlaylistRequestEvent =
  sendMessage avMetricHLSPlaylistRequestEvent urlSelector

-- | Returns true if the playlist request is for a multivariant playlist.
--
-- ObjC selector: @- isMultivariantPlaylist@
isMultivariantPlaylist :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO Bool
isMultivariantPlaylist avMetricHLSPlaylistRequestEvent =
  sendMessage avMetricHLSPlaylistRequestEvent isMultivariantPlaylistSelector

-- | Returns the media type. If the value cannot be determined, returns AVMediaTypeMuxed.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO (Id NSString)
mediaType avMetricHLSPlaylistRequestEvent =
  sendMessage avMetricHLSPlaylistRequestEvent mediaTypeSelector

-- | Returns the media resource request event which was used to satisfy the playlist.
--
-- ObjC selector: @- mediaResourceRequestEvent@
mediaResourceRequestEvent :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEvent avMetricHLSPlaylistRequestEvent =
  sendMessage avMetricHLSPlaylistRequestEvent mediaResourceRequestEventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricHLSPlaylistRequestEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricHLSPlaylistRequestEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @isMultivariantPlaylist@
isMultivariantPlaylistSelector :: Selector '[] Bool
isMultivariantPlaylistSelector = mkSelector "isMultivariantPlaylist"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] (Id NSString)
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @mediaResourceRequestEvent@
mediaResourceRequestEventSelector :: Selector '[] (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEventSelector = mkSelector "mediaResourceRequestEvent"

