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
  , newSelector
  , urlSelector
  , isMultivariantPlaylistSelector
  , mediaTypeSelector
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
init_ :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO (Id AVMetricHLSPlaylistRequestEvent)
init_ avMetricHLSPlaylistRequestEvent  =
  sendMsg avMetricHLSPlaylistRequestEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricHLSPlaylistRequestEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricHLSPlaylistRequestEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the URL of the playlist. If no value is available, returns nil.
--
-- ObjC selector: @- url@
url :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO (Id NSURL)
url avMetricHLSPlaylistRequestEvent  =
  sendMsg avMetricHLSPlaylistRequestEvent (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns true if the playlist request is for a multivariant playlist.
--
-- ObjC selector: @- isMultivariantPlaylist@
isMultivariantPlaylist :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO Bool
isMultivariantPlaylist avMetricHLSPlaylistRequestEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricHLSPlaylistRequestEvent (mkSelector "isMultivariantPlaylist") retCULong []

-- | Returns the media type. If the value cannot be determined, returns AVMediaTypeMuxed.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO (Id NSString)
mediaType avMetricHLSPlaylistRequestEvent  =
  sendMsg avMetricHLSPlaylistRequestEvent (mkSelector "mediaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the media resource request event which was used to satisfy the playlist.
--
-- ObjC selector: @- mediaResourceRequestEvent@
mediaResourceRequestEvent :: IsAVMetricHLSPlaylistRequestEvent avMetricHLSPlaylistRequestEvent => avMetricHLSPlaylistRequestEvent -> IO (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEvent avMetricHLSPlaylistRequestEvent  =
  sendMsg avMetricHLSPlaylistRequestEvent (mkSelector "mediaResourceRequestEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @isMultivariantPlaylist@
isMultivariantPlaylistSelector :: Selector
isMultivariantPlaylistSelector = mkSelector "isMultivariantPlaylist"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @mediaResourceRequestEvent@
mediaResourceRequestEventSelector :: Selector
mediaResourceRequestEventSelector = mkSelector "mediaResourceRequestEvent"

