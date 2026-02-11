{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event associated with a HLS media segment resource request.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricHLSMediaSegmentRequestEvent@.
module ObjC.AVFoundation.AVMetricHLSMediaSegmentRequestEvent
  ( AVMetricHLSMediaSegmentRequestEvent
  , IsAVMetricHLSMediaSegmentRequestEvent(..)
  , init_
  , new
  , url
  , isMapSegment
  , mediaType
  , byteRange
  , indexFileURL
  , segmentDuration
  , mediaResourceRequestEvent
  , initSelector
  , newSelector
  , urlSelector
  , isMapSegmentSelector
  , mediaTypeSelector
  , byteRangeSelector
  , indexFileURLSelector
  , segmentDurationSelector
  , mediaResourceRequestEventSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id AVMetricHLSMediaSegmentRequestEvent)
init_ avMetricHLSMediaSegmentRequestEvent  =
  sendMsg avMetricHLSMediaSegmentRequestEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricHLSMediaSegmentRequestEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricHLSMediaSegmentRequestEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the URL of the media segment. If no value is available, returns nil.
--
-- ObjC selector: @- url@
url :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id NSURL)
url avMetricHLSMediaSegmentRequestEvent  =
  sendMsg avMetricHLSMediaSegmentRequestEvent (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns true if the media segment request is for a map segment.
--
-- ObjC selector: @- isMapSegment@
isMapSegment :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO Bool
isMapSegment avMetricHLSMediaSegmentRequestEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricHLSMediaSegmentRequestEvent (mkSelector "isMapSegment") retCULong []

-- | Returns the media type. If the value cannot be determined, returns AVMediaTypeMuxed.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id NSString)
mediaType avMetricHLSMediaSegmentRequestEvent  =
  sendMsg avMetricHLSMediaSegmentRequestEvent (mkSelector "mediaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the byte range for the media segment. If not available, the range start and end will be 0.
--
-- ObjC selector: @- byteRange@
byteRange :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO NSRange
byteRange avMetricHLSMediaSegmentRequestEvent  =
  sendMsgStret avMetricHLSMediaSegmentRequestEvent (mkSelector "byteRange") retNSRange []

-- | Returns the URL of the index file in which this segment was declared. If not available, returns nil.
--
-- ObjC selector: @- indexFileURL@
indexFileURL :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id NSURL)
indexFileURL avMetricHLSMediaSegmentRequestEvent  =
  sendMsg avMetricHLSMediaSegmentRequestEvent (mkSelector "indexFileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the duration of segment in seconds.
--
-- ObjC selector: @- segmentDuration@
segmentDuration :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO CDouble
segmentDuration avMetricHLSMediaSegmentRequestEvent  =
  sendMsg avMetricHLSMediaSegmentRequestEvent (mkSelector "segmentDuration") retCDouble []

-- | Returns the media resource request event which was used to satisfy the media segment.
--
-- ObjC selector: @- mediaResourceRequestEvent@
mediaResourceRequestEvent :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEvent avMetricHLSMediaSegmentRequestEvent  =
  sendMsg avMetricHLSMediaSegmentRequestEvent (mkSelector "mediaResourceRequestEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @isMapSegment@
isMapSegmentSelector :: Selector
isMapSegmentSelector = mkSelector "isMapSegment"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @byteRange@
byteRangeSelector :: Selector
byteRangeSelector = mkSelector "byteRange"

-- | @Selector@ for @indexFileURL@
indexFileURLSelector :: Selector
indexFileURLSelector = mkSelector "indexFileURL"

-- | @Selector@ for @segmentDuration@
segmentDurationSelector :: Selector
segmentDurationSelector = mkSelector "segmentDuration"

-- | @Selector@ for @mediaResourceRequestEvent@
mediaResourceRequestEventSelector :: Selector
mediaResourceRequestEventSelector = mkSelector "mediaResourceRequestEvent"

