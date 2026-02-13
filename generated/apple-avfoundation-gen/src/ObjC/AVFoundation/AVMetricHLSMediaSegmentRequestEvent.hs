{-# LANGUAGE DataKinds #-}
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
  , byteRangeSelector
  , indexFileURLSelector
  , initSelector
  , isMapSegmentSelector
  , mediaResourceRequestEventSelector
  , mediaTypeSelector
  , newSelector
  , segmentDurationSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id AVMetricHLSMediaSegmentRequestEvent)
init_ avMetricHLSMediaSegmentRequestEvent =
  sendOwnedMessage avMetricHLSMediaSegmentRequestEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricHLSMediaSegmentRequestEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricHLSMediaSegmentRequestEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the URL of the media segment. If no value is available, returns nil.
--
-- ObjC selector: @- url@
url :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id NSURL)
url avMetricHLSMediaSegmentRequestEvent =
  sendMessage avMetricHLSMediaSegmentRequestEvent urlSelector

-- | Returns true if the media segment request is for a map segment.
--
-- ObjC selector: @- isMapSegment@
isMapSegment :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO Bool
isMapSegment avMetricHLSMediaSegmentRequestEvent =
  sendMessage avMetricHLSMediaSegmentRequestEvent isMapSegmentSelector

-- | Returns the media type. If the value cannot be determined, returns AVMediaTypeMuxed.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id NSString)
mediaType avMetricHLSMediaSegmentRequestEvent =
  sendMessage avMetricHLSMediaSegmentRequestEvent mediaTypeSelector

-- | Returns the byte range for the media segment. If not available, the range start and end will be 0.
--
-- ObjC selector: @- byteRange@
byteRange :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO NSRange
byteRange avMetricHLSMediaSegmentRequestEvent =
  sendMessage avMetricHLSMediaSegmentRequestEvent byteRangeSelector

-- | Returns the URL of the index file in which this segment was declared. If not available, returns nil.
--
-- ObjC selector: @- indexFileURL@
indexFileURL :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id NSURL)
indexFileURL avMetricHLSMediaSegmentRequestEvent =
  sendMessage avMetricHLSMediaSegmentRequestEvent indexFileURLSelector

-- | Returns the duration of segment in seconds.
--
-- ObjC selector: @- segmentDuration@
segmentDuration :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO CDouble
segmentDuration avMetricHLSMediaSegmentRequestEvent =
  sendMessage avMetricHLSMediaSegmentRequestEvent segmentDurationSelector

-- | Returns the media resource request event which was used to satisfy the media segment.
--
-- ObjC selector: @- mediaResourceRequestEvent@
mediaResourceRequestEvent :: IsAVMetricHLSMediaSegmentRequestEvent avMetricHLSMediaSegmentRequestEvent => avMetricHLSMediaSegmentRequestEvent -> IO (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEvent avMetricHLSMediaSegmentRequestEvent =
  sendMessage avMetricHLSMediaSegmentRequestEvent mediaResourceRequestEventSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricHLSMediaSegmentRequestEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricHLSMediaSegmentRequestEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @isMapSegment@
isMapSegmentSelector :: Selector '[] Bool
isMapSegmentSelector = mkSelector "isMapSegment"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] (Id NSString)
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @byteRange@
byteRangeSelector :: Selector '[] NSRange
byteRangeSelector = mkSelector "byteRange"

-- | @Selector@ for @indexFileURL@
indexFileURLSelector :: Selector '[] (Id NSURL)
indexFileURLSelector = mkSelector "indexFileURL"

-- | @Selector@ for @segmentDuration@
segmentDurationSelector :: Selector '[] CDouble
segmentDurationSelector = mkSelector "segmentDuration"

-- | @Selector@ for @mediaResourceRequestEvent@
mediaResourceRequestEventSelector :: Selector '[] (Id AVMetricMediaResourceRequestEvent)
mediaResourceRequestEventSelector = mkSelector "mediaResourceRequestEvent"

