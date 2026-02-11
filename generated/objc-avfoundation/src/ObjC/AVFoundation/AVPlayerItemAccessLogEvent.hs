{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerItemAccessLogEvent represents a single log entry.
--
-- An AVPlayerItemAccessLogEvent provides named properties for accessing the data fields of each log event. None of the properties of this class are observable.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemAccessLogEvent@.
module ObjC.AVFoundation.AVPlayerItemAccessLogEvent
  ( AVPlayerItemAccessLogEvent
  , IsAVPlayerItemAccessLogEvent(..)
  , init_
  , new
  , numberOfSegmentsDownloaded
  , numberOfMediaRequests
  , playbackStartDate
  , uri
  , serverAddress
  , numberOfServerAddressChanges
  , playbackSessionID
  , playbackStartOffset
  , segmentsDownloadedDuration
  , durationWatched
  , numberOfStalls
  , numberOfBytesTransferred
  , transferDuration
  , observedBitrate
  , indicatedBitrate
  , indicatedAverageBitrate
  , averageVideoBitrate
  , averageAudioBitrate
  , numberOfDroppedVideoFrames
  , startupTime
  , downloadOverdue
  , observedMaxBitrate
  , observedMinBitrate
  , observedBitrateStandardDeviation
  , playbackType
  , mediaRequestsWWAN
  , switchBitrate
  , initSelector
  , newSelector
  , numberOfSegmentsDownloadedSelector
  , numberOfMediaRequestsSelector
  , playbackStartDateSelector
  , uriSelector
  , serverAddressSelector
  , numberOfServerAddressChangesSelector
  , playbackSessionIDSelector
  , playbackStartOffsetSelector
  , segmentsDownloadedDurationSelector
  , durationWatchedSelector
  , numberOfStallsSelector
  , numberOfBytesTransferredSelector
  , transferDurationSelector
  , observedBitrateSelector
  , indicatedBitrateSelector
  , indicatedAverageBitrateSelector
  , averageVideoBitrateSelector
  , averageAudioBitrateSelector
  , numberOfDroppedVideoFramesSelector
  , startupTimeSelector
  , downloadOverdueSelector
  , observedMaxBitrateSelector
  , observedMinBitrateSelector
  , observedBitrateStandardDeviationSelector
  , playbackTypeSelector
  , mediaRequestsWWANSelector
  , switchBitrateSelector


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
init_ :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id AVPlayerItemAccessLogEvent)
init_ avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerItemAccessLogEvent)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemAccessLogEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A count of media segments downloaded.
--
-- Value is negative if unknown. A count of media segments downloaded from the server to this client. Corresponds to "sc-count". This property is not observable. This property is deprecated. Use numberOfMediaRequests instead.
--
-- ObjC selector: @- numberOfSegmentsDownloaded@
numberOfSegmentsDownloaded :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfSegmentsDownloaded avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "numberOfSegmentsDownloaded") retCLong []

-- | A count of media read requests.
--
-- Value is negative if unknown. A count of media read requests from the server to this client. Corresponds to "sc-count". For HTTP live Streaming, a count of media segments downloaded from the server to this client. For progressive-style HTTP media downloads, a count of HTTP GET (byte-range) requests for the resource. This property is not observable.
--
-- ObjC selector: @- numberOfMediaRequests@
numberOfMediaRequests :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfMediaRequests avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "numberOfMediaRequests") retCLong []

-- | The date/time at which playback began for this event. Can be nil.
--
-- If nil is returned the date is unknown. Corresponds to "date". This property is not observable.
--
-- ObjC selector: @- playbackStartDate@
playbackStartDate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSDate)
playbackStartDate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "playbackStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URI of the playback item. Can be nil.
--
-- If nil is returned the URI is unknown. Corresponds to "uri". This property is not observable.
--
-- ObjC selector: @- URI@
uri :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSString)
uri avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "URI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The IP address of the server that was the source of the last delivered media segment. Can be nil.
--
-- If nil is returned the address is unknown. Can be either an IPv4 or IPv6 address. Corresponds to "s-ip". This property is not observable.
--
-- ObjC selector: @- serverAddress@
serverAddress :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSString)
serverAddress avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "serverAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A count of changes to the property serverAddress, see above, over the last uninterrupted period of playback.
--
-- Value is negative if unknown. Corresponds to "s-ip-changes". This property is not observable.
--
-- ObjC selector: @- numberOfServerAddressChanges@
numberOfServerAddressChanges :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfServerAddressChanges avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "numberOfServerAddressChanges") retCLong []

-- | A GUID that identifies the playback session. This value is used in HTTP requests. Can be nil.
--
-- If nil is returned the GUID is unknown. Corresponds to "cs-guid". This property is not observable.
--
-- ObjC selector: @- playbackSessionID@
playbackSessionID :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSString)
playbackSessionID avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "playbackSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An offset into the playlist where the last uninterrupted period of playback began. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-start-time". This property is not observable.
--
-- ObjC selector: @- playbackStartOffset@
playbackStartOffset :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
playbackStartOffset avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "playbackStartOffset") retCDouble []

-- | The accumulated duration of the media downloaded. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-duration-downloaded". This property is not observable.
--
-- ObjC selector: @- segmentsDownloadedDuration@
segmentsDownloadedDuration :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
segmentsDownloadedDuration avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "segmentsDownloadedDuration") retCDouble []

-- | The accumulated duration of the media played. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-duration-watched". This property is not observable.
--
-- ObjC selector: @- durationWatched@
durationWatched :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
durationWatched avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "durationWatched") retCDouble []

-- | The total number of playback stalls encountered.
--
-- Value is negative if unknown. Corresponds to "c-stalls". This property is not observable.
--
-- ObjC selector: @- numberOfStalls@
numberOfStalls :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfStalls avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "numberOfStalls") retCLong []

-- | The accumulated number of bytes transferred.
--
-- Value is negative if unknown. Corresponds to "bytes". This property is not observable.
--
-- ObjC selector: @- numberOfBytesTransferred@
numberOfBytesTransferred :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfBytesTransferred avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "numberOfBytesTransferred") retCLong []

-- | The accumulated duration of active network transfer of bytes. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-transfer-duration". This property is not observable.
--
-- ObjC selector: @- transferDuration@
transferDuration :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
transferDuration avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "transferDuration") retCDouble []

-- | The empirical throughput across all media downloaded. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "c-observed-bitrate". This property is not observable.
--
-- ObjC selector: @- observedBitrate@
observedBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
observedBitrate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "observedBitrate") retCDouble []

-- | The throughput required to play the stream, as advertised by the server. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "sc-indicated-bitrate". This property is not observable.
--
-- ObjC selector: @- indicatedBitrate@
indicatedBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
indicatedBitrate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "indicatedBitrate") retCDouble []

-- | Average throughput required to play the stream, as advertised by the server. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "sc-indicated-avg-bitrate". This property is not observable.
--
-- ObjC selector: @- indicatedAverageBitrate@
indicatedAverageBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
indicatedAverageBitrate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "indicatedAverageBitrate") retCDouble []

-- | The average bitrate of video track if it is unmuxed. Average bitrate of combined content if muxed. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "c-avg-video-bitrate". This property is not observable.
--
-- ObjC selector: @- averageVideoBitrate@
averageVideoBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
averageVideoBitrate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "averageVideoBitrate") retCDouble []

-- | The average bitrate of audio track. This is not available if audio is muxed with video. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "c-avg-audio-bitrate". This property is not observable.
--
-- ObjC selector: @- averageAudioBitrate@
averageAudioBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
averageAudioBitrate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "averageAudioBitrate") retCDouble []

-- | The total number of dropped video frames.
--
-- Value is negative if unknown. Corresponds to "c-frames-dropped". This property is not observable.
--
-- ObjC selector: @- numberOfDroppedVideoFrames@
numberOfDroppedVideoFrames :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfDroppedVideoFrames avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "numberOfDroppedVideoFrames") retCLong []

-- | The accumulated duration until player item is ready to play. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-startup-time". This property is not observable.
--
-- ObjC selector: @- startupTime@
startupTime :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
startupTime avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "startupTime") retCDouble []

-- | The total number of times the download of the segments took too long.
--
-- Value is negative if unknown. Corresponds to "c-overdue". This property is not observable.
--
-- ObjC selector: @- downloadOverdue@
downloadOverdue :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
downloadOverdue avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "downloadOverdue") retCLong []

-- | Maximum observed segment download bit rate.
--
-- Value is negative if unknown. Corresponds to "c-observed-max-bitrate". This property is not observable.
--
-- ObjC selector: @- observedMaxBitrate@
observedMaxBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
observedMaxBitrate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "observedMaxBitrate") retCDouble []

-- | Minimum observed segment download bit rate.
--
-- Value is negative if unknown. Corresponds to "c-observed-min-bitrate". This property is not observable.
--
-- ObjC selector: @- observedMinBitrate@
observedMinBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
observedMinBitrate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "observedMinBitrate") retCDouble []

-- | Standard deviation of observed segment download bit rates.
--
-- Value is negative if unknown. Corresponds to "c-observed-bitrate-sd". This property is not observable.
--
-- ObjC selector: @- observedBitrateStandardDeviation@
observedBitrateStandardDeviation :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
observedBitrateStandardDeviation avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "observedBitrateStandardDeviation") retCDouble []

-- | Playback type (LIVE, VOD, FILE).
--
-- If nil is returned the playback type is unknown. Corresponds to "s-playback-type". This property is not observable.
--
-- ObjC selector: @- playbackType@
playbackType :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSString)
playbackType avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "playbackType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Number of network read requests over WWAN.
--
-- Value is negative if unknown. Corresponds to "sc-wwan-count". This property is not observable.
--
-- ObjC selector: @- mediaRequestsWWAN@
mediaRequestsWWAN :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
mediaRequestsWWAN avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "mediaRequestsWWAN") retCLong []

-- | Bandwidth that caused us to switch (up or down).
--
-- Value is negative if unknown. Corresponds to "c-switch-bitrate". This property is not observable.
--
-- ObjC selector: @- switchBitrate@
switchBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
switchBitrate avPlayerItemAccessLogEvent  =
  sendMsg avPlayerItemAccessLogEvent (mkSelector "switchBitrate") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @numberOfSegmentsDownloaded@
numberOfSegmentsDownloadedSelector :: Selector
numberOfSegmentsDownloadedSelector = mkSelector "numberOfSegmentsDownloaded"

-- | @Selector@ for @numberOfMediaRequests@
numberOfMediaRequestsSelector :: Selector
numberOfMediaRequestsSelector = mkSelector "numberOfMediaRequests"

-- | @Selector@ for @playbackStartDate@
playbackStartDateSelector :: Selector
playbackStartDateSelector = mkSelector "playbackStartDate"

-- | @Selector@ for @URI@
uriSelector :: Selector
uriSelector = mkSelector "URI"

-- | @Selector@ for @serverAddress@
serverAddressSelector :: Selector
serverAddressSelector = mkSelector "serverAddress"

-- | @Selector@ for @numberOfServerAddressChanges@
numberOfServerAddressChangesSelector :: Selector
numberOfServerAddressChangesSelector = mkSelector "numberOfServerAddressChanges"

-- | @Selector@ for @playbackSessionID@
playbackSessionIDSelector :: Selector
playbackSessionIDSelector = mkSelector "playbackSessionID"

-- | @Selector@ for @playbackStartOffset@
playbackStartOffsetSelector :: Selector
playbackStartOffsetSelector = mkSelector "playbackStartOffset"

-- | @Selector@ for @segmentsDownloadedDuration@
segmentsDownloadedDurationSelector :: Selector
segmentsDownloadedDurationSelector = mkSelector "segmentsDownloadedDuration"

-- | @Selector@ for @durationWatched@
durationWatchedSelector :: Selector
durationWatchedSelector = mkSelector "durationWatched"

-- | @Selector@ for @numberOfStalls@
numberOfStallsSelector :: Selector
numberOfStallsSelector = mkSelector "numberOfStalls"

-- | @Selector@ for @numberOfBytesTransferred@
numberOfBytesTransferredSelector :: Selector
numberOfBytesTransferredSelector = mkSelector "numberOfBytesTransferred"

-- | @Selector@ for @transferDuration@
transferDurationSelector :: Selector
transferDurationSelector = mkSelector "transferDuration"

-- | @Selector@ for @observedBitrate@
observedBitrateSelector :: Selector
observedBitrateSelector = mkSelector "observedBitrate"

-- | @Selector@ for @indicatedBitrate@
indicatedBitrateSelector :: Selector
indicatedBitrateSelector = mkSelector "indicatedBitrate"

-- | @Selector@ for @indicatedAverageBitrate@
indicatedAverageBitrateSelector :: Selector
indicatedAverageBitrateSelector = mkSelector "indicatedAverageBitrate"

-- | @Selector@ for @averageVideoBitrate@
averageVideoBitrateSelector :: Selector
averageVideoBitrateSelector = mkSelector "averageVideoBitrate"

-- | @Selector@ for @averageAudioBitrate@
averageAudioBitrateSelector :: Selector
averageAudioBitrateSelector = mkSelector "averageAudioBitrate"

-- | @Selector@ for @numberOfDroppedVideoFrames@
numberOfDroppedVideoFramesSelector :: Selector
numberOfDroppedVideoFramesSelector = mkSelector "numberOfDroppedVideoFrames"

-- | @Selector@ for @startupTime@
startupTimeSelector :: Selector
startupTimeSelector = mkSelector "startupTime"

-- | @Selector@ for @downloadOverdue@
downloadOverdueSelector :: Selector
downloadOverdueSelector = mkSelector "downloadOverdue"

-- | @Selector@ for @observedMaxBitrate@
observedMaxBitrateSelector :: Selector
observedMaxBitrateSelector = mkSelector "observedMaxBitrate"

-- | @Selector@ for @observedMinBitrate@
observedMinBitrateSelector :: Selector
observedMinBitrateSelector = mkSelector "observedMinBitrate"

-- | @Selector@ for @observedBitrateStandardDeviation@
observedBitrateStandardDeviationSelector :: Selector
observedBitrateStandardDeviationSelector = mkSelector "observedBitrateStandardDeviation"

-- | @Selector@ for @playbackType@
playbackTypeSelector :: Selector
playbackTypeSelector = mkSelector "playbackType"

-- | @Selector@ for @mediaRequestsWWAN@
mediaRequestsWWANSelector :: Selector
mediaRequestsWWANSelector = mkSelector "mediaRequestsWWAN"

-- | @Selector@ for @switchBitrate@
switchBitrateSelector :: Selector
switchBitrateSelector = mkSelector "switchBitrate"

