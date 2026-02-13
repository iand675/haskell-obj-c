{-# LANGUAGE DataKinds #-}
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
  , averageAudioBitrateSelector
  , averageVideoBitrateSelector
  , downloadOverdueSelector
  , durationWatchedSelector
  , indicatedAverageBitrateSelector
  , indicatedBitrateSelector
  , initSelector
  , mediaRequestsWWANSelector
  , newSelector
  , numberOfBytesTransferredSelector
  , numberOfDroppedVideoFramesSelector
  , numberOfMediaRequestsSelector
  , numberOfSegmentsDownloadedSelector
  , numberOfServerAddressChangesSelector
  , numberOfStallsSelector
  , observedBitrateSelector
  , observedBitrateStandardDeviationSelector
  , observedMaxBitrateSelector
  , observedMinBitrateSelector
  , playbackSessionIDSelector
  , playbackStartDateSelector
  , playbackStartOffsetSelector
  , playbackTypeSelector
  , segmentsDownloadedDurationSelector
  , serverAddressSelector
  , startupTimeSelector
  , switchBitrateSelector
  , transferDurationSelector
  , uriSelector


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
init_ :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id AVPlayerItemAccessLogEvent)
init_ avPlayerItemAccessLogEvent =
  sendOwnedMessage avPlayerItemAccessLogEvent initSelector

-- | @+ new@
new :: IO (Id AVPlayerItemAccessLogEvent)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemAccessLogEvent"
    sendOwnedClassMessage cls' newSelector

-- | A count of media segments downloaded.
--
-- Value is negative if unknown. A count of media segments downloaded from the server to this client. Corresponds to "sc-count". This property is not observable. This property is deprecated. Use numberOfMediaRequests instead.
--
-- ObjC selector: @- numberOfSegmentsDownloaded@
numberOfSegmentsDownloaded :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfSegmentsDownloaded avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent numberOfSegmentsDownloadedSelector

-- | A count of media read requests.
--
-- Value is negative if unknown. A count of media read requests from the server to this client. Corresponds to "sc-count". For HTTP live Streaming, a count of media segments downloaded from the server to this client. For progressive-style HTTP media downloads, a count of HTTP GET (byte-range) requests for the resource. This property is not observable.
--
-- ObjC selector: @- numberOfMediaRequests@
numberOfMediaRequests :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfMediaRequests avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent numberOfMediaRequestsSelector

-- | The date/time at which playback began for this event. Can be nil.
--
-- If nil is returned the date is unknown. Corresponds to "date". This property is not observable.
--
-- ObjC selector: @- playbackStartDate@
playbackStartDate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSDate)
playbackStartDate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent playbackStartDateSelector

-- | The URI of the playback item. Can be nil.
--
-- If nil is returned the URI is unknown. Corresponds to "uri". This property is not observable.
--
-- ObjC selector: @- URI@
uri :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSString)
uri avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent uriSelector

-- | The IP address of the server that was the source of the last delivered media segment. Can be nil.
--
-- If nil is returned the address is unknown. Can be either an IPv4 or IPv6 address. Corresponds to "s-ip". This property is not observable.
--
-- ObjC selector: @- serverAddress@
serverAddress :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSString)
serverAddress avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent serverAddressSelector

-- | A count of changes to the property serverAddress, see above, over the last uninterrupted period of playback.
--
-- Value is negative if unknown. Corresponds to "s-ip-changes". This property is not observable.
--
-- ObjC selector: @- numberOfServerAddressChanges@
numberOfServerAddressChanges :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfServerAddressChanges avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent numberOfServerAddressChangesSelector

-- | A GUID that identifies the playback session. This value is used in HTTP requests. Can be nil.
--
-- If nil is returned the GUID is unknown. Corresponds to "cs-guid". This property is not observable.
--
-- ObjC selector: @- playbackSessionID@
playbackSessionID :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSString)
playbackSessionID avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent playbackSessionIDSelector

-- | An offset into the playlist where the last uninterrupted period of playback began. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-start-time". This property is not observable.
--
-- ObjC selector: @- playbackStartOffset@
playbackStartOffset :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
playbackStartOffset avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent playbackStartOffsetSelector

-- | The accumulated duration of the media downloaded. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-duration-downloaded". This property is not observable.
--
-- ObjC selector: @- segmentsDownloadedDuration@
segmentsDownloadedDuration :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
segmentsDownloadedDuration avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent segmentsDownloadedDurationSelector

-- | The accumulated duration of the media played. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-duration-watched". This property is not observable.
--
-- ObjC selector: @- durationWatched@
durationWatched :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
durationWatched avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent durationWatchedSelector

-- | The total number of playback stalls encountered.
--
-- Value is negative if unknown. Corresponds to "c-stalls". This property is not observable.
--
-- ObjC selector: @- numberOfStalls@
numberOfStalls :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfStalls avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent numberOfStallsSelector

-- | The accumulated number of bytes transferred.
--
-- Value is negative if unknown. Corresponds to "bytes". This property is not observable.
--
-- ObjC selector: @- numberOfBytesTransferred@
numberOfBytesTransferred :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfBytesTransferred avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent numberOfBytesTransferredSelector

-- | The accumulated duration of active network transfer of bytes. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-transfer-duration". This property is not observable.
--
-- ObjC selector: @- transferDuration@
transferDuration :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
transferDuration avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent transferDurationSelector

-- | The empirical throughput across all media downloaded. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "c-observed-bitrate". This property is not observable.
--
-- ObjC selector: @- observedBitrate@
observedBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
observedBitrate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent observedBitrateSelector

-- | The throughput required to play the stream, as advertised by the server. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "sc-indicated-bitrate". This property is not observable.
--
-- ObjC selector: @- indicatedBitrate@
indicatedBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
indicatedBitrate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent indicatedBitrateSelector

-- | Average throughput required to play the stream, as advertised by the server. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "sc-indicated-avg-bitrate". This property is not observable.
--
-- ObjC selector: @- indicatedAverageBitrate@
indicatedAverageBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
indicatedAverageBitrate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent indicatedAverageBitrateSelector

-- | The average bitrate of video track if it is unmuxed. Average bitrate of combined content if muxed. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "c-avg-video-bitrate". This property is not observable.
--
-- ObjC selector: @- averageVideoBitrate@
averageVideoBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
averageVideoBitrate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent averageVideoBitrateSelector

-- | The average bitrate of audio track. This is not available if audio is muxed with video. Measured in bits per second.
--
-- Value is negative if unknown. Corresponds to "c-avg-audio-bitrate". This property is not observable.
--
-- ObjC selector: @- averageAudioBitrate@
averageAudioBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
averageAudioBitrate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent averageAudioBitrateSelector

-- | The total number of dropped video frames.
--
-- Value is negative if unknown. Corresponds to "c-frames-dropped". This property is not observable.
--
-- ObjC selector: @- numberOfDroppedVideoFrames@
numberOfDroppedVideoFrames :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
numberOfDroppedVideoFrames avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent numberOfDroppedVideoFramesSelector

-- | The accumulated duration until player item is ready to play. Measured in seconds.
--
-- Value is negative if unknown. Corresponds to "c-startup-time". This property is not observable.
--
-- ObjC selector: @- startupTime@
startupTime :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
startupTime avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent startupTimeSelector

-- | The total number of times the download of the segments took too long.
--
-- Value is negative if unknown. Corresponds to "c-overdue". This property is not observable.
--
-- ObjC selector: @- downloadOverdue@
downloadOverdue :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
downloadOverdue avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent downloadOverdueSelector

-- | Maximum observed segment download bit rate.
--
-- Value is negative if unknown. Corresponds to "c-observed-max-bitrate". This property is not observable.
--
-- ObjC selector: @- observedMaxBitrate@
observedMaxBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
observedMaxBitrate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent observedMaxBitrateSelector

-- | Minimum observed segment download bit rate.
--
-- Value is negative if unknown. Corresponds to "c-observed-min-bitrate". This property is not observable.
--
-- ObjC selector: @- observedMinBitrate@
observedMinBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
observedMinBitrate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent observedMinBitrateSelector

-- | Standard deviation of observed segment download bit rates.
--
-- Value is negative if unknown. Corresponds to "c-observed-bitrate-sd". This property is not observable.
--
-- ObjC selector: @- observedBitrateStandardDeviation@
observedBitrateStandardDeviation :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
observedBitrateStandardDeviation avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent observedBitrateStandardDeviationSelector

-- | Playback type (LIVE, VOD, FILE).
--
-- If nil is returned the playback type is unknown. Corresponds to "s-playback-type". This property is not observable.
--
-- ObjC selector: @- playbackType@
playbackType :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO (Id NSString)
playbackType avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent playbackTypeSelector

-- | Number of network read requests over WWAN.
--
-- Value is negative if unknown. Corresponds to "sc-wwan-count". This property is not observable.
--
-- ObjC selector: @- mediaRequestsWWAN@
mediaRequestsWWAN :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CLong
mediaRequestsWWAN avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent mediaRequestsWWANSelector

-- | Bandwidth that caused us to switch (up or down).
--
-- Value is negative if unknown. Corresponds to "c-switch-bitrate". This property is not observable.
--
-- ObjC selector: @- switchBitrate@
switchBitrate :: IsAVPlayerItemAccessLogEvent avPlayerItemAccessLogEvent => avPlayerItemAccessLogEvent -> IO CDouble
switchBitrate avPlayerItemAccessLogEvent =
  sendMessage avPlayerItemAccessLogEvent switchBitrateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerItemAccessLogEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerItemAccessLogEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @numberOfSegmentsDownloaded@
numberOfSegmentsDownloadedSelector :: Selector '[] CLong
numberOfSegmentsDownloadedSelector = mkSelector "numberOfSegmentsDownloaded"

-- | @Selector@ for @numberOfMediaRequests@
numberOfMediaRequestsSelector :: Selector '[] CLong
numberOfMediaRequestsSelector = mkSelector "numberOfMediaRequests"

-- | @Selector@ for @playbackStartDate@
playbackStartDateSelector :: Selector '[] (Id NSDate)
playbackStartDateSelector = mkSelector "playbackStartDate"

-- | @Selector@ for @URI@
uriSelector :: Selector '[] (Id NSString)
uriSelector = mkSelector "URI"

-- | @Selector@ for @serverAddress@
serverAddressSelector :: Selector '[] (Id NSString)
serverAddressSelector = mkSelector "serverAddress"

-- | @Selector@ for @numberOfServerAddressChanges@
numberOfServerAddressChangesSelector :: Selector '[] CLong
numberOfServerAddressChangesSelector = mkSelector "numberOfServerAddressChanges"

-- | @Selector@ for @playbackSessionID@
playbackSessionIDSelector :: Selector '[] (Id NSString)
playbackSessionIDSelector = mkSelector "playbackSessionID"

-- | @Selector@ for @playbackStartOffset@
playbackStartOffsetSelector :: Selector '[] CDouble
playbackStartOffsetSelector = mkSelector "playbackStartOffset"

-- | @Selector@ for @segmentsDownloadedDuration@
segmentsDownloadedDurationSelector :: Selector '[] CDouble
segmentsDownloadedDurationSelector = mkSelector "segmentsDownloadedDuration"

-- | @Selector@ for @durationWatched@
durationWatchedSelector :: Selector '[] CDouble
durationWatchedSelector = mkSelector "durationWatched"

-- | @Selector@ for @numberOfStalls@
numberOfStallsSelector :: Selector '[] CLong
numberOfStallsSelector = mkSelector "numberOfStalls"

-- | @Selector@ for @numberOfBytesTransferred@
numberOfBytesTransferredSelector :: Selector '[] CLong
numberOfBytesTransferredSelector = mkSelector "numberOfBytesTransferred"

-- | @Selector@ for @transferDuration@
transferDurationSelector :: Selector '[] CDouble
transferDurationSelector = mkSelector "transferDuration"

-- | @Selector@ for @observedBitrate@
observedBitrateSelector :: Selector '[] CDouble
observedBitrateSelector = mkSelector "observedBitrate"

-- | @Selector@ for @indicatedBitrate@
indicatedBitrateSelector :: Selector '[] CDouble
indicatedBitrateSelector = mkSelector "indicatedBitrate"

-- | @Selector@ for @indicatedAverageBitrate@
indicatedAverageBitrateSelector :: Selector '[] CDouble
indicatedAverageBitrateSelector = mkSelector "indicatedAverageBitrate"

-- | @Selector@ for @averageVideoBitrate@
averageVideoBitrateSelector :: Selector '[] CDouble
averageVideoBitrateSelector = mkSelector "averageVideoBitrate"

-- | @Selector@ for @averageAudioBitrate@
averageAudioBitrateSelector :: Selector '[] CDouble
averageAudioBitrateSelector = mkSelector "averageAudioBitrate"

-- | @Selector@ for @numberOfDroppedVideoFrames@
numberOfDroppedVideoFramesSelector :: Selector '[] CLong
numberOfDroppedVideoFramesSelector = mkSelector "numberOfDroppedVideoFrames"

-- | @Selector@ for @startupTime@
startupTimeSelector :: Selector '[] CDouble
startupTimeSelector = mkSelector "startupTime"

-- | @Selector@ for @downloadOverdue@
downloadOverdueSelector :: Selector '[] CLong
downloadOverdueSelector = mkSelector "downloadOverdue"

-- | @Selector@ for @observedMaxBitrate@
observedMaxBitrateSelector :: Selector '[] CDouble
observedMaxBitrateSelector = mkSelector "observedMaxBitrate"

-- | @Selector@ for @observedMinBitrate@
observedMinBitrateSelector :: Selector '[] CDouble
observedMinBitrateSelector = mkSelector "observedMinBitrate"

-- | @Selector@ for @observedBitrateStandardDeviation@
observedBitrateStandardDeviationSelector :: Selector '[] CDouble
observedBitrateStandardDeviationSelector = mkSelector "observedBitrateStandardDeviation"

-- | @Selector@ for @playbackType@
playbackTypeSelector :: Selector '[] (Id NSString)
playbackTypeSelector = mkSelector "playbackType"

-- | @Selector@ for @mediaRequestsWWAN@
mediaRequestsWWANSelector :: Selector '[] CLong
mediaRequestsWWANSelector = mkSelector "mediaRequestsWWAN"

-- | @Selector@ for @switchBitrate@
switchBitrateSelector :: Selector '[] CDouble
switchBitrateSelector = mkSelector "switchBitrate"

