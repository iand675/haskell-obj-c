{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a summary metric event with aggregated metrics for the entire download task.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricDownloadSummaryEvent@.
module ObjC.AVFoundation.AVMetricDownloadSummaryEvent
  ( AVMetricDownloadSummaryEvent
  , IsAVMetricDownloadSummaryEvent(..)
  , init_
  , new
  , errorEvent
  , recoverableErrorCount
  , mediaResourceRequestCount
  , bytesDownloadedCount
  , downloadDuration
  , variants
  , bytesDownloadedCountSelector
  , downloadDurationSelector
  , errorEventSelector
  , initSelector
  , mediaResourceRequestCountSelector
  , newSelector
  , recoverableErrorCountSelector
  , variantsSelector


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
init_ :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO (Id AVMetricDownloadSummaryEvent)
init_ avMetricDownloadSummaryEvent =
  sendOwnedMessage avMetricDownloadSummaryEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricDownloadSummaryEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricDownloadSummaryEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the error event if any. If no value is available, returns nil.
--
-- ObjC selector: @- errorEvent@
errorEvent :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO (Id AVMetricErrorEvent)
errorEvent avMetricDownloadSummaryEvent =
  sendMessage avMetricDownloadSummaryEvent errorEventSelector

-- | Returns the total count of recoverable errors encountered during the download. If no errors were encountered, returns 0.
--
-- Error counts may not be consistent across OS versions. Comparisons should be made within a given OS version, as error reporting is subject to change with OS updates.
--
-- ObjC selector: @- recoverableErrorCount@
recoverableErrorCount :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO CLong
recoverableErrorCount avMetricDownloadSummaryEvent =
  sendMessage avMetricDownloadSummaryEvent recoverableErrorCountSelector

-- | Returns the total number of media requests performed by the download task. This includes playlist requests, media segment requests, and content key requests.
--
-- ObjC selector: @- mediaResourceRequestCount@
mediaResourceRequestCount :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO CLong
mediaResourceRequestCount avMetricDownloadSummaryEvent =
  sendMessage avMetricDownloadSummaryEvent mediaResourceRequestCountSelector

-- | Returns the total number of bytes downloaded by the download task.
--
-- ObjC selector: @- bytesDownloadedCount@
bytesDownloadedCount :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO CLong
bytesDownloadedCount avMetricDownloadSummaryEvent =
  sendMessage avMetricDownloadSummaryEvent bytesDownloadedCountSelector

-- | Returns the total duration of the download in seconds.
--
-- ObjC selector: @- downloadDuration@
downloadDuration :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO CDouble
downloadDuration avMetricDownloadSummaryEvent =
  sendMessage avMetricDownloadSummaryEvent downloadDurationSelector

-- | Returns the variants that were downloaded.
--
-- ObjC selector: @- variants@
variants :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO (Id NSArray)
variants avMetricDownloadSummaryEvent =
  sendMessage avMetricDownloadSummaryEvent variantsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricDownloadSummaryEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricDownloadSummaryEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @errorEvent@
errorEventSelector :: Selector '[] (Id AVMetricErrorEvent)
errorEventSelector = mkSelector "errorEvent"

-- | @Selector@ for @recoverableErrorCount@
recoverableErrorCountSelector :: Selector '[] CLong
recoverableErrorCountSelector = mkSelector "recoverableErrorCount"

-- | @Selector@ for @mediaResourceRequestCount@
mediaResourceRequestCountSelector :: Selector '[] CLong
mediaResourceRequestCountSelector = mkSelector "mediaResourceRequestCount"

-- | @Selector@ for @bytesDownloadedCount@
bytesDownloadedCountSelector :: Selector '[] CLong
bytesDownloadedCountSelector = mkSelector "bytesDownloadedCount"

-- | @Selector@ for @downloadDuration@
downloadDurationSelector :: Selector '[] CDouble
downloadDurationSelector = mkSelector "downloadDuration"

-- | @Selector@ for @variants@
variantsSelector :: Selector '[] (Id NSArray)
variantsSelector = mkSelector "variants"

