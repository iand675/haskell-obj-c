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
  , initSelector
  , newSelector
  , errorEventSelector
  , recoverableErrorCountSelector
  , mediaResourceRequestCountSelector
  , bytesDownloadedCountSelector
  , downloadDurationSelector
  , variantsSelector


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
init_ :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO (Id AVMetricDownloadSummaryEvent)
init_ avMetricDownloadSummaryEvent  =
  sendMsg avMetricDownloadSummaryEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricDownloadSummaryEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricDownloadSummaryEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the error event if any. If no value is available, returns nil.
--
-- ObjC selector: @- errorEvent@
errorEvent :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO (Id AVMetricErrorEvent)
errorEvent avMetricDownloadSummaryEvent  =
  sendMsg avMetricDownloadSummaryEvent (mkSelector "errorEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the total count of recoverable errors encountered during the download. If no errors were encountered, returns 0.
--
-- Error counts may not be consistent across OS versions. Comparisons should be made within a given OS version, as error reporting is subject to change with OS updates.
--
-- ObjC selector: @- recoverableErrorCount@
recoverableErrorCount :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO CLong
recoverableErrorCount avMetricDownloadSummaryEvent  =
  sendMsg avMetricDownloadSummaryEvent (mkSelector "recoverableErrorCount") retCLong []

-- | Returns the total number of media requests performed by the download task. This includes playlist requests, media segment requests, and content key requests.
--
-- ObjC selector: @- mediaResourceRequestCount@
mediaResourceRequestCount :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO CLong
mediaResourceRequestCount avMetricDownloadSummaryEvent  =
  sendMsg avMetricDownloadSummaryEvent (mkSelector "mediaResourceRequestCount") retCLong []

-- | Returns the total number of bytes downloaded by the download task.
--
-- ObjC selector: @- bytesDownloadedCount@
bytesDownloadedCount :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO CLong
bytesDownloadedCount avMetricDownloadSummaryEvent  =
  sendMsg avMetricDownloadSummaryEvent (mkSelector "bytesDownloadedCount") retCLong []

-- | Returns the total duration of the download in seconds.
--
-- ObjC selector: @- downloadDuration@
downloadDuration :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO CDouble
downloadDuration avMetricDownloadSummaryEvent  =
  sendMsg avMetricDownloadSummaryEvent (mkSelector "downloadDuration") retCDouble []

-- | Returns the variants that were downloaded.
--
-- ObjC selector: @- variants@
variants :: IsAVMetricDownloadSummaryEvent avMetricDownloadSummaryEvent => avMetricDownloadSummaryEvent -> IO (Id NSArray)
variants avMetricDownloadSummaryEvent  =
  sendMsg avMetricDownloadSummaryEvent (mkSelector "variants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @errorEvent@
errorEventSelector :: Selector
errorEventSelector = mkSelector "errorEvent"

-- | @Selector@ for @recoverableErrorCount@
recoverableErrorCountSelector :: Selector
recoverableErrorCountSelector = mkSelector "recoverableErrorCount"

-- | @Selector@ for @mediaResourceRequestCount@
mediaResourceRequestCountSelector :: Selector
mediaResourceRequestCountSelector = mkSelector "mediaResourceRequestCount"

-- | @Selector@ for @bytesDownloadedCount@
bytesDownloadedCountSelector :: Selector
bytesDownloadedCountSelector = mkSelector "bytesDownloadedCount"

-- | @Selector@ for @downloadDuration@
downloadDurationSelector :: Selector
downloadDurationSelector = mkSelector "downloadDuration"

-- | @Selector@ for @variants@
variantsSelector :: Selector
variantsSelector = mkSelector "variants"

