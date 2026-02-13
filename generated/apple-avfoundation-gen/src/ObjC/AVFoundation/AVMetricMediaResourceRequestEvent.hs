{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a metric event associated with media resource requests.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMetricMediaResourceRequestEvent@.
module ObjC.AVFoundation.AVMetricMediaResourceRequestEvent
  ( AVMetricMediaResourceRequestEvent
  , IsAVMetricMediaResourceRequestEvent(..)
  , init_
  , new
  , url
  , serverAddress
  , requestStartTime
  , requestEndTime
  , responseStartTime
  , responseEndTime
  , byteRange
  , readFromCache
  , errorEvent
  , networkTransactionMetrics
  , byteRangeSelector
  , errorEventSelector
  , initSelector
  , networkTransactionMetricsSelector
  , newSelector
  , readFromCacheSelector
  , requestEndTimeSelector
  , requestStartTimeSelector
  , responseEndTimeSelector
  , responseStartTimeSelector
  , serverAddressSelector
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
init_ :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id AVMetricMediaResourceRequestEvent)
init_ avMetricMediaResourceRequestEvent =
  sendOwnedMessage avMetricMediaResourceRequestEvent initSelector

-- | @+ new@
new :: IO (Id AVMetricMediaResourceRequestEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricMediaResourceRequestEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns the URL of the resource request. If no value is available, returns nil.
--
-- ObjC selector: @- url@
url :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSURL)
url avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent urlSelector

-- | The IP address of the server. If not available, the value is nil.
--
-- ObjC selector: @- serverAddress@
serverAddress :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSString)
serverAddress avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent serverAddressSelector

-- | Returns the start time of the resource request.
--
-- ObjC selector: @- requestStartTime@
requestStartTime :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSDate)
requestStartTime avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent requestStartTimeSelector

-- | Returns the end time of the resource request.
--
-- ObjC selector: @- requestEndTime@
requestEndTime :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSDate)
requestEndTime avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent requestEndTimeSelector

-- | Returns the start time of the resource request response.
--
-- ObjC selector: @- responseStartTime@
responseStartTime :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSDate)
responseStartTime avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent responseStartTimeSelector

-- | Returns the end time of the resource request response.
--
-- ObjC selector: @- responseEndTime@
responseEndTime :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSDate)
responseEndTime avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent responseEndTimeSelector

-- | Returns the byte range downloaded for the resource request. If not available, the range start and end will be 0.
--
-- ObjC selector: @- byteRange@
byteRange :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO NSRange
byteRange avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent byteRangeSelector

-- | Returns true if the resource was read from the cache.
--
-- ObjC selector: @- readFromCache@
readFromCache :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO Bool
readFromCache avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent readFromCacheSelector

-- | Returns the error event, if any, encountered during the resource request. If no value is present, returns nil.
--
-- ObjC selector: @- errorEvent@
errorEvent :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id AVMetricErrorEvent)
errorEvent avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent errorEventSelector

-- | Returns the NSURLSessionTaskMetrics associated with the resource request. If no value is present, returns nil
--
-- ObjC selector: @- networkTransactionMetrics@
networkTransactionMetrics :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSURLSessionTaskMetrics)
networkTransactionMetrics avMetricMediaResourceRequestEvent =
  sendMessage avMetricMediaResourceRequestEvent networkTransactionMetricsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricMediaResourceRequestEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricMediaResourceRequestEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @serverAddress@
serverAddressSelector :: Selector '[] (Id NSString)
serverAddressSelector = mkSelector "serverAddress"

-- | @Selector@ for @requestStartTime@
requestStartTimeSelector :: Selector '[] (Id NSDate)
requestStartTimeSelector = mkSelector "requestStartTime"

-- | @Selector@ for @requestEndTime@
requestEndTimeSelector :: Selector '[] (Id NSDate)
requestEndTimeSelector = mkSelector "requestEndTime"

-- | @Selector@ for @responseStartTime@
responseStartTimeSelector :: Selector '[] (Id NSDate)
responseStartTimeSelector = mkSelector "responseStartTime"

-- | @Selector@ for @responseEndTime@
responseEndTimeSelector :: Selector '[] (Id NSDate)
responseEndTimeSelector = mkSelector "responseEndTime"

-- | @Selector@ for @byteRange@
byteRangeSelector :: Selector '[] NSRange
byteRangeSelector = mkSelector "byteRange"

-- | @Selector@ for @readFromCache@
readFromCacheSelector :: Selector '[] Bool
readFromCacheSelector = mkSelector "readFromCache"

-- | @Selector@ for @errorEvent@
errorEventSelector :: Selector '[] (Id AVMetricErrorEvent)
errorEventSelector = mkSelector "errorEvent"

-- | @Selector@ for @networkTransactionMetrics@
networkTransactionMetricsSelector :: Selector '[] (Id NSURLSessionTaskMetrics)
networkTransactionMetricsSelector = mkSelector "networkTransactionMetrics"

