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
  , initSelector
  , newSelector
  , urlSelector
  , serverAddressSelector
  , requestStartTimeSelector
  , requestEndTimeSelector
  , responseStartTimeSelector
  , responseEndTimeSelector
  , byteRangeSelector
  , readFromCacheSelector
  , errorEventSelector
  , networkTransactionMetricsSelector


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
init_ :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id AVMetricMediaResourceRequestEvent)
init_ avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricMediaResourceRequestEvent)
new  =
  do
    cls' <- getRequiredClass "AVMetricMediaResourceRequestEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the URL of the resource request. If no value is available, returns nil.
--
-- ObjC selector: @- url@
url :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSURL)
url avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The IP address of the server. If not available, the value is nil.
--
-- ObjC selector: @- serverAddress@
serverAddress :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSString)
serverAddress avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "serverAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the start time of the resource request.
--
-- ObjC selector: @- requestStartTime@
requestStartTime :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSDate)
requestStartTime avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "requestStartTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the end time of the resource request.
--
-- ObjC selector: @- requestEndTime@
requestEndTime :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSDate)
requestEndTime avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "requestEndTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the start time of the resource request response.
--
-- ObjC selector: @- responseStartTime@
responseStartTime :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSDate)
responseStartTime avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "responseStartTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the end time of the resource request response.
--
-- ObjC selector: @- responseEndTime@
responseEndTime :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSDate)
responseEndTime avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "responseEndTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the byte range downloaded for the resource request. If not available, the range start and end will be 0.
--
-- ObjC selector: @- byteRange@
byteRange :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO NSRange
byteRange avMetricMediaResourceRequestEvent  =
  sendMsgStret avMetricMediaResourceRequestEvent (mkSelector "byteRange") retNSRange []

-- | Returns true if the resource was read from the cache.
--
-- ObjC selector: @- readFromCache@
readFromCache :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO Bool
readFromCache avMetricMediaResourceRequestEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricMediaResourceRequestEvent (mkSelector "readFromCache") retCULong []

-- | Returns the error event, if any, encountered during the resource request. If no value is present, returns nil.
--
-- ObjC selector: @- errorEvent@
errorEvent :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id AVMetricErrorEvent)
errorEvent avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "errorEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the NSURLSessionTaskMetrics associated with the resource request. If no value is present, returns nil
--
-- ObjC selector: @- networkTransactionMetrics@
networkTransactionMetrics :: IsAVMetricMediaResourceRequestEvent avMetricMediaResourceRequestEvent => avMetricMediaResourceRequestEvent -> IO (Id NSURLSessionTaskMetrics)
networkTransactionMetrics avMetricMediaResourceRequestEvent  =
  sendMsg avMetricMediaResourceRequestEvent (mkSelector "networkTransactionMetrics") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @serverAddress@
serverAddressSelector :: Selector
serverAddressSelector = mkSelector "serverAddress"

-- | @Selector@ for @requestStartTime@
requestStartTimeSelector :: Selector
requestStartTimeSelector = mkSelector "requestStartTime"

-- | @Selector@ for @requestEndTime@
requestEndTimeSelector :: Selector
requestEndTimeSelector = mkSelector "requestEndTime"

-- | @Selector@ for @responseStartTime@
responseStartTimeSelector :: Selector
responseStartTimeSelector = mkSelector "responseStartTime"

-- | @Selector@ for @responseEndTime@
responseEndTimeSelector :: Selector
responseEndTimeSelector = mkSelector "responseEndTime"

-- | @Selector@ for @byteRange@
byteRangeSelector :: Selector
byteRangeSelector = mkSelector "byteRange"

-- | @Selector@ for @readFromCache@
readFromCacheSelector :: Selector
readFromCacheSelector = mkSelector "readFromCache"

-- | @Selector@ for @errorEvent@
errorEventSelector :: Selector
errorEventSelector = mkSelector "errorEvent"

-- | @Selector@ for @networkTransactionMetrics@
networkTransactionMetricsSelector :: Selector
networkTransactionMetricsSelector = mkSelector "networkTransactionMetrics"

