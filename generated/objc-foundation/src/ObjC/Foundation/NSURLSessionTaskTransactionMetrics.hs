{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionTaskTransactionMetrics@.
module ObjC.Foundation.NSURLSessionTaskTransactionMetrics
  ( NSURLSessionTaskTransactionMetrics
  , IsNSURLSessionTaskTransactionMetrics(..)
  , init_
  , new
  , request
  , response
  , fetchStartDate
  , domainLookupStartDate
  , domainLookupEndDate
  , connectStartDate
  , secureConnectionStartDate
  , secureConnectionEndDate
  , connectEndDate
  , requestStartDate
  , requestEndDate
  , responseStartDate
  , responseEndDate
  , networkProtocolName
  , proxyConnection
  , reusedConnection
  , resourceFetchType
  , countOfRequestHeaderBytesSent
  , countOfRequestBodyBytesSent
  , countOfRequestBodyBytesBeforeEncoding
  , countOfResponseHeaderBytesReceived
  , countOfResponseBodyBytesReceived
  , countOfResponseBodyBytesAfterDecoding
  , localAddress
  , localPort
  , remoteAddress
  , remotePort
  , negotiatedTLSProtocolVersion
  , negotiatedTLSCipherSuite
  , cellular
  , expensive
  , constrained
  , multipath
  , domainResolutionProtocol
  , initSelector
  , newSelector
  , requestSelector
  , responseSelector
  , fetchStartDateSelector
  , domainLookupStartDateSelector
  , domainLookupEndDateSelector
  , connectStartDateSelector
  , secureConnectionStartDateSelector
  , secureConnectionEndDateSelector
  , connectEndDateSelector
  , requestStartDateSelector
  , requestEndDateSelector
  , responseStartDateSelector
  , responseEndDateSelector
  , networkProtocolNameSelector
  , proxyConnectionSelector
  , reusedConnectionSelector
  , resourceFetchTypeSelector
  , countOfRequestHeaderBytesSentSelector
  , countOfRequestBodyBytesSentSelector
  , countOfRequestBodyBytesBeforeEncodingSelector
  , countOfResponseHeaderBytesReceivedSelector
  , countOfResponseBodyBytesReceivedSelector
  , countOfResponseBodyBytesAfterDecodingSelector
  , localAddressSelector
  , localPortSelector
  , remoteAddressSelector
  , remotePortSelector
  , negotiatedTLSProtocolVersionSelector
  , negotiatedTLSCipherSuiteSelector
  , cellularSelector
  , expensiveSelector
  , constrainedSelector
  , multipathSelector
  , domainResolutionProtocolSelector

  -- * Enum types
  , NSURLSessionTaskMetricsDomainResolutionProtocol(NSURLSessionTaskMetricsDomainResolutionProtocol)
  , pattern NSURLSessionTaskMetricsDomainResolutionProtocolUnknown
  , pattern NSURLSessionTaskMetricsDomainResolutionProtocolUDP
  , pattern NSURLSessionTaskMetricsDomainResolutionProtocolTCP
  , pattern NSURLSessionTaskMetricsDomainResolutionProtocolTLS
  , pattern NSURLSessionTaskMetricsDomainResolutionProtocolHTTPS
  , NSURLSessionTaskMetricsResourceFetchType(NSURLSessionTaskMetricsResourceFetchType)
  , pattern NSURLSessionTaskMetricsResourceFetchTypeUnknown
  , pattern NSURLSessionTaskMetricsResourceFetchTypeNetworkLoad
  , pattern NSURLSessionTaskMetricsResourceFetchTypeServerPush
  , pattern NSURLSessionTaskMetricsResourceFetchTypeLocalCache

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSURLSessionTaskTransactionMetrics)
init_ nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionTaskTransactionMetrics)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionTaskTransactionMetrics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- request@
request :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSURLRequest)
request nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "request") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- response@
response :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSURLResponse)
response nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "response") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fetchStartDate@
fetchStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
fetchStartDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "fetchStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- domainLookupStartDate@
domainLookupStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
domainLookupStartDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "domainLookupStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- domainLookupEndDate@
domainLookupEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
domainLookupEndDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "domainLookupEndDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- connectStartDate@
connectStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
connectStartDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "connectStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- secureConnectionStartDate@
secureConnectionStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
secureConnectionStartDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "secureConnectionStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- secureConnectionEndDate@
secureConnectionEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
secureConnectionEndDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "secureConnectionEndDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- connectEndDate@
connectEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
connectEndDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "connectEndDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requestStartDate@
requestStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
requestStartDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "requestStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requestEndDate@
requestEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
requestEndDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "requestEndDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- responseStartDate@
responseStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
responseStartDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "responseStartDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- responseEndDate@
responseEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
responseEndDate nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "responseEndDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- networkProtocolName@
networkProtocolName :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSString)
networkProtocolName nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "networkProtocolName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- proxyConnection@
proxyConnection :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
proxyConnection nsurlSessionTaskTransactionMetrics  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "proxyConnection") retCULong []

-- | @- reusedConnection@
reusedConnection :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
reusedConnection nsurlSessionTaskTransactionMetrics  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "reusedConnection") retCULong []

-- | @- resourceFetchType@
resourceFetchType :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO NSURLSessionTaskMetricsResourceFetchType
resourceFetchType nsurlSessionTaskTransactionMetrics  =
  fmap (coerce :: CLong -> NSURLSessionTaskMetricsResourceFetchType) $ sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "resourceFetchType") retCLong []

-- | @- countOfRequestHeaderBytesSent@
countOfRequestHeaderBytesSent :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfRequestHeaderBytesSent nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "countOfRequestHeaderBytesSent") retCLong []

-- | @- countOfRequestBodyBytesSent@
countOfRequestBodyBytesSent :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfRequestBodyBytesSent nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "countOfRequestBodyBytesSent") retCLong []

-- | @- countOfRequestBodyBytesBeforeEncoding@
countOfRequestBodyBytesBeforeEncoding :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfRequestBodyBytesBeforeEncoding nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "countOfRequestBodyBytesBeforeEncoding") retCLong []

-- | @- countOfResponseHeaderBytesReceived@
countOfResponseHeaderBytesReceived :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfResponseHeaderBytesReceived nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "countOfResponseHeaderBytesReceived") retCLong []

-- | @- countOfResponseBodyBytesReceived@
countOfResponseBodyBytesReceived :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfResponseBodyBytesReceived nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "countOfResponseBodyBytesReceived") retCLong []

-- | @- countOfResponseBodyBytesAfterDecoding@
countOfResponseBodyBytesAfterDecoding :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfResponseBodyBytesAfterDecoding nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "countOfResponseBodyBytesAfterDecoding") retCLong []

-- | @- localAddress@
localAddress :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSString)
localAddress nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "localAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localPort@
localPort :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSNumber)
localPort nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "localPort") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- remoteAddress@
remoteAddress :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSString)
remoteAddress nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "remoteAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- remotePort@
remotePort :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSNumber)
remotePort nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "remotePort") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- negotiatedTLSProtocolVersion@
negotiatedTLSProtocolVersion :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSNumber)
negotiatedTLSProtocolVersion nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "negotiatedTLSProtocolVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- negotiatedTLSCipherSuite@
negotiatedTLSCipherSuite :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSNumber)
negotiatedTLSCipherSuite nsurlSessionTaskTransactionMetrics  =
  sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "negotiatedTLSCipherSuite") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cellular@
cellular :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
cellular nsurlSessionTaskTransactionMetrics  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "cellular") retCULong []

-- | @- expensive@
expensive :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
expensive nsurlSessionTaskTransactionMetrics  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "expensive") retCULong []

-- | @- constrained@
constrained :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
constrained nsurlSessionTaskTransactionMetrics  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "constrained") retCULong []

-- | @- multipath@
multipath :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
multipath nsurlSessionTaskTransactionMetrics  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "multipath") retCULong []

-- | @- domainResolutionProtocol@
domainResolutionProtocol :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO NSURLSessionTaskMetricsDomainResolutionProtocol
domainResolutionProtocol nsurlSessionTaskTransactionMetrics  =
  fmap (coerce :: CLong -> NSURLSessionTaskMetricsDomainResolutionProtocol) $ sendMsg nsurlSessionTaskTransactionMetrics (mkSelector "domainResolutionProtocol") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @request@
requestSelector :: Selector
requestSelector = mkSelector "request"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

-- | @Selector@ for @fetchStartDate@
fetchStartDateSelector :: Selector
fetchStartDateSelector = mkSelector "fetchStartDate"

-- | @Selector@ for @domainLookupStartDate@
domainLookupStartDateSelector :: Selector
domainLookupStartDateSelector = mkSelector "domainLookupStartDate"

-- | @Selector@ for @domainLookupEndDate@
domainLookupEndDateSelector :: Selector
domainLookupEndDateSelector = mkSelector "domainLookupEndDate"

-- | @Selector@ for @connectStartDate@
connectStartDateSelector :: Selector
connectStartDateSelector = mkSelector "connectStartDate"

-- | @Selector@ for @secureConnectionStartDate@
secureConnectionStartDateSelector :: Selector
secureConnectionStartDateSelector = mkSelector "secureConnectionStartDate"

-- | @Selector@ for @secureConnectionEndDate@
secureConnectionEndDateSelector :: Selector
secureConnectionEndDateSelector = mkSelector "secureConnectionEndDate"

-- | @Selector@ for @connectEndDate@
connectEndDateSelector :: Selector
connectEndDateSelector = mkSelector "connectEndDate"

-- | @Selector@ for @requestStartDate@
requestStartDateSelector :: Selector
requestStartDateSelector = mkSelector "requestStartDate"

-- | @Selector@ for @requestEndDate@
requestEndDateSelector :: Selector
requestEndDateSelector = mkSelector "requestEndDate"

-- | @Selector@ for @responseStartDate@
responseStartDateSelector :: Selector
responseStartDateSelector = mkSelector "responseStartDate"

-- | @Selector@ for @responseEndDate@
responseEndDateSelector :: Selector
responseEndDateSelector = mkSelector "responseEndDate"

-- | @Selector@ for @networkProtocolName@
networkProtocolNameSelector :: Selector
networkProtocolNameSelector = mkSelector "networkProtocolName"

-- | @Selector@ for @proxyConnection@
proxyConnectionSelector :: Selector
proxyConnectionSelector = mkSelector "proxyConnection"

-- | @Selector@ for @reusedConnection@
reusedConnectionSelector :: Selector
reusedConnectionSelector = mkSelector "reusedConnection"

-- | @Selector@ for @resourceFetchType@
resourceFetchTypeSelector :: Selector
resourceFetchTypeSelector = mkSelector "resourceFetchType"

-- | @Selector@ for @countOfRequestHeaderBytesSent@
countOfRequestHeaderBytesSentSelector :: Selector
countOfRequestHeaderBytesSentSelector = mkSelector "countOfRequestHeaderBytesSent"

-- | @Selector@ for @countOfRequestBodyBytesSent@
countOfRequestBodyBytesSentSelector :: Selector
countOfRequestBodyBytesSentSelector = mkSelector "countOfRequestBodyBytesSent"

-- | @Selector@ for @countOfRequestBodyBytesBeforeEncoding@
countOfRequestBodyBytesBeforeEncodingSelector :: Selector
countOfRequestBodyBytesBeforeEncodingSelector = mkSelector "countOfRequestBodyBytesBeforeEncoding"

-- | @Selector@ for @countOfResponseHeaderBytesReceived@
countOfResponseHeaderBytesReceivedSelector :: Selector
countOfResponseHeaderBytesReceivedSelector = mkSelector "countOfResponseHeaderBytesReceived"

-- | @Selector@ for @countOfResponseBodyBytesReceived@
countOfResponseBodyBytesReceivedSelector :: Selector
countOfResponseBodyBytesReceivedSelector = mkSelector "countOfResponseBodyBytesReceived"

-- | @Selector@ for @countOfResponseBodyBytesAfterDecoding@
countOfResponseBodyBytesAfterDecodingSelector :: Selector
countOfResponseBodyBytesAfterDecodingSelector = mkSelector "countOfResponseBodyBytesAfterDecoding"

-- | @Selector@ for @localAddress@
localAddressSelector :: Selector
localAddressSelector = mkSelector "localAddress"

-- | @Selector@ for @localPort@
localPortSelector :: Selector
localPortSelector = mkSelector "localPort"

-- | @Selector@ for @remoteAddress@
remoteAddressSelector :: Selector
remoteAddressSelector = mkSelector "remoteAddress"

-- | @Selector@ for @remotePort@
remotePortSelector :: Selector
remotePortSelector = mkSelector "remotePort"

-- | @Selector@ for @negotiatedTLSProtocolVersion@
negotiatedTLSProtocolVersionSelector :: Selector
negotiatedTLSProtocolVersionSelector = mkSelector "negotiatedTLSProtocolVersion"

-- | @Selector@ for @negotiatedTLSCipherSuite@
negotiatedTLSCipherSuiteSelector :: Selector
negotiatedTLSCipherSuiteSelector = mkSelector "negotiatedTLSCipherSuite"

-- | @Selector@ for @cellular@
cellularSelector :: Selector
cellularSelector = mkSelector "cellular"

-- | @Selector@ for @expensive@
expensiveSelector :: Selector
expensiveSelector = mkSelector "expensive"

-- | @Selector@ for @constrained@
constrainedSelector :: Selector
constrainedSelector = mkSelector "constrained"

-- | @Selector@ for @multipath@
multipathSelector :: Selector
multipathSelector = mkSelector "multipath"

-- | @Selector@ for @domainResolutionProtocol@
domainResolutionProtocolSelector :: Selector
domainResolutionProtocolSelector = mkSelector "domainResolutionProtocol"

