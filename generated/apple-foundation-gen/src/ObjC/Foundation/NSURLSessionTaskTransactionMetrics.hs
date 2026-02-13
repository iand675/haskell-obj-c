{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , cellularSelector
  , connectEndDateSelector
  , connectStartDateSelector
  , constrainedSelector
  , countOfRequestBodyBytesBeforeEncodingSelector
  , countOfRequestBodyBytesSentSelector
  , countOfRequestHeaderBytesSentSelector
  , countOfResponseBodyBytesAfterDecodingSelector
  , countOfResponseBodyBytesReceivedSelector
  , countOfResponseHeaderBytesReceivedSelector
  , domainLookupEndDateSelector
  , domainLookupStartDateSelector
  , domainResolutionProtocolSelector
  , expensiveSelector
  , fetchStartDateSelector
  , initSelector
  , localAddressSelector
  , localPortSelector
  , multipathSelector
  , negotiatedTLSCipherSuiteSelector
  , negotiatedTLSProtocolVersionSelector
  , networkProtocolNameSelector
  , newSelector
  , proxyConnectionSelector
  , remoteAddressSelector
  , remotePortSelector
  , requestEndDateSelector
  , requestSelector
  , requestStartDateSelector
  , resourceFetchTypeSelector
  , responseEndDateSelector
  , responseSelector
  , responseStartDateSelector
  , reusedConnectionSelector
  , secureConnectionEndDateSelector
  , secureConnectionStartDateSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSURLSessionTaskTransactionMetrics)
init_ nsurlSessionTaskTransactionMetrics =
  sendOwnedMessage nsurlSessionTaskTransactionMetrics initSelector

-- | @+ new@
new :: IO (Id NSURLSessionTaskTransactionMetrics)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionTaskTransactionMetrics"
    sendOwnedClassMessage cls' newSelector

-- | @- request@
request :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSURLRequest)
request nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics requestSelector

-- | @- response@
response :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSURLResponse)
response nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics responseSelector

-- | @- fetchStartDate@
fetchStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
fetchStartDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics fetchStartDateSelector

-- | @- domainLookupStartDate@
domainLookupStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
domainLookupStartDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics domainLookupStartDateSelector

-- | @- domainLookupEndDate@
domainLookupEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
domainLookupEndDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics domainLookupEndDateSelector

-- | @- connectStartDate@
connectStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
connectStartDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics connectStartDateSelector

-- | @- secureConnectionStartDate@
secureConnectionStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
secureConnectionStartDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics secureConnectionStartDateSelector

-- | @- secureConnectionEndDate@
secureConnectionEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
secureConnectionEndDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics secureConnectionEndDateSelector

-- | @- connectEndDate@
connectEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
connectEndDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics connectEndDateSelector

-- | @- requestStartDate@
requestStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
requestStartDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics requestStartDateSelector

-- | @- requestEndDate@
requestEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
requestEndDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics requestEndDateSelector

-- | @- responseStartDate@
responseStartDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
responseStartDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics responseStartDateSelector

-- | @- responseEndDate@
responseEndDate :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSDate)
responseEndDate nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics responseEndDateSelector

-- | @- networkProtocolName@
networkProtocolName :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSString)
networkProtocolName nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics networkProtocolNameSelector

-- | @- proxyConnection@
proxyConnection :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
proxyConnection nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics proxyConnectionSelector

-- | @- reusedConnection@
reusedConnection :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
reusedConnection nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics reusedConnectionSelector

-- | @- resourceFetchType@
resourceFetchType :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO NSURLSessionTaskMetricsResourceFetchType
resourceFetchType nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics resourceFetchTypeSelector

-- | @- countOfRequestHeaderBytesSent@
countOfRequestHeaderBytesSent :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfRequestHeaderBytesSent nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics countOfRequestHeaderBytesSentSelector

-- | @- countOfRequestBodyBytesSent@
countOfRequestBodyBytesSent :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfRequestBodyBytesSent nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics countOfRequestBodyBytesSentSelector

-- | @- countOfRequestBodyBytesBeforeEncoding@
countOfRequestBodyBytesBeforeEncoding :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfRequestBodyBytesBeforeEncoding nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics countOfRequestBodyBytesBeforeEncodingSelector

-- | @- countOfResponseHeaderBytesReceived@
countOfResponseHeaderBytesReceived :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfResponseHeaderBytesReceived nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics countOfResponseHeaderBytesReceivedSelector

-- | @- countOfResponseBodyBytesReceived@
countOfResponseBodyBytesReceived :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfResponseBodyBytesReceived nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics countOfResponseBodyBytesReceivedSelector

-- | @- countOfResponseBodyBytesAfterDecoding@
countOfResponseBodyBytesAfterDecoding :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO CLong
countOfResponseBodyBytesAfterDecoding nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics countOfResponseBodyBytesAfterDecodingSelector

-- | @- localAddress@
localAddress :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSString)
localAddress nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics localAddressSelector

-- | @- localPort@
localPort :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSNumber)
localPort nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics localPortSelector

-- | @- remoteAddress@
remoteAddress :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSString)
remoteAddress nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics remoteAddressSelector

-- | @- remotePort@
remotePort :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSNumber)
remotePort nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics remotePortSelector

-- | @- negotiatedTLSProtocolVersion@
negotiatedTLSProtocolVersion :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSNumber)
negotiatedTLSProtocolVersion nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics negotiatedTLSProtocolVersionSelector

-- | @- negotiatedTLSCipherSuite@
negotiatedTLSCipherSuite :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO (Id NSNumber)
negotiatedTLSCipherSuite nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics negotiatedTLSCipherSuiteSelector

-- | @- cellular@
cellular :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
cellular nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics cellularSelector

-- | @- expensive@
expensive :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
expensive nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics expensiveSelector

-- | @- constrained@
constrained :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
constrained nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics constrainedSelector

-- | @- multipath@
multipath :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO Bool
multipath nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics multipathSelector

-- | @- domainResolutionProtocol@
domainResolutionProtocol :: IsNSURLSessionTaskTransactionMetrics nsurlSessionTaskTransactionMetrics => nsurlSessionTaskTransactionMetrics -> IO NSURLSessionTaskMetricsDomainResolutionProtocol
domainResolutionProtocol nsurlSessionTaskTransactionMetrics =
  sendMessage nsurlSessionTaskTransactionMetrics domainResolutionProtocolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionTaskTransactionMetrics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionTaskTransactionMetrics)
newSelector = mkSelector "new"

-- | @Selector@ for @request@
requestSelector :: Selector '[] (Id NSURLRequest)
requestSelector = mkSelector "request"

-- | @Selector@ for @response@
responseSelector :: Selector '[] (Id NSURLResponse)
responseSelector = mkSelector "response"

-- | @Selector@ for @fetchStartDate@
fetchStartDateSelector :: Selector '[] (Id NSDate)
fetchStartDateSelector = mkSelector "fetchStartDate"

-- | @Selector@ for @domainLookupStartDate@
domainLookupStartDateSelector :: Selector '[] (Id NSDate)
domainLookupStartDateSelector = mkSelector "domainLookupStartDate"

-- | @Selector@ for @domainLookupEndDate@
domainLookupEndDateSelector :: Selector '[] (Id NSDate)
domainLookupEndDateSelector = mkSelector "domainLookupEndDate"

-- | @Selector@ for @connectStartDate@
connectStartDateSelector :: Selector '[] (Id NSDate)
connectStartDateSelector = mkSelector "connectStartDate"

-- | @Selector@ for @secureConnectionStartDate@
secureConnectionStartDateSelector :: Selector '[] (Id NSDate)
secureConnectionStartDateSelector = mkSelector "secureConnectionStartDate"

-- | @Selector@ for @secureConnectionEndDate@
secureConnectionEndDateSelector :: Selector '[] (Id NSDate)
secureConnectionEndDateSelector = mkSelector "secureConnectionEndDate"

-- | @Selector@ for @connectEndDate@
connectEndDateSelector :: Selector '[] (Id NSDate)
connectEndDateSelector = mkSelector "connectEndDate"

-- | @Selector@ for @requestStartDate@
requestStartDateSelector :: Selector '[] (Id NSDate)
requestStartDateSelector = mkSelector "requestStartDate"

-- | @Selector@ for @requestEndDate@
requestEndDateSelector :: Selector '[] (Id NSDate)
requestEndDateSelector = mkSelector "requestEndDate"

-- | @Selector@ for @responseStartDate@
responseStartDateSelector :: Selector '[] (Id NSDate)
responseStartDateSelector = mkSelector "responseStartDate"

-- | @Selector@ for @responseEndDate@
responseEndDateSelector :: Selector '[] (Id NSDate)
responseEndDateSelector = mkSelector "responseEndDate"

-- | @Selector@ for @networkProtocolName@
networkProtocolNameSelector :: Selector '[] (Id NSString)
networkProtocolNameSelector = mkSelector "networkProtocolName"

-- | @Selector@ for @proxyConnection@
proxyConnectionSelector :: Selector '[] Bool
proxyConnectionSelector = mkSelector "proxyConnection"

-- | @Selector@ for @reusedConnection@
reusedConnectionSelector :: Selector '[] Bool
reusedConnectionSelector = mkSelector "reusedConnection"

-- | @Selector@ for @resourceFetchType@
resourceFetchTypeSelector :: Selector '[] NSURLSessionTaskMetricsResourceFetchType
resourceFetchTypeSelector = mkSelector "resourceFetchType"

-- | @Selector@ for @countOfRequestHeaderBytesSent@
countOfRequestHeaderBytesSentSelector :: Selector '[] CLong
countOfRequestHeaderBytesSentSelector = mkSelector "countOfRequestHeaderBytesSent"

-- | @Selector@ for @countOfRequestBodyBytesSent@
countOfRequestBodyBytesSentSelector :: Selector '[] CLong
countOfRequestBodyBytesSentSelector = mkSelector "countOfRequestBodyBytesSent"

-- | @Selector@ for @countOfRequestBodyBytesBeforeEncoding@
countOfRequestBodyBytesBeforeEncodingSelector :: Selector '[] CLong
countOfRequestBodyBytesBeforeEncodingSelector = mkSelector "countOfRequestBodyBytesBeforeEncoding"

-- | @Selector@ for @countOfResponseHeaderBytesReceived@
countOfResponseHeaderBytesReceivedSelector :: Selector '[] CLong
countOfResponseHeaderBytesReceivedSelector = mkSelector "countOfResponseHeaderBytesReceived"

-- | @Selector@ for @countOfResponseBodyBytesReceived@
countOfResponseBodyBytesReceivedSelector :: Selector '[] CLong
countOfResponseBodyBytesReceivedSelector = mkSelector "countOfResponseBodyBytesReceived"

-- | @Selector@ for @countOfResponseBodyBytesAfterDecoding@
countOfResponseBodyBytesAfterDecodingSelector :: Selector '[] CLong
countOfResponseBodyBytesAfterDecodingSelector = mkSelector "countOfResponseBodyBytesAfterDecoding"

-- | @Selector@ for @localAddress@
localAddressSelector :: Selector '[] (Id NSString)
localAddressSelector = mkSelector "localAddress"

-- | @Selector@ for @localPort@
localPortSelector :: Selector '[] (Id NSNumber)
localPortSelector = mkSelector "localPort"

-- | @Selector@ for @remoteAddress@
remoteAddressSelector :: Selector '[] (Id NSString)
remoteAddressSelector = mkSelector "remoteAddress"

-- | @Selector@ for @remotePort@
remotePortSelector :: Selector '[] (Id NSNumber)
remotePortSelector = mkSelector "remotePort"

-- | @Selector@ for @negotiatedTLSProtocolVersion@
negotiatedTLSProtocolVersionSelector :: Selector '[] (Id NSNumber)
negotiatedTLSProtocolVersionSelector = mkSelector "negotiatedTLSProtocolVersion"

-- | @Selector@ for @negotiatedTLSCipherSuite@
negotiatedTLSCipherSuiteSelector :: Selector '[] (Id NSNumber)
negotiatedTLSCipherSuiteSelector = mkSelector "negotiatedTLSCipherSuite"

-- | @Selector@ for @cellular@
cellularSelector :: Selector '[] Bool
cellularSelector = mkSelector "cellular"

-- | @Selector@ for @expensive@
expensiveSelector :: Selector '[] Bool
expensiveSelector = mkSelector "expensive"

-- | @Selector@ for @constrained@
constrainedSelector :: Selector '[] Bool
constrainedSelector = mkSelector "constrained"

-- | @Selector@ for @multipath@
multipathSelector :: Selector '[] Bool
multipathSelector = mkSelector "multipath"

-- | @Selector@ for @domainResolutionProtocol@
domainResolutionProtocolSelector :: Selector '[] NSURLSessionTaskMetricsDomainResolutionProtocol
domainResolutionProtocolSelector = mkSelector "domainResolutionProtocol"

