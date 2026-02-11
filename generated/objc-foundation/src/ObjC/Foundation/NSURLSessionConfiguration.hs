{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSessionConfiguration@.
module ObjC.Foundation.NSURLSessionConfiguration
  ( NSURLSessionConfiguration
  , IsNSURLSessionConfiguration(..)
  , backgroundSessionConfigurationWithIdentifier
  , init_
  , new
  , backgroundSessionConfiguration
  , defaultSessionConfiguration
  , ephemeralSessionConfiguration
  , identifier
  , requestCachePolicy
  , setRequestCachePolicy
  , timeoutIntervalForRequest
  , setTimeoutIntervalForRequest
  , timeoutIntervalForResource
  , setTimeoutIntervalForResource
  , networkServiceType
  , setNetworkServiceType
  , allowsCellularAccess
  , setAllowsCellularAccess
  , allowsExpensiveNetworkAccess
  , setAllowsExpensiveNetworkAccess
  , allowsConstrainedNetworkAccess
  , setAllowsConstrainedNetworkAccess
  , allowsUltraConstrainedNetworkAccess
  , setAllowsUltraConstrainedNetworkAccess
  , requiresDNSSECValidation
  , setRequiresDNSSECValidation
  , waitsForConnectivity
  , setWaitsForConnectivity
  , discretionary
  , setDiscretionary
  , sharedContainerIdentifier
  , setSharedContainerIdentifier
  , sessionSendsLaunchEvents
  , setSessionSendsLaunchEvents
  , connectionProxyDictionary
  , setConnectionProxyDictionary
  , tlsMinimumSupportedProtocol
  , setTLSMinimumSupportedProtocol
  , tlsMaximumSupportedProtocol
  , setTLSMaximumSupportedProtocol
  , tlsMinimumSupportedProtocolVersion
  , setTLSMinimumSupportedProtocolVersion
  , tlsMaximumSupportedProtocolVersion
  , setTLSMaximumSupportedProtocolVersion
  , httpShouldUsePipelining
  , setHTTPShouldUsePipelining
  , httpShouldSetCookies
  , setHTTPShouldSetCookies
  , httpCookieAcceptPolicy
  , setHTTPCookieAcceptPolicy
  , httpAdditionalHeaders
  , setHTTPAdditionalHeaders
  , httpMaximumConnectionsPerHost
  , setHTTPMaximumConnectionsPerHost
  , httpCookieStorage
  , setHTTPCookieStorage
  , urlCredentialStorage
  , setURLCredentialStorage
  , urlCache
  , setURLCache
  , shouldUseExtendedBackgroundIdleMode
  , setShouldUseExtendedBackgroundIdleMode
  , protocolClasses
  , setProtocolClasses
  , multipathServiceType
  , setMultipathServiceType
  , usesClassicLoadingMode
  , setUsesClassicLoadingMode
  , enablesEarlyData
  , setEnablesEarlyData
  , backgroundSessionConfigurationWithIdentifierSelector
  , initSelector
  , newSelector
  , backgroundSessionConfigurationSelector
  , defaultSessionConfigurationSelector
  , ephemeralSessionConfigurationSelector
  , identifierSelector
  , requestCachePolicySelector
  , setRequestCachePolicySelector
  , timeoutIntervalForRequestSelector
  , setTimeoutIntervalForRequestSelector
  , timeoutIntervalForResourceSelector
  , setTimeoutIntervalForResourceSelector
  , networkServiceTypeSelector
  , setNetworkServiceTypeSelector
  , allowsCellularAccessSelector
  , setAllowsCellularAccessSelector
  , allowsExpensiveNetworkAccessSelector
  , setAllowsExpensiveNetworkAccessSelector
  , allowsConstrainedNetworkAccessSelector
  , setAllowsConstrainedNetworkAccessSelector
  , allowsUltraConstrainedNetworkAccessSelector
  , setAllowsUltraConstrainedNetworkAccessSelector
  , requiresDNSSECValidationSelector
  , setRequiresDNSSECValidationSelector
  , waitsForConnectivitySelector
  , setWaitsForConnectivitySelector
  , discretionarySelector
  , setDiscretionarySelector
  , sharedContainerIdentifierSelector
  , setSharedContainerIdentifierSelector
  , sessionSendsLaunchEventsSelector
  , setSessionSendsLaunchEventsSelector
  , connectionProxyDictionarySelector
  , setConnectionProxyDictionarySelector
  , tlsMinimumSupportedProtocolSelector
  , setTLSMinimumSupportedProtocolSelector
  , tlsMaximumSupportedProtocolSelector
  , setTLSMaximumSupportedProtocolSelector
  , tlsMinimumSupportedProtocolVersionSelector
  , setTLSMinimumSupportedProtocolVersionSelector
  , tlsMaximumSupportedProtocolVersionSelector
  , setTLSMaximumSupportedProtocolVersionSelector
  , httpShouldUsePipeliningSelector
  , setHTTPShouldUsePipeliningSelector
  , httpShouldSetCookiesSelector
  , setHTTPShouldSetCookiesSelector
  , httpCookieAcceptPolicySelector
  , setHTTPCookieAcceptPolicySelector
  , httpAdditionalHeadersSelector
  , setHTTPAdditionalHeadersSelector
  , httpMaximumConnectionsPerHostSelector
  , setHTTPMaximumConnectionsPerHostSelector
  , httpCookieStorageSelector
  , setHTTPCookieStorageSelector
  , urlCredentialStorageSelector
  , setURLCredentialStorageSelector
  , urlCacheSelector
  , setURLCacheSelector
  , shouldUseExtendedBackgroundIdleModeSelector
  , setShouldUseExtendedBackgroundIdleModeSelector
  , protocolClassesSelector
  , setProtocolClassesSelector
  , multipathServiceTypeSelector
  , setMultipathServiceTypeSelector
  , usesClassicLoadingModeSelector
  , setUsesClassicLoadingModeSelector
  , enablesEarlyDataSelector
  , setEnablesEarlyDataSelector

  -- * Enum types
  , NSHTTPCookieAcceptPolicy(NSHTTPCookieAcceptPolicy)
  , pattern NSHTTPCookieAcceptPolicyAlways
  , pattern NSHTTPCookieAcceptPolicyNever
  , pattern NSHTTPCookieAcceptPolicyOnlyFromMainDocumentDomain
  , NSURLRequestCachePolicy(NSURLRequestCachePolicy)
  , pattern NSURLRequestUseProtocolCachePolicy
  , pattern NSURLRequestReloadIgnoringLocalCacheData
  , pattern NSURLRequestReloadIgnoringLocalAndRemoteCacheData
  , pattern NSURLRequestReloadIgnoringCacheData
  , pattern NSURLRequestReturnCacheDataElseLoad
  , pattern NSURLRequestReturnCacheDataDontLoad
  , pattern NSURLRequestReloadRevalidatingCacheData
  , NSURLRequestNetworkServiceType(NSURLRequestNetworkServiceType)
  , pattern NSURLNetworkServiceTypeDefault
  , pattern NSURLNetworkServiceTypeVoIP
  , pattern NSURLNetworkServiceTypeVideo
  , pattern NSURLNetworkServiceTypeBackground
  , pattern NSURLNetworkServiceTypeVoice
  , pattern NSURLNetworkServiceTypeResponsiveData
  , pattern NSURLNetworkServiceTypeAVStreaming
  , pattern NSURLNetworkServiceTypeResponsiveAV
  , pattern NSURLNetworkServiceTypeCallSignaling
  , NSURLSessionMultipathServiceType(NSURLSessionMultipathServiceType)
  , pattern NSURLSessionMultipathServiceTypeNone
  , pattern NSURLSessionMultipathServiceTypeHandover
  , pattern NSURLSessionMultipathServiceTypeInteractive
  , pattern NSURLSessionMultipathServiceTypeAggregate
  , SSLProtocol(SSLProtocol)
  , pattern KSSLProtocolUnknown
  , pattern KTLSProtocol1
  , pattern KTLSProtocol11
  , pattern KTLSProtocol12
  , pattern KDTLSProtocol1
  , pattern KTLSProtocol13
  , pattern KDTLSProtocol12
  , pattern KTLSProtocolMaxSupported
  , pattern KSSLProtocol2
  , pattern KSSLProtocol3
  , pattern KSSLProtocol3Only
  , pattern KTLSProtocol1Only
  , pattern KSSLProtocolAll

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

-- | @+ backgroundSessionConfigurationWithIdentifier:@
backgroundSessionConfigurationWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSURLSessionConfiguration)
backgroundSessionConfigurationWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "backgroundSessionConfigurationWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSURLSessionConfiguration)
init_ nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSessionConfiguration)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ backgroundSessionConfiguration:@
backgroundSessionConfiguration :: IsNSString identifier => identifier -> IO (Id NSURLSessionConfiguration)
backgroundSessionConfiguration identifier =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "backgroundSessionConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ defaultSessionConfiguration@
defaultSessionConfiguration :: IO (Id NSURLSessionConfiguration)
defaultSessionConfiguration  =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    sendClassMsg cls' (mkSelector "defaultSessionConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ephemeralSessionConfiguration@
ephemeralSessionConfiguration :: IO (Id NSURLSessionConfiguration)
ephemeralSessionConfiguration  =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    sendClassMsg cls' (mkSelector "ephemeralSessionConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSString)
identifier nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requestCachePolicy@
requestCachePolicy :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO NSURLRequestCachePolicy
requestCachePolicy nsurlSessionConfiguration  =
  fmap (coerce :: CULong -> NSURLRequestCachePolicy) $ sendMsg nsurlSessionConfiguration (mkSelector "requestCachePolicy") retCULong []

-- | @- setRequestCachePolicy:@
setRequestCachePolicy :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> NSURLRequestCachePolicy -> IO ()
setRequestCachePolicy nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setRequestCachePolicy:") retVoid [argCULong (coerce value)]

-- | @- timeoutIntervalForRequest@
timeoutIntervalForRequest :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CDouble
timeoutIntervalForRequest nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "timeoutIntervalForRequest") retCDouble []

-- | @- setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequest :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CDouble -> IO ()
setTimeoutIntervalForRequest nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setTimeoutIntervalForRequest:") retVoid [argCDouble (fromIntegral value)]

-- | @- timeoutIntervalForResource@
timeoutIntervalForResource :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CDouble
timeoutIntervalForResource nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "timeoutIntervalForResource") retCDouble []

-- | @- setTimeoutIntervalForResource:@
setTimeoutIntervalForResource :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CDouble -> IO ()
setTimeoutIntervalForResource nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setTimeoutIntervalForResource:") retVoid [argCDouble (fromIntegral value)]

-- | @- networkServiceType@
networkServiceType :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO NSURLRequestNetworkServiceType
networkServiceType nsurlSessionConfiguration  =
  fmap (coerce :: CULong -> NSURLRequestNetworkServiceType) $ sendMsg nsurlSessionConfiguration (mkSelector "networkServiceType") retCULong []

-- | @- setNetworkServiceType:@
setNetworkServiceType :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> NSURLRequestNetworkServiceType -> IO ()
setNetworkServiceType nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setNetworkServiceType:") retVoid [argCULong (coerce value)]

-- | @- allowsCellularAccess@
allowsCellularAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
allowsCellularAccess nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "allowsCellularAccess") retCULong []

-- | @- setAllowsCellularAccess:@
setAllowsCellularAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setAllowsCellularAccess nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setAllowsCellularAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsExpensiveNetworkAccess@
allowsExpensiveNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
allowsExpensiveNetworkAccess nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "allowsExpensiveNetworkAccess") retCULong []

-- | @- setAllowsExpensiveNetworkAccess:@
setAllowsExpensiveNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setAllowsExpensiveNetworkAccess nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setAllowsExpensiveNetworkAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsConstrainedNetworkAccess@
allowsConstrainedNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
allowsConstrainedNetworkAccess nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "allowsConstrainedNetworkAccess") retCULong []

-- | @- setAllowsConstrainedNetworkAccess:@
setAllowsConstrainedNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setAllowsConstrainedNetworkAccess nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setAllowsConstrainedNetworkAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsUltraConstrainedNetworkAccess@
allowsUltraConstrainedNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
allowsUltraConstrainedNetworkAccess nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "allowsUltraConstrainedNetworkAccess") retCULong []

-- | @- setAllowsUltraConstrainedNetworkAccess:@
setAllowsUltraConstrainedNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setAllowsUltraConstrainedNetworkAccess nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setAllowsUltraConstrainedNetworkAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | @- requiresDNSSECValidation@
requiresDNSSECValidation :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
requiresDNSSECValidation nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "requiresDNSSECValidation") retCULong []

-- | @- setRequiresDNSSECValidation:@
setRequiresDNSSECValidation :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setRequiresDNSSECValidation nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setRequiresDNSSECValidation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- waitsForConnectivity@
waitsForConnectivity :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
waitsForConnectivity nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "waitsForConnectivity") retCULong []

-- | @- setWaitsForConnectivity:@
setWaitsForConnectivity :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setWaitsForConnectivity nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setWaitsForConnectivity:") retVoid [argCULong (if value then 1 else 0)]

-- | @- discretionary@
discretionary :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
discretionary nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "discretionary") retCULong []

-- | @- setDiscretionary:@
setDiscretionary :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setDiscretionary nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setDiscretionary:") retVoid [argCULong (if value then 1 else 0)]

-- | @- sharedContainerIdentifier@
sharedContainerIdentifier :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSString)
sharedContainerIdentifier nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "sharedContainerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSharedContainerIdentifier:@
setSharedContainerIdentifier :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSString value) => nsurlSessionConfiguration -> value -> IO ()
setSharedContainerIdentifier nsurlSessionConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionConfiguration (mkSelector "setSharedContainerIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sessionSendsLaunchEvents@
sessionSendsLaunchEvents :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
sessionSendsLaunchEvents nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "sessionSendsLaunchEvents") retCULong []

-- | @- setSessionSendsLaunchEvents:@
setSessionSendsLaunchEvents :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setSessionSendsLaunchEvents nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setSessionSendsLaunchEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- connectionProxyDictionary@
connectionProxyDictionary :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSDictionary)
connectionProxyDictionary nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "connectionProxyDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnectionProxyDictionary:@
setConnectionProxyDictionary :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSDictionary value) => nsurlSessionConfiguration -> value -> IO ()
setConnectionProxyDictionary nsurlSessionConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionConfiguration (mkSelector "setConnectionProxyDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- TLSMinimumSupportedProtocol@
tlsMinimumSupportedProtocol :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO SSLProtocol
tlsMinimumSupportedProtocol nsurlSessionConfiguration  =
  fmap (coerce :: CInt -> SSLProtocol) $ sendMsg nsurlSessionConfiguration (mkSelector "TLSMinimumSupportedProtocol") retCInt []

-- | @- setTLSMinimumSupportedProtocol:@
setTLSMinimumSupportedProtocol :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> SSLProtocol -> IO ()
setTLSMinimumSupportedProtocol nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setTLSMinimumSupportedProtocol:") retVoid [argCInt (coerce value)]

-- | @- TLSMaximumSupportedProtocol@
tlsMaximumSupportedProtocol :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO SSLProtocol
tlsMaximumSupportedProtocol nsurlSessionConfiguration  =
  fmap (coerce :: CInt -> SSLProtocol) $ sendMsg nsurlSessionConfiguration (mkSelector "TLSMaximumSupportedProtocol") retCInt []

-- | @- setTLSMaximumSupportedProtocol:@
setTLSMaximumSupportedProtocol :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> SSLProtocol -> IO ()
setTLSMaximumSupportedProtocol nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setTLSMaximumSupportedProtocol:") retVoid [argCInt (coerce value)]

-- | @- TLSMinimumSupportedProtocolVersion@
tlsMinimumSupportedProtocolVersion :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CInt
tlsMinimumSupportedProtocolVersion nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "TLSMinimumSupportedProtocolVersion") retCInt []

-- | @- setTLSMinimumSupportedProtocolVersion:@
setTLSMinimumSupportedProtocolVersion :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CInt -> IO ()
setTLSMinimumSupportedProtocolVersion nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setTLSMinimumSupportedProtocolVersion:") retVoid [argCInt (fromIntegral value)]

-- | @- TLSMaximumSupportedProtocolVersion@
tlsMaximumSupportedProtocolVersion :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CInt
tlsMaximumSupportedProtocolVersion nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "TLSMaximumSupportedProtocolVersion") retCInt []

-- | @- setTLSMaximumSupportedProtocolVersion:@
setTLSMaximumSupportedProtocolVersion :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CInt -> IO ()
setTLSMaximumSupportedProtocolVersion nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setTLSMaximumSupportedProtocolVersion:") retVoid [argCInt (fromIntegral value)]

-- | @- HTTPShouldUsePipelining@
httpShouldUsePipelining :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
httpShouldUsePipelining nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "HTTPShouldUsePipelining") retCULong []

-- | @- setHTTPShouldUsePipelining:@
setHTTPShouldUsePipelining :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setHTTPShouldUsePipelining nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setHTTPShouldUsePipelining:") retVoid [argCULong (if value then 1 else 0)]

-- | @- HTTPShouldSetCookies@
httpShouldSetCookies :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
httpShouldSetCookies nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "HTTPShouldSetCookies") retCULong []

-- | @- setHTTPShouldSetCookies:@
setHTTPShouldSetCookies :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setHTTPShouldSetCookies nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setHTTPShouldSetCookies:") retVoid [argCULong (if value then 1 else 0)]

-- | @- HTTPCookieAcceptPolicy@
httpCookieAcceptPolicy :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO NSHTTPCookieAcceptPolicy
httpCookieAcceptPolicy nsurlSessionConfiguration  =
  fmap (coerce :: CULong -> NSHTTPCookieAcceptPolicy) $ sendMsg nsurlSessionConfiguration (mkSelector "HTTPCookieAcceptPolicy") retCULong []

-- | @- setHTTPCookieAcceptPolicy:@
setHTTPCookieAcceptPolicy :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> NSHTTPCookieAcceptPolicy -> IO ()
setHTTPCookieAcceptPolicy nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setHTTPCookieAcceptPolicy:") retVoid [argCULong (coerce value)]

-- | @- HTTPAdditionalHeaders@
httpAdditionalHeaders :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSDictionary)
httpAdditionalHeaders nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "HTTPAdditionalHeaders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHTTPAdditionalHeaders:@
setHTTPAdditionalHeaders :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSDictionary value) => nsurlSessionConfiguration -> value -> IO ()
setHTTPAdditionalHeaders nsurlSessionConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionConfiguration (mkSelector "setHTTPAdditionalHeaders:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- HTTPMaximumConnectionsPerHost@
httpMaximumConnectionsPerHost :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CLong
httpMaximumConnectionsPerHost nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "HTTPMaximumConnectionsPerHost") retCLong []

-- | @- setHTTPMaximumConnectionsPerHost:@
setHTTPMaximumConnectionsPerHost :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CLong -> IO ()
setHTTPMaximumConnectionsPerHost nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setHTTPMaximumConnectionsPerHost:") retVoid [argCLong (fromIntegral value)]

-- | @- HTTPCookieStorage@
httpCookieStorage :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSHTTPCookieStorage)
httpCookieStorage nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "HTTPCookieStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHTTPCookieStorage:@
setHTTPCookieStorage :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSHTTPCookieStorage value) => nsurlSessionConfiguration -> value -> IO ()
setHTTPCookieStorage nsurlSessionConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionConfiguration (mkSelector "setHTTPCookieStorage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- URLCredentialStorage@
urlCredentialStorage :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSURLCredentialStorage)
urlCredentialStorage nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "URLCredentialStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURLCredentialStorage:@
setURLCredentialStorage :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSURLCredentialStorage value) => nsurlSessionConfiguration -> value -> IO ()
setURLCredentialStorage nsurlSessionConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionConfiguration (mkSelector "setURLCredentialStorage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- URLCache@
urlCache :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSURLCache)
urlCache nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "URLCache") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURLCache:@
setURLCache :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSURLCache value) => nsurlSessionConfiguration -> value -> IO ()
setURLCache nsurlSessionConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionConfiguration (mkSelector "setURLCache:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shouldUseExtendedBackgroundIdleMode@
shouldUseExtendedBackgroundIdleMode :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
shouldUseExtendedBackgroundIdleMode nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "shouldUseExtendedBackgroundIdleMode") retCULong []

-- | @- setShouldUseExtendedBackgroundIdleMode:@
setShouldUseExtendedBackgroundIdleMode :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setShouldUseExtendedBackgroundIdleMode nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setShouldUseExtendedBackgroundIdleMode:") retVoid [argCULong (if value then 1 else 0)]

-- | @- protocolClasses@
protocolClasses :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSArray)
protocolClasses nsurlSessionConfiguration  =
  sendMsg nsurlSessionConfiguration (mkSelector "protocolClasses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProtocolClasses:@
setProtocolClasses :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSArray value) => nsurlSessionConfiguration -> value -> IO ()
setProtocolClasses nsurlSessionConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSessionConfiguration (mkSelector "setProtocolClasses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- multipathServiceType@
multipathServiceType :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO NSURLSessionMultipathServiceType
multipathServiceType nsurlSessionConfiguration  =
  fmap (coerce :: CLong -> NSURLSessionMultipathServiceType) $ sendMsg nsurlSessionConfiguration (mkSelector "multipathServiceType") retCLong []

-- | @- setMultipathServiceType:@
setMultipathServiceType :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> NSURLSessionMultipathServiceType -> IO ()
setMultipathServiceType nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setMultipathServiceType:") retVoid [argCLong (coerce value)]

-- | @- usesClassicLoadingMode@
usesClassicLoadingMode :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
usesClassicLoadingMode nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "usesClassicLoadingMode") retCULong []

-- | @- setUsesClassicLoadingMode:@
setUsesClassicLoadingMode :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setUsesClassicLoadingMode nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setUsesClassicLoadingMode:") retVoid [argCULong (if value then 1 else 0)]

-- | @- enablesEarlyData@
enablesEarlyData :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
enablesEarlyData nsurlSessionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlSessionConfiguration (mkSelector "enablesEarlyData") retCULong []

-- | @- setEnablesEarlyData:@
setEnablesEarlyData :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setEnablesEarlyData nsurlSessionConfiguration  value =
  sendMsg nsurlSessionConfiguration (mkSelector "setEnablesEarlyData:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @backgroundSessionConfigurationWithIdentifier:@
backgroundSessionConfigurationWithIdentifierSelector :: Selector
backgroundSessionConfigurationWithIdentifierSelector = mkSelector "backgroundSessionConfigurationWithIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @backgroundSessionConfiguration:@
backgroundSessionConfigurationSelector :: Selector
backgroundSessionConfigurationSelector = mkSelector "backgroundSessionConfiguration:"

-- | @Selector@ for @defaultSessionConfiguration@
defaultSessionConfigurationSelector :: Selector
defaultSessionConfigurationSelector = mkSelector "defaultSessionConfiguration"

-- | @Selector@ for @ephemeralSessionConfiguration@
ephemeralSessionConfigurationSelector :: Selector
ephemeralSessionConfigurationSelector = mkSelector "ephemeralSessionConfiguration"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @requestCachePolicy@
requestCachePolicySelector :: Selector
requestCachePolicySelector = mkSelector "requestCachePolicy"

-- | @Selector@ for @setRequestCachePolicy:@
setRequestCachePolicySelector :: Selector
setRequestCachePolicySelector = mkSelector "setRequestCachePolicy:"

-- | @Selector@ for @timeoutIntervalForRequest@
timeoutIntervalForRequestSelector :: Selector
timeoutIntervalForRequestSelector = mkSelector "timeoutIntervalForRequest"

-- | @Selector@ for @setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequestSelector :: Selector
setTimeoutIntervalForRequestSelector = mkSelector "setTimeoutIntervalForRequest:"

-- | @Selector@ for @timeoutIntervalForResource@
timeoutIntervalForResourceSelector :: Selector
timeoutIntervalForResourceSelector = mkSelector "timeoutIntervalForResource"

-- | @Selector@ for @setTimeoutIntervalForResource:@
setTimeoutIntervalForResourceSelector :: Selector
setTimeoutIntervalForResourceSelector = mkSelector "setTimeoutIntervalForResource:"

-- | @Selector@ for @networkServiceType@
networkServiceTypeSelector :: Selector
networkServiceTypeSelector = mkSelector "networkServiceType"

-- | @Selector@ for @setNetworkServiceType:@
setNetworkServiceTypeSelector :: Selector
setNetworkServiceTypeSelector = mkSelector "setNetworkServiceType:"

-- | @Selector@ for @allowsCellularAccess@
allowsCellularAccessSelector :: Selector
allowsCellularAccessSelector = mkSelector "allowsCellularAccess"

-- | @Selector@ for @setAllowsCellularAccess:@
setAllowsCellularAccessSelector :: Selector
setAllowsCellularAccessSelector = mkSelector "setAllowsCellularAccess:"

-- | @Selector@ for @allowsExpensiveNetworkAccess@
allowsExpensiveNetworkAccessSelector :: Selector
allowsExpensiveNetworkAccessSelector = mkSelector "allowsExpensiveNetworkAccess"

-- | @Selector@ for @setAllowsExpensiveNetworkAccess:@
setAllowsExpensiveNetworkAccessSelector :: Selector
setAllowsExpensiveNetworkAccessSelector = mkSelector "setAllowsExpensiveNetworkAccess:"

-- | @Selector@ for @allowsConstrainedNetworkAccess@
allowsConstrainedNetworkAccessSelector :: Selector
allowsConstrainedNetworkAccessSelector = mkSelector "allowsConstrainedNetworkAccess"

-- | @Selector@ for @setAllowsConstrainedNetworkAccess:@
setAllowsConstrainedNetworkAccessSelector :: Selector
setAllowsConstrainedNetworkAccessSelector = mkSelector "setAllowsConstrainedNetworkAccess:"

-- | @Selector@ for @allowsUltraConstrainedNetworkAccess@
allowsUltraConstrainedNetworkAccessSelector :: Selector
allowsUltraConstrainedNetworkAccessSelector = mkSelector "allowsUltraConstrainedNetworkAccess"

-- | @Selector@ for @setAllowsUltraConstrainedNetworkAccess:@
setAllowsUltraConstrainedNetworkAccessSelector :: Selector
setAllowsUltraConstrainedNetworkAccessSelector = mkSelector "setAllowsUltraConstrainedNetworkAccess:"

-- | @Selector@ for @requiresDNSSECValidation@
requiresDNSSECValidationSelector :: Selector
requiresDNSSECValidationSelector = mkSelector "requiresDNSSECValidation"

-- | @Selector@ for @setRequiresDNSSECValidation:@
setRequiresDNSSECValidationSelector :: Selector
setRequiresDNSSECValidationSelector = mkSelector "setRequiresDNSSECValidation:"

-- | @Selector@ for @waitsForConnectivity@
waitsForConnectivitySelector :: Selector
waitsForConnectivitySelector = mkSelector "waitsForConnectivity"

-- | @Selector@ for @setWaitsForConnectivity:@
setWaitsForConnectivitySelector :: Selector
setWaitsForConnectivitySelector = mkSelector "setWaitsForConnectivity:"

-- | @Selector@ for @discretionary@
discretionarySelector :: Selector
discretionarySelector = mkSelector "discretionary"

-- | @Selector@ for @setDiscretionary:@
setDiscretionarySelector :: Selector
setDiscretionarySelector = mkSelector "setDiscretionary:"

-- | @Selector@ for @sharedContainerIdentifier@
sharedContainerIdentifierSelector :: Selector
sharedContainerIdentifierSelector = mkSelector "sharedContainerIdentifier"

-- | @Selector@ for @setSharedContainerIdentifier:@
setSharedContainerIdentifierSelector :: Selector
setSharedContainerIdentifierSelector = mkSelector "setSharedContainerIdentifier:"

-- | @Selector@ for @sessionSendsLaunchEvents@
sessionSendsLaunchEventsSelector :: Selector
sessionSendsLaunchEventsSelector = mkSelector "sessionSendsLaunchEvents"

-- | @Selector@ for @setSessionSendsLaunchEvents:@
setSessionSendsLaunchEventsSelector :: Selector
setSessionSendsLaunchEventsSelector = mkSelector "setSessionSendsLaunchEvents:"

-- | @Selector@ for @connectionProxyDictionary@
connectionProxyDictionarySelector :: Selector
connectionProxyDictionarySelector = mkSelector "connectionProxyDictionary"

-- | @Selector@ for @setConnectionProxyDictionary:@
setConnectionProxyDictionarySelector :: Selector
setConnectionProxyDictionarySelector = mkSelector "setConnectionProxyDictionary:"

-- | @Selector@ for @TLSMinimumSupportedProtocol@
tlsMinimumSupportedProtocolSelector :: Selector
tlsMinimumSupportedProtocolSelector = mkSelector "TLSMinimumSupportedProtocol"

-- | @Selector@ for @setTLSMinimumSupportedProtocol:@
setTLSMinimumSupportedProtocolSelector :: Selector
setTLSMinimumSupportedProtocolSelector = mkSelector "setTLSMinimumSupportedProtocol:"

-- | @Selector@ for @TLSMaximumSupportedProtocol@
tlsMaximumSupportedProtocolSelector :: Selector
tlsMaximumSupportedProtocolSelector = mkSelector "TLSMaximumSupportedProtocol"

-- | @Selector@ for @setTLSMaximumSupportedProtocol:@
setTLSMaximumSupportedProtocolSelector :: Selector
setTLSMaximumSupportedProtocolSelector = mkSelector "setTLSMaximumSupportedProtocol:"

-- | @Selector@ for @TLSMinimumSupportedProtocolVersion@
tlsMinimumSupportedProtocolVersionSelector :: Selector
tlsMinimumSupportedProtocolVersionSelector = mkSelector "TLSMinimumSupportedProtocolVersion"

-- | @Selector@ for @setTLSMinimumSupportedProtocolVersion:@
setTLSMinimumSupportedProtocolVersionSelector :: Selector
setTLSMinimumSupportedProtocolVersionSelector = mkSelector "setTLSMinimumSupportedProtocolVersion:"

-- | @Selector@ for @TLSMaximumSupportedProtocolVersion@
tlsMaximumSupportedProtocolVersionSelector :: Selector
tlsMaximumSupportedProtocolVersionSelector = mkSelector "TLSMaximumSupportedProtocolVersion"

-- | @Selector@ for @setTLSMaximumSupportedProtocolVersion:@
setTLSMaximumSupportedProtocolVersionSelector :: Selector
setTLSMaximumSupportedProtocolVersionSelector = mkSelector "setTLSMaximumSupportedProtocolVersion:"

-- | @Selector@ for @HTTPShouldUsePipelining@
httpShouldUsePipeliningSelector :: Selector
httpShouldUsePipeliningSelector = mkSelector "HTTPShouldUsePipelining"

-- | @Selector@ for @setHTTPShouldUsePipelining:@
setHTTPShouldUsePipeliningSelector :: Selector
setHTTPShouldUsePipeliningSelector = mkSelector "setHTTPShouldUsePipelining:"

-- | @Selector@ for @HTTPShouldSetCookies@
httpShouldSetCookiesSelector :: Selector
httpShouldSetCookiesSelector = mkSelector "HTTPShouldSetCookies"

-- | @Selector@ for @setHTTPShouldSetCookies:@
setHTTPShouldSetCookiesSelector :: Selector
setHTTPShouldSetCookiesSelector = mkSelector "setHTTPShouldSetCookies:"

-- | @Selector@ for @HTTPCookieAcceptPolicy@
httpCookieAcceptPolicySelector :: Selector
httpCookieAcceptPolicySelector = mkSelector "HTTPCookieAcceptPolicy"

-- | @Selector@ for @setHTTPCookieAcceptPolicy:@
setHTTPCookieAcceptPolicySelector :: Selector
setHTTPCookieAcceptPolicySelector = mkSelector "setHTTPCookieAcceptPolicy:"

-- | @Selector@ for @HTTPAdditionalHeaders@
httpAdditionalHeadersSelector :: Selector
httpAdditionalHeadersSelector = mkSelector "HTTPAdditionalHeaders"

-- | @Selector@ for @setHTTPAdditionalHeaders:@
setHTTPAdditionalHeadersSelector :: Selector
setHTTPAdditionalHeadersSelector = mkSelector "setHTTPAdditionalHeaders:"

-- | @Selector@ for @HTTPMaximumConnectionsPerHost@
httpMaximumConnectionsPerHostSelector :: Selector
httpMaximumConnectionsPerHostSelector = mkSelector "HTTPMaximumConnectionsPerHost"

-- | @Selector@ for @setHTTPMaximumConnectionsPerHost:@
setHTTPMaximumConnectionsPerHostSelector :: Selector
setHTTPMaximumConnectionsPerHostSelector = mkSelector "setHTTPMaximumConnectionsPerHost:"

-- | @Selector@ for @HTTPCookieStorage@
httpCookieStorageSelector :: Selector
httpCookieStorageSelector = mkSelector "HTTPCookieStorage"

-- | @Selector@ for @setHTTPCookieStorage:@
setHTTPCookieStorageSelector :: Selector
setHTTPCookieStorageSelector = mkSelector "setHTTPCookieStorage:"

-- | @Selector@ for @URLCredentialStorage@
urlCredentialStorageSelector :: Selector
urlCredentialStorageSelector = mkSelector "URLCredentialStorage"

-- | @Selector@ for @setURLCredentialStorage:@
setURLCredentialStorageSelector :: Selector
setURLCredentialStorageSelector = mkSelector "setURLCredentialStorage:"

-- | @Selector@ for @URLCache@
urlCacheSelector :: Selector
urlCacheSelector = mkSelector "URLCache"

-- | @Selector@ for @setURLCache:@
setURLCacheSelector :: Selector
setURLCacheSelector = mkSelector "setURLCache:"

-- | @Selector@ for @shouldUseExtendedBackgroundIdleMode@
shouldUseExtendedBackgroundIdleModeSelector :: Selector
shouldUseExtendedBackgroundIdleModeSelector = mkSelector "shouldUseExtendedBackgroundIdleMode"

-- | @Selector@ for @setShouldUseExtendedBackgroundIdleMode:@
setShouldUseExtendedBackgroundIdleModeSelector :: Selector
setShouldUseExtendedBackgroundIdleModeSelector = mkSelector "setShouldUseExtendedBackgroundIdleMode:"

-- | @Selector@ for @protocolClasses@
protocolClassesSelector :: Selector
protocolClassesSelector = mkSelector "protocolClasses"

-- | @Selector@ for @setProtocolClasses:@
setProtocolClassesSelector :: Selector
setProtocolClassesSelector = mkSelector "setProtocolClasses:"

-- | @Selector@ for @multipathServiceType@
multipathServiceTypeSelector :: Selector
multipathServiceTypeSelector = mkSelector "multipathServiceType"

-- | @Selector@ for @setMultipathServiceType:@
setMultipathServiceTypeSelector :: Selector
setMultipathServiceTypeSelector = mkSelector "setMultipathServiceType:"

-- | @Selector@ for @usesClassicLoadingMode@
usesClassicLoadingModeSelector :: Selector
usesClassicLoadingModeSelector = mkSelector "usesClassicLoadingMode"

-- | @Selector@ for @setUsesClassicLoadingMode:@
setUsesClassicLoadingModeSelector :: Selector
setUsesClassicLoadingModeSelector = mkSelector "setUsesClassicLoadingMode:"

-- | @Selector@ for @enablesEarlyData@
enablesEarlyDataSelector :: Selector
enablesEarlyDataSelector = mkSelector "enablesEarlyData"

-- | @Selector@ for @setEnablesEarlyData:@
setEnablesEarlyDataSelector :: Selector
setEnablesEarlyDataSelector = mkSelector "setEnablesEarlyData:"

