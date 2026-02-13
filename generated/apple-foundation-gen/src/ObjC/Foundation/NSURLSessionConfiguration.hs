{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowsCellularAccessSelector
  , allowsConstrainedNetworkAccessSelector
  , allowsExpensiveNetworkAccessSelector
  , allowsUltraConstrainedNetworkAccessSelector
  , backgroundSessionConfigurationSelector
  , backgroundSessionConfigurationWithIdentifierSelector
  , connectionProxyDictionarySelector
  , defaultSessionConfigurationSelector
  , discretionarySelector
  , enablesEarlyDataSelector
  , ephemeralSessionConfigurationSelector
  , httpAdditionalHeadersSelector
  , httpCookieAcceptPolicySelector
  , httpCookieStorageSelector
  , httpMaximumConnectionsPerHostSelector
  , httpShouldSetCookiesSelector
  , httpShouldUsePipeliningSelector
  , identifierSelector
  , initSelector
  , multipathServiceTypeSelector
  , networkServiceTypeSelector
  , newSelector
  , protocolClassesSelector
  , requestCachePolicySelector
  , requiresDNSSECValidationSelector
  , sessionSendsLaunchEventsSelector
  , setAllowsCellularAccessSelector
  , setAllowsConstrainedNetworkAccessSelector
  , setAllowsExpensiveNetworkAccessSelector
  , setAllowsUltraConstrainedNetworkAccessSelector
  , setConnectionProxyDictionarySelector
  , setDiscretionarySelector
  , setEnablesEarlyDataSelector
  , setHTTPAdditionalHeadersSelector
  , setHTTPCookieAcceptPolicySelector
  , setHTTPCookieStorageSelector
  , setHTTPMaximumConnectionsPerHostSelector
  , setHTTPShouldSetCookiesSelector
  , setHTTPShouldUsePipeliningSelector
  , setMultipathServiceTypeSelector
  , setNetworkServiceTypeSelector
  , setProtocolClassesSelector
  , setRequestCachePolicySelector
  , setRequiresDNSSECValidationSelector
  , setSessionSendsLaunchEventsSelector
  , setSharedContainerIdentifierSelector
  , setShouldUseExtendedBackgroundIdleModeSelector
  , setTLSMaximumSupportedProtocolSelector
  , setTLSMaximumSupportedProtocolVersionSelector
  , setTLSMinimumSupportedProtocolSelector
  , setTLSMinimumSupportedProtocolVersionSelector
  , setTimeoutIntervalForRequestSelector
  , setTimeoutIntervalForResourceSelector
  , setURLCacheSelector
  , setURLCredentialStorageSelector
  , setUsesClassicLoadingModeSelector
  , setWaitsForConnectivitySelector
  , sharedContainerIdentifierSelector
  , shouldUseExtendedBackgroundIdleModeSelector
  , timeoutIntervalForRequestSelector
  , timeoutIntervalForResourceSelector
  , tlsMaximumSupportedProtocolSelector
  , tlsMaximumSupportedProtocolVersionSelector
  , tlsMinimumSupportedProtocolSelector
  , tlsMinimumSupportedProtocolVersionSelector
  , urlCacheSelector
  , urlCredentialStorageSelector
  , usesClassicLoadingModeSelector
  , waitsForConnectivitySelector

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

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ backgroundSessionConfigurationWithIdentifier:@
backgroundSessionConfigurationWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSURLSessionConfiguration)
backgroundSessionConfigurationWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    sendClassMessage cls' backgroundSessionConfigurationWithIdentifierSelector (toNSString identifier)

-- | @- init@
init_ :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSURLSessionConfiguration)
init_ nsurlSessionConfiguration =
  sendOwnedMessage nsurlSessionConfiguration initSelector

-- | @+ new@
new :: IO (Id NSURLSessionConfiguration)
new  =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @+ backgroundSessionConfiguration:@
backgroundSessionConfiguration :: IsNSString identifier => identifier -> IO (Id NSURLSessionConfiguration)
backgroundSessionConfiguration identifier =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    sendClassMessage cls' backgroundSessionConfigurationSelector (toNSString identifier)

-- | @+ defaultSessionConfiguration@
defaultSessionConfiguration :: IO (Id NSURLSessionConfiguration)
defaultSessionConfiguration  =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    sendClassMessage cls' defaultSessionConfigurationSelector

-- | @+ ephemeralSessionConfiguration@
ephemeralSessionConfiguration :: IO (Id NSURLSessionConfiguration)
ephemeralSessionConfiguration  =
  do
    cls' <- getRequiredClass "NSURLSessionConfiguration"
    sendClassMessage cls' ephemeralSessionConfigurationSelector

-- | @- identifier@
identifier :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSString)
identifier nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration identifierSelector

-- | @- requestCachePolicy@
requestCachePolicy :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO NSURLRequestCachePolicy
requestCachePolicy nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration requestCachePolicySelector

-- | @- setRequestCachePolicy:@
setRequestCachePolicy :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> NSURLRequestCachePolicy -> IO ()
setRequestCachePolicy nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setRequestCachePolicySelector value

-- | @- timeoutIntervalForRequest@
timeoutIntervalForRequest :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CDouble
timeoutIntervalForRequest nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration timeoutIntervalForRequestSelector

-- | @- setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequest :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CDouble -> IO ()
setTimeoutIntervalForRequest nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setTimeoutIntervalForRequestSelector value

-- | @- timeoutIntervalForResource@
timeoutIntervalForResource :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CDouble
timeoutIntervalForResource nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration timeoutIntervalForResourceSelector

-- | @- setTimeoutIntervalForResource:@
setTimeoutIntervalForResource :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CDouble -> IO ()
setTimeoutIntervalForResource nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setTimeoutIntervalForResourceSelector value

-- | @- networkServiceType@
networkServiceType :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO NSURLRequestNetworkServiceType
networkServiceType nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration networkServiceTypeSelector

-- | @- setNetworkServiceType:@
setNetworkServiceType :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> NSURLRequestNetworkServiceType -> IO ()
setNetworkServiceType nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setNetworkServiceTypeSelector value

-- | @- allowsCellularAccess@
allowsCellularAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
allowsCellularAccess nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration allowsCellularAccessSelector

-- | @- setAllowsCellularAccess:@
setAllowsCellularAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setAllowsCellularAccess nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setAllowsCellularAccessSelector value

-- | @- allowsExpensiveNetworkAccess@
allowsExpensiveNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
allowsExpensiveNetworkAccess nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration allowsExpensiveNetworkAccessSelector

-- | @- setAllowsExpensiveNetworkAccess:@
setAllowsExpensiveNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setAllowsExpensiveNetworkAccess nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setAllowsExpensiveNetworkAccessSelector value

-- | @- allowsConstrainedNetworkAccess@
allowsConstrainedNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
allowsConstrainedNetworkAccess nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration allowsConstrainedNetworkAccessSelector

-- | @- setAllowsConstrainedNetworkAccess:@
setAllowsConstrainedNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setAllowsConstrainedNetworkAccess nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setAllowsConstrainedNetworkAccessSelector value

-- | @- allowsUltraConstrainedNetworkAccess@
allowsUltraConstrainedNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
allowsUltraConstrainedNetworkAccess nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration allowsUltraConstrainedNetworkAccessSelector

-- | @- setAllowsUltraConstrainedNetworkAccess:@
setAllowsUltraConstrainedNetworkAccess :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setAllowsUltraConstrainedNetworkAccess nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setAllowsUltraConstrainedNetworkAccessSelector value

-- | @- requiresDNSSECValidation@
requiresDNSSECValidation :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
requiresDNSSECValidation nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration requiresDNSSECValidationSelector

-- | @- setRequiresDNSSECValidation:@
setRequiresDNSSECValidation :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setRequiresDNSSECValidation nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setRequiresDNSSECValidationSelector value

-- | @- waitsForConnectivity@
waitsForConnectivity :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
waitsForConnectivity nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration waitsForConnectivitySelector

-- | @- setWaitsForConnectivity:@
setWaitsForConnectivity :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setWaitsForConnectivity nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setWaitsForConnectivitySelector value

-- | @- discretionary@
discretionary :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
discretionary nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration discretionarySelector

-- | @- setDiscretionary:@
setDiscretionary :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setDiscretionary nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setDiscretionarySelector value

-- | @- sharedContainerIdentifier@
sharedContainerIdentifier :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSString)
sharedContainerIdentifier nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration sharedContainerIdentifierSelector

-- | @- setSharedContainerIdentifier:@
setSharedContainerIdentifier :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSString value) => nsurlSessionConfiguration -> value -> IO ()
setSharedContainerIdentifier nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setSharedContainerIdentifierSelector (toNSString value)

-- | @- sessionSendsLaunchEvents@
sessionSendsLaunchEvents :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
sessionSendsLaunchEvents nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration sessionSendsLaunchEventsSelector

-- | @- setSessionSendsLaunchEvents:@
setSessionSendsLaunchEvents :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setSessionSendsLaunchEvents nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setSessionSendsLaunchEventsSelector value

-- | @- connectionProxyDictionary@
connectionProxyDictionary :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSDictionary)
connectionProxyDictionary nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration connectionProxyDictionarySelector

-- | @- setConnectionProxyDictionary:@
setConnectionProxyDictionary :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSDictionary value) => nsurlSessionConfiguration -> value -> IO ()
setConnectionProxyDictionary nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setConnectionProxyDictionarySelector (toNSDictionary value)

-- | @- TLSMinimumSupportedProtocol@
tlsMinimumSupportedProtocol :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CInt
tlsMinimumSupportedProtocol nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration tlsMinimumSupportedProtocolSelector

-- | @- setTLSMinimumSupportedProtocol:@
setTLSMinimumSupportedProtocol :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CInt -> IO ()
setTLSMinimumSupportedProtocol nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setTLSMinimumSupportedProtocolSelector value

-- | @- TLSMaximumSupportedProtocol@
tlsMaximumSupportedProtocol :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CInt
tlsMaximumSupportedProtocol nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration tlsMaximumSupportedProtocolSelector

-- | @- setTLSMaximumSupportedProtocol:@
setTLSMaximumSupportedProtocol :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CInt -> IO ()
setTLSMaximumSupportedProtocol nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setTLSMaximumSupportedProtocolSelector value

-- | @- TLSMinimumSupportedProtocolVersion@
tlsMinimumSupportedProtocolVersion :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CInt
tlsMinimumSupportedProtocolVersion nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration tlsMinimumSupportedProtocolVersionSelector

-- | @- setTLSMinimumSupportedProtocolVersion:@
setTLSMinimumSupportedProtocolVersion :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CInt -> IO ()
setTLSMinimumSupportedProtocolVersion nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setTLSMinimumSupportedProtocolVersionSelector value

-- | @- TLSMaximumSupportedProtocolVersion@
tlsMaximumSupportedProtocolVersion :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CInt
tlsMaximumSupportedProtocolVersion nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration tlsMaximumSupportedProtocolVersionSelector

-- | @- setTLSMaximumSupportedProtocolVersion:@
setTLSMaximumSupportedProtocolVersion :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CInt -> IO ()
setTLSMaximumSupportedProtocolVersion nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setTLSMaximumSupportedProtocolVersionSelector value

-- | @- HTTPShouldUsePipelining@
httpShouldUsePipelining :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
httpShouldUsePipelining nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration httpShouldUsePipeliningSelector

-- | @- setHTTPShouldUsePipelining:@
setHTTPShouldUsePipelining :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setHTTPShouldUsePipelining nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setHTTPShouldUsePipeliningSelector value

-- | @- HTTPShouldSetCookies@
httpShouldSetCookies :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
httpShouldSetCookies nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration httpShouldSetCookiesSelector

-- | @- setHTTPShouldSetCookies:@
setHTTPShouldSetCookies :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setHTTPShouldSetCookies nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setHTTPShouldSetCookiesSelector value

-- | @- HTTPCookieAcceptPolicy@
httpCookieAcceptPolicy :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO NSHTTPCookieAcceptPolicy
httpCookieAcceptPolicy nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration httpCookieAcceptPolicySelector

-- | @- setHTTPCookieAcceptPolicy:@
setHTTPCookieAcceptPolicy :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> NSHTTPCookieAcceptPolicy -> IO ()
setHTTPCookieAcceptPolicy nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setHTTPCookieAcceptPolicySelector value

-- | @- HTTPAdditionalHeaders@
httpAdditionalHeaders :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSDictionary)
httpAdditionalHeaders nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration httpAdditionalHeadersSelector

-- | @- setHTTPAdditionalHeaders:@
setHTTPAdditionalHeaders :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSDictionary value) => nsurlSessionConfiguration -> value -> IO ()
setHTTPAdditionalHeaders nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setHTTPAdditionalHeadersSelector (toNSDictionary value)

-- | @- HTTPMaximumConnectionsPerHost@
httpMaximumConnectionsPerHost :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO CLong
httpMaximumConnectionsPerHost nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration httpMaximumConnectionsPerHostSelector

-- | @- setHTTPMaximumConnectionsPerHost:@
setHTTPMaximumConnectionsPerHost :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> CLong -> IO ()
setHTTPMaximumConnectionsPerHost nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setHTTPMaximumConnectionsPerHostSelector value

-- | @- HTTPCookieStorage@
httpCookieStorage :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSHTTPCookieStorage)
httpCookieStorage nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration httpCookieStorageSelector

-- | @- setHTTPCookieStorage:@
setHTTPCookieStorage :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSHTTPCookieStorage value) => nsurlSessionConfiguration -> value -> IO ()
setHTTPCookieStorage nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setHTTPCookieStorageSelector (toNSHTTPCookieStorage value)

-- | @- URLCredentialStorage@
urlCredentialStorage :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSURLCredentialStorage)
urlCredentialStorage nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration urlCredentialStorageSelector

-- | @- setURLCredentialStorage:@
setURLCredentialStorage :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSURLCredentialStorage value) => nsurlSessionConfiguration -> value -> IO ()
setURLCredentialStorage nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setURLCredentialStorageSelector (toNSURLCredentialStorage value)

-- | @- URLCache@
urlCache :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSURLCache)
urlCache nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration urlCacheSelector

-- | @- setURLCache:@
setURLCache :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSURLCache value) => nsurlSessionConfiguration -> value -> IO ()
setURLCache nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setURLCacheSelector (toNSURLCache value)

-- | @- shouldUseExtendedBackgroundIdleMode@
shouldUseExtendedBackgroundIdleMode :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
shouldUseExtendedBackgroundIdleMode nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration shouldUseExtendedBackgroundIdleModeSelector

-- | @- setShouldUseExtendedBackgroundIdleMode:@
setShouldUseExtendedBackgroundIdleMode :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setShouldUseExtendedBackgroundIdleMode nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setShouldUseExtendedBackgroundIdleModeSelector value

-- | @- protocolClasses@
protocolClasses :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO (Id NSArray)
protocolClasses nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration protocolClassesSelector

-- | @- setProtocolClasses:@
setProtocolClasses :: (IsNSURLSessionConfiguration nsurlSessionConfiguration, IsNSArray value) => nsurlSessionConfiguration -> value -> IO ()
setProtocolClasses nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setProtocolClassesSelector (toNSArray value)

-- | @- multipathServiceType@
multipathServiceType :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO NSURLSessionMultipathServiceType
multipathServiceType nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration multipathServiceTypeSelector

-- | @- setMultipathServiceType:@
setMultipathServiceType :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> NSURLSessionMultipathServiceType -> IO ()
setMultipathServiceType nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setMultipathServiceTypeSelector value

-- | @- usesClassicLoadingMode@
usesClassicLoadingMode :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
usesClassicLoadingMode nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration usesClassicLoadingModeSelector

-- | @- setUsesClassicLoadingMode:@
setUsesClassicLoadingMode :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setUsesClassicLoadingMode nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setUsesClassicLoadingModeSelector value

-- | @- enablesEarlyData@
enablesEarlyData :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> IO Bool
enablesEarlyData nsurlSessionConfiguration =
  sendMessage nsurlSessionConfiguration enablesEarlyDataSelector

-- | @- setEnablesEarlyData:@
setEnablesEarlyData :: IsNSURLSessionConfiguration nsurlSessionConfiguration => nsurlSessionConfiguration -> Bool -> IO ()
setEnablesEarlyData nsurlSessionConfiguration value =
  sendMessage nsurlSessionConfiguration setEnablesEarlyDataSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @backgroundSessionConfigurationWithIdentifier:@
backgroundSessionConfigurationWithIdentifierSelector :: Selector '[Id NSString] (Id NSURLSessionConfiguration)
backgroundSessionConfigurationWithIdentifierSelector = mkSelector "backgroundSessionConfigurationWithIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSessionConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSessionConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @backgroundSessionConfiguration:@
backgroundSessionConfigurationSelector :: Selector '[Id NSString] (Id NSURLSessionConfiguration)
backgroundSessionConfigurationSelector = mkSelector "backgroundSessionConfiguration:"

-- | @Selector@ for @defaultSessionConfiguration@
defaultSessionConfigurationSelector :: Selector '[] (Id NSURLSessionConfiguration)
defaultSessionConfigurationSelector = mkSelector "defaultSessionConfiguration"

-- | @Selector@ for @ephemeralSessionConfiguration@
ephemeralSessionConfigurationSelector :: Selector '[] (Id NSURLSessionConfiguration)
ephemeralSessionConfigurationSelector = mkSelector "ephemeralSessionConfiguration"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @requestCachePolicy@
requestCachePolicySelector :: Selector '[] NSURLRequestCachePolicy
requestCachePolicySelector = mkSelector "requestCachePolicy"

-- | @Selector@ for @setRequestCachePolicy:@
setRequestCachePolicySelector :: Selector '[NSURLRequestCachePolicy] ()
setRequestCachePolicySelector = mkSelector "setRequestCachePolicy:"

-- | @Selector@ for @timeoutIntervalForRequest@
timeoutIntervalForRequestSelector :: Selector '[] CDouble
timeoutIntervalForRequestSelector = mkSelector "timeoutIntervalForRequest"

-- | @Selector@ for @setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequestSelector :: Selector '[CDouble] ()
setTimeoutIntervalForRequestSelector = mkSelector "setTimeoutIntervalForRequest:"

-- | @Selector@ for @timeoutIntervalForResource@
timeoutIntervalForResourceSelector :: Selector '[] CDouble
timeoutIntervalForResourceSelector = mkSelector "timeoutIntervalForResource"

-- | @Selector@ for @setTimeoutIntervalForResource:@
setTimeoutIntervalForResourceSelector :: Selector '[CDouble] ()
setTimeoutIntervalForResourceSelector = mkSelector "setTimeoutIntervalForResource:"

-- | @Selector@ for @networkServiceType@
networkServiceTypeSelector :: Selector '[] NSURLRequestNetworkServiceType
networkServiceTypeSelector = mkSelector "networkServiceType"

-- | @Selector@ for @setNetworkServiceType:@
setNetworkServiceTypeSelector :: Selector '[NSURLRequestNetworkServiceType] ()
setNetworkServiceTypeSelector = mkSelector "setNetworkServiceType:"

-- | @Selector@ for @allowsCellularAccess@
allowsCellularAccessSelector :: Selector '[] Bool
allowsCellularAccessSelector = mkSelector "allowsCellularAccess"

-- | @Selector@ for @setAllowsCellularAccess:@
setAllowsCellularAccessSelector :: Selector '[Bool] ()
setAllowsCellularAccessSelector = mkSelector "setAllowsCellularAccess:"

-- | @Selector@ for @allowsExpensiveNetworkAccess@
allowsExpensiveNetworkAccessSelector :: Selector '[] Bool
allowsExpensiveNetworkAccessSelector = mkSelector "allowsExpensiveNetworkAccess"

-- | @Selector@ for @setAllowsExpensiveNetworkAccess:@
setAllowsExpensiveNetworkAccessSelector :: Selector '[Bool] ()
setAllowsExpensiveNetworkAccessSelector = mkSelector "setAllowsExpensiveNetworkAccess:"

-- | @Selector@ for @allowsConstrainedNetworkAccess@
allowsConstrainedNetworkAccessSelector :: Selector '[] Bool
allowsConstrainedNetworkAccessSelector = mkSelector "allowsConstrainedNetworkAccess"

-- | @Selector@ for @setAllowsConstrainedNetworkAccess:@
setAllowsConstrainedNetworkAccessSelector :: Selector '[Bool] ()
setAllowsConstrainedNetworkAccessSelector = mkSelector "setAllowsConstrainedNetworkAccess:"

-- | @Selector@ for @allowsUltraConstrainedNetworkAccess@
allowsUltraConstrainedNetworkAccessSelector :: Selector '[] Bool
allowsUltraConstrainedNetworkAccessSelector = mkSelector "allowsUltraConstrainedNetworkAccess"

-- | @Selector@ for @setAllowsUltraConstrainedNetworkAccess:@
setAllowsUltraConstrainedNetworkAccessSelector :: Selector '[Bool] ()
setAllowsUltraConstrainedNetworkAccessSelector = mkSelector "setAllowsUltraConstrainedNetworkAccess:"

-- | @Selector@ for @requiresDNSSECValidation@
requiresDNSSECValidationSelector :: Selector '[] Bool
requiresDNSSECValidationSelector = mkSelector "requiresDNSSECValidation"

-- | @Selector@ for @setRequiresDNSSECValidation:@
setRequiresDNSSECValidationSelector :: Selector '[Bool] ()
setRequiresDNSSECValidationSelector = mkSelector "setRequiresDNSSECValidation:"

-- | @Selector@ for @waitsForConnectivity@
waitsForConnectivitySelector :: Selector '[] Bool
waitsForConnectivitySelector = mkSelector "waitsForConnectivity"

-- | @Selector@ for @setWaitsForConnectivity:@
setWaitsForConnectivitySelector :: Selector '[Bool] ()
setWaitsForConnectivitySelector = mkSelector "setWaitsForConnectivity:"

-- | @Selector@ for @discretionary@
discretionarySelector :: Selector '[] Bool
discretionarySelector = mkSelector "discretionary"

-- | @Selector@ for @setDiscretionary:@
setDiscretionarySelector :: Selector '[Bool] ()
setDiscretionarySelector = mkSelector "setDiscretionary:"

-- | @Selector@ for @sharedContainerIdentifier@
sharedContainerIdentifierSelector :: Selector '[] (Id NSString)
sharedContainerIdentifierSelector = mkSelector "sharedContainerIdentifier"

-- | @Selector@ for @setSharedContainerIdentifier:@
setSharedContainerIdentifierSelector :: Selector '[Id NSString] ()
setSharedContainerIdentifierSelector = mkSelector "setSharedContainerIdentifier:"

-- | @Selector@ for @sessionSendsLaunchEvents@
sessionSendsLaunchEventsSelector :: Selector '[] Bool
sessionSendsLaunchEventsSelector = mkSelector "sessionSendsLaunchEvents"

-- | @Selector@ for @setSessionSendsLaunchEvents:@
setSessionSendsLaunchEventsSelector :: Selector '[Bool] ()
setSessionSendsLaunchEventsSelector = mkSelector "setSessionSendsLaunchEvents:"

-- | @Selector@ for @connectionProxyDictionary@
connectionProxyDictionarySelector :: Selector '[] (Id NSDictionary)
connectionProxyDictionarySelector = mkSelector "connectionProxyDictionary"

-- | @Selector@ for @setConnectionProxyDictionary:@
setConnectionProxyDictionarySelector :: Selector '[Id NSDictionary] ()
setConnectionProxyDictionarySelector = mkSelector "setConnectionProxyDictionary:"

-- | @Selector@ for @TLSMinimumSupportedProtocol@
tlsMinimumSupportedProtocolSelector :: Selector '[] CInt
tlsMinimumSupportedProtocolSelector = mkSelector "TLSMinimumSupportedProtocol"

-- | @Selector@ for @setTLSMinimumSupportedProtocol:@
setTLSMinimumSupportedProtocolSelector :: Selector '[CInt] ()
setTLSMinimumSupportedProtocolSelector = mkSelector "setTLSMinimumSupportedProtocol:"

-- | @Selector@ for @TLSMaximumSupportedProtocol@
tlsMaximumSupportedProtocolSelector :: Selector '[] CInt
tlsMaximumSupportedProtocolSelector = mkSelector "TLSMaximumSupportedProtocol"

-- | @Selector@ for @setTLSMaximumSupportedProtocol:@
setTLSMaximumSupportedProtocolSelector :: Selector '[CInt] ()
setTLSMaximumSupportedProtocolSelector = mkSelector "setTLSMaximumSupportedProtocol:"

-- | @Selector@ for @TLSMinimumSupportedProtocolVersion@
tlsMinimumSupportedProtocolVersionSelector :: Selector '[] CInt
tlsMinimumSupportedProtocolVersionSelector = mkSelector "TLSMinimumSupportedProtocolVersion"

-- | @Selector@ for @setTLSMinimumSupportedProtocolVersion:@
setTLSMinimumSupportedProtocolVersionSelector :: Selector '[CInt] ()
setTLSMinimumSupportedProtocolVersionSelector = mkSelector "setTLSMinimumSupportedProtocolVersion:"

-- | @Selector@ for @TLSMaximumSupportedProtocolVersion@
tlsMaximumSupportedProtocolVersionSelector :: Selector '[] CInt
tlsMaximumSupportedProtocolVersionSelector = mkSelector "TLSMaximumSupportedProtocolVersion"

-- | @Selector@ for @setTLSMaximumSupportedProtocolVersion:@
setTLSMaximumSupportedProtocolVersionSelector :: Selector '[CInt] ()
setTLSMaximumSupportedProtocolVersionSelector = mkSelector "setTLSMaximumSupportedProtocolVersion:"

-- | @Selector@ for @HTTPShouldUsePipelining@
httpShouldUsePipeliningSelector :: Selector '[] Bool
httpShouldUsePipeliningSelector = mkSelector "HTTPShouldUsePipelining"

-- | @Selector@ for @setHTTPShouldUsePipelining:@
setHTTPShouldUsePipeliningSelector :: Selector '[Bool] ()
setHTTPShouldUsePipeliningSelector = mkSelector "setHTTPShouldUsePipelining:"

-- | @Selector@ for @HTTPShouldSetCookies@
httpShouldSetCookiesSelector :: Selector '[] Bool
httpShouldSetCookiesSelector = mkSelector "HTTPShouldSetCookies"

-- | @Selector@ for @setHTTPShouldSetCookies:@
setHTTPShouldSetCookiesSelector :: Selector '[Bool] ()
setHTTPShouldSetCookiesSelector = mkSelector "setHTTPShouldSetCookies:"

-- | @Selector@ for @HTTPCookieAcceptPolicy@
httpCookieAcceptPolicySelector :: Selector '[] NSHTTPCookieAcceptPolicy
httpCookieAcceptPolicySelector = mkSelector "HTTPCookieAcceptPolicy"

-- | @Selector@ for @setHTTPCookieAcceptPolicy:@
setHTTPCookieAcceptPolicySelector :: Selector '[NSHTTPCookieAcceptPolicy] ()
setHTTPCookieAcceptPolicySelector = mkSelector "setHTTPCookieAcceptPolicy:"

-- | @Selector@ for @HTTPAdditionalHeaders@
httpAdditionalHeadersSelector :: Selector '[] (Id NSDictionary)
httpAdditionalHeadersSelector = mkSelector "HTTPAdditionalHeaders"

-- | @Selector@ for @setHTTPAdditionalHeaders:@
setHTTPAdditionalHeadersSelector :: Selector '[Id NSDictionary] ()
setHTTPAdditionalHeadersSelector = mkSelector "setHTTPAdditionalHeaders:"

-- | @Selector@ for @HTTPMaximumConnectionsPerHost@
httpMaximumConnectionsPerHostSelector :: Selector '[] CLong
httpMaximumConnectionsPerHostSelector = mkSelector "HTTPMaximumConnectionsPerHost"

-- | @Selector@ for @setHTTPMaximumConnectionsPerHost:@
setHTTPMaximumConnectionsPerHostSelector :: Selector '[CLong] ()
setHTTPMaximumConnectionsPerHostSelector = mkSelector "setHTTPMaximumConnectionsPerHost:"

-- | @Selector@ for @HTTPCookieStorage@
httpCookieStorageSelector :: Selector '[] (Id NSHTTPCookieStorage)
httpCookieStorageSelector = mkSelector "HTTPCookieStorage"

-- | @Selector@ for @setHTTPCookieStorage:@
setHTTPCookieStorageSelector :: Selector '[Id NSHTTPCookieStorage] ()
setHTTPCookieStorageSelector = mkSelector "setHTTPCookieStorage:"

-- | @Selector@ for @URLCredentialStorage@
urlCredentialStorageSelector :: Selector '[] (Id NSURLCredentialStorage)
urlCredentialStorageSelector = mkSelector "URLCredentialStorage"

-- | @Selector@ for @setURLCredentialStorage:@
setURLCredentialStorageSelector :: Selector '[Id NSURLCredentialStorage] ()
setURLCredentialStorageSelector = mkSelector "setURLCredentialStorage:"

-- | @Selector@ for @URLCache@
urlCacheSelector :: Selector '[] (Id NSURLCache)
urlCacheSelector = mkSelector "URLCache"

-- | @Selector@ for @setURLCache:@
setURLCacheSelector :: Selector '[Id NSURLCache] ()
setURLCacheSelector = mkSelector "setURLCache:"

-- | @Selector@ for @shouldUseExtendedBackgroundIdleMode@
shouldUseExtendedBackgroundIdleModeSelector :: Selector '[] Bool
shouldUseExtendedBackgroundIdleModeSelector = mkSelector "shouldUseExtendedBackgroundIdleMode"

-- | @Selector@ for @setShouldUseExtendedBackgroundIdleMode:@
setShouldUseExtendedBackgroundIdleModeSelector :: Selector '[Bool] ()
setShouldUseExtendedBackgroundIdleModeSelector = mkSelector "setShouldUseExtendedBackgroundIdleMode:"

-- | @Selector@ for @protocolClasses@
protocolClassesSelector :: Selector '[] (Id NSArray)
protocolClassesSelector = mkSelector "protocolClasses"

-- | @Selector@ for @setProtocolClasses:@
setProtocolClassesSelector :: Selector '[Id NSArray] ()
setProtocolClassesSelector = mkSelector "setProtocolClasses:"

-- | @Selector@ for @multipathServiceType@
multipathServiceTypeSelector :: Selector '[] NSURLSessionMultipathServiceType
multipathServiceTypeSelector = mkSelector "multipathServiceType"

-- | @Selector@ for @setMultipathServiceType:@
setMultipathServiceTypeSelector :: Selector '[NSURLSessionMultipathServiceType] ()
setMultipathServiceTypeSelector = mkSelector "setMultipathServiceType:"

-- | @Selector@ for @usesClassicLoadingMode@
usesClassicLoadingModeSelector :: Selector '[] Bool
usesClassicLoadingModeSelector = mkSelector "usesClassicLoadingMode"

-- | @Selector@ for @setUsesClassicLoadingMode:@
setUsesClassicLoadingModeSelector :: Selector '[Bool] ()
setUsesClassicLoadingModeSelector = mkSelector "setUsesClassicLoadingMode:"

-- | @Selector@ for @enablesEarlyData@
enablesEarlyDataSelector :: Selector '[] Bool
enablesEarlyDataSelector = mkSelector "enablesEarlyData"

-- | @Selector@ for @setEnablesEarlyData:@
setEnablesEarlyDataSelector :: Selector '[Bool] ()
setEnablesEarlyDataSelector = mkSelector "setEnablesEarlyData:"

