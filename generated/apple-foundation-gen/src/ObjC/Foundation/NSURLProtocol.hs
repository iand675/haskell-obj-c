{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLProtocol
--
-- NSURLProtocol is an abstract class which provides the    basic structure for performing protocol-specific loading of URL    data. Concrete subclasses handle the specifics associated with one    or more protocols or URL schemes.
--
-- Generated bindings for @NSURLProtocol@.
module ObjC.Foundation.NSURLProtocol
  ( NSURLProtocol
  , IsNSURLProtocol(..)
  , initWithRequest_cachedResponse_client
  , canInitWithRequest
  , canonicalRequestForRequest
  , requestIsCacheEquivalent_toRequest
  , startLoading
  , stopLoading
  , propertyForKey_inRequest
  , setProperty_forKey_inRequest
  , removePropertyForKey_inRequest
  , registerClass
  , unregisterClass
  , canInitWithTask
  , initWithTask_cachedResponse_client
  , client
  , request
  , cachedResponse
  , task
  , cachedResponseSelector
  , canInitWithRequestSelector
  , canInitWithTaskSelector
  , canonicalRequestForRequestSelector
  , clientSelector
  , initWithRequest_cachedResponse_clientSelector
  , initWithTask_cachedResponse_clientSelector
  , propertyForKey_inRequestSelector
  , registerClassSelector
  , removePropertyForKey_inRequestSelector
  , requestIsCacheEquivalent_toRequestSelector
  , requestSelector
  , setProperty_forKey_inRequestSelector
  , startLoadingSelector
  , stopLoadingSelector
  , taskSelector
  , unregisterClassSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | initWithRequest:cachedResponse:client:
--
-- Initializes an NSURLProtocol given request,     cached response, and client.
--
-- @request@ — The request to load.
--
-- @cachedResponse@ — A response that has been retrieved from the    cache for the given request. The protocol implementation should    apply protocol-specific validity checks if such tests are    necessary.
--
-- @client@ — The NSURLProtocolClient object that serves as the    interface the protocol implementation can use to report results back    to the URL loading system.
--
-- ObjC selector: @- initWithRequest:cachedResponse:client:@
initWithRequest_cachedResponse_client :: (IsNSURLProtocol nsurlProtocol, IsNSURLRequest request, IsNSCachedURLResponse cachedResponse) => nsurlProtocol -> request -> cachedResponse -> RawId -> IO (Id NSURLProtocol)
initWithRequest_cachedResponse_client nsurlProtocol request cachedResponse client =
  sendOwnedMessage nsurlProtocol initWithRequest_cachedResponse_clientSelector (toNSURLRequest request) (toNSCachedURLResponse cachedResponse) client

-- | canInitWithRequest:
--
-- This method determines whether this protocol can handle    the given request.
--
-- A concrete subclass should inspect the given request and    determine whether or not the implementation can perform a load with    that request. This is an abstract method. Subclasses must provide an    implementation.
--
-- @request@ — A request to inspect.
--
-- Returns: YES if the protocol can handle the given request, NO if not.
--
-- ObjC selector: @+ canInitWithRequest:@
canInitWithRequest :: IsNSURLRequest request => request -> IO Bool
canInitWithRequest request =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' canInitWithRequestSelector (toNSURLRequest request)

-- | canonicalRequestForRequest:
--
-- This method returns a canonical version of the given    request.
--
-- It is up to each concrete protocol implementation to    define what "canonical" means. However, a protocol should    guarantee that the same input request always yields the same    canonical form. Special consideration should be given when    implementing this method since the canonical form of a request is    used to look up objects in the URL cache, a process which performs    equality checks between NSURLRequest objects.        This is an abstract method; subclasses must provide an    implementation.
--
-- @request@ — A request to make canonical.
--
-- Returns: The canonical form of the given request.
--
-- ObjC selector: @+ canonicalRequestForRequest:@
canonicalRequestForRequest :: IsNSURLRequest request => request -> IO (Id NSURLRequest)
canonicalRequestForRequest request =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' canonicalRequestForRequestSelector (toNSURLRequest request)

-- | requestIsCacheEquivalent:toRequest:
--
-- Compares two requests for equivalence with regard to caching.
--
-- Requests are considered equivalent for cache purposes    if and only if they would be handled by the same protocol AND that    protocol declares them equivalent after performing     implementation-specific checks.
--
-- Returns: YES if the two requests are cache-equivalent, NO otherwise.
--
-- ObjC selector: @+ requestIsCacheEquivalent:toRequest:@
requestIsCacheEquivalent_toRequest :: (IsNSURLRequest a, IsNSURLRequest b) => a -> b -> IO Bool
requestIsCacheEquivalent_toRequest a b =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' requestIsCacheEquivalent_toRequestSelector (toNSURLRequest a) (toNSURLRequest b)

-- | startLoading
--
-- Starts protocol-specific loading of a request.
--
-- When this method is called, the protocol implementation    should start loading a request.
--
-- ObjC selector: @- startLoading@
startLoading :: IsNSURLProtocol nsurlProtocol => nsurlProtocol -> IO ()
startLoading nsurlProtocol =
  sendMessage nsurlProtocol startLoadingSelector

-- | stopLoading
--
-- Stops protocol-specific loading of a request.
--
-- When this method is called, the protocol implementation    should end the work of loading a request. This could be in response    to a cancel operation, so protocol implementations must be able to    handle this call while a load is in progress.
--
-- ObjC selector: @- stopLoading@
stopLoading :: IsNSURLProtocol nsurlProtocol => nsurlProtocol -> IO ()
stopLoading nsurlProtocol =
  sendMessage nsurlProtocol stopLoadingSelector

-- | propertyForKey:inRequest:
--
-- Returns the property in the given request previously    stored with the given key.
--
-- The purpose of this method is to provide an interface    for protocol implementors to access protocol-specific information    associated with NSURLRequest objects.
--
-- @key@ — The string to use for the property lookup.
--
-- @request@ — The request to use for the property lookup.
--
-- Returns: The property stored with the given key, or nil if no property    had previously been stored with the given key in the given request.
--
-- ObjC selector: @+ propertyForKey:inRequest:@
propertyForKey_inRequest :: (IsNSString key, IsNSURLRequest request) => key -> request -> IO RawId
propertyForKey_inRequest key request =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' propertyForKey_inRequestSelector (toNSString key) (toNSURLRequest request)

-- | setProperty:forKey:inRequest:
--
-- Stores the given property in the given request using the    given key.
--
-- The purpose of this method is to provide an interface    for protocol implementors to customize protocol-specific    information associated with NSMutableURLRequest objects.
--
-- @value@ — The property to store.
--
-- @key@ — The string to use for the property storage.
--
-- @request@ — The request in which to store the property.
--
-- ObjC selector: @+ setProperty:forKey:inRequest:@
setProperty_forKey_inRequest :: (IsNSString key, IsNSMutableURLRequest request) => RawId -> key -> request -> IO ()
setProperty_forKey_inRequest value key request =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' setProperty_forKey_inRequestSelector value (toNSString key) (toNSMutableURLRequest request)

-- | removePropertyForKey:inRequest:
--
-- Remove any property stored under the given key
--
-- Like setProperty:forKey:inRequest: above, the purpose of this        method is to give protocol implementors the ability to store         protocol-specific information in an NSURLRequest
--
-- @key@ — The key whose value should be removed
--
-- @request@ — The request to be modified
--
-- ObjC selector: @+ removePropertyForKey:inRequest:@
removePropertyForKey_inRequest :: (IsNSString key, IsNSMutableURLRequest request) => key -> request -> IO ()
removePropertyForKey_inRequest key request =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' removePropertyForKey_inRequestSelector (toNSString key) (toNSMutableURLRequest request)

-- | registerClass:
--
-- This method registers a protocol class, making it visible    to several other NSURLProtocol class methods.
--
-- When the URL loading system begins to load a request,    each protocol class that has been registered is consulted in turn to    see if it can be initialized with a given request. The first    protocol handler class to provide a YES answer to    +canInitWithRequest: "wins" and that protocol    implementation is used to perform the URL load. There is no    guarantee that all registered protocol classes will be consulted.    Hence, it should be noted that registering a class places it first    on the list of classes that will be consulted in calls to    +canInitWithRequest:, moving it in front of all classes    that had been registered previously.    A similar design governs the process to create the canonical form    of a request with the +canonicalRequestForRequest: class    method.
--
-- @protocolClass@ — the class to register.
--
-- Returns: YES if the protocol was registered successfully, NO if not.    The only way that failure can occur is if the given class is not a    subclass of NSURLProtocol.
--
-- ObjC selector: @+ registerClass:@
registerClass :: Class -> IO Bool
registerClass protocolClass =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' registerClassSelector protocolClass

-- | unregisterClass:
--
-- This method unregisters a protocol.
--
-- After unregistration, a protocol class is no longer    consulted in calls to NSURLProtocol class methods.
--
-- @protocolClass@ — The class to unregister.
--
-- ObjC selector: @+ unregisterClass:@
unregisterClass :: Class -> IO ()
unregisterClass protocolClass =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' unregisterClassSelector protocolClass

-- | @+ canInitWithTask:@
canInitWithTask :: IsNSURLSessionTask task => task -> IO Bool
canInitWithTask task =
  do
    cls' <- getRequiredClass "NSURLProtocol"
    sendClassMessage cls' canInitWithTaskSelector (toNSURLSessionTask task)

-- | @- initWithTask:cachedResponse:client:@
initWithTask_cachedResponse_client :: (IsNSURLProtocol nsurlProtocol, IsNSURLSessionTask task, IsNSCachedURLResponse cachedResponse) => nsurlProtocol -> task -> cachedResponse -> RawId -> IO (Id NSURLProtocol)
initWithTask_cachedResponse_client nsurlProtocol task cachedResponse client =
  sendOwnedMessage nsurlProtocol initWithTask_cachedResponse_clientSelector (toNSURLSessionTask task) (toNSCachedURLResponse cachedResponse) client

-- | Returns the NSURLProtocolClient of the receiver.
--
-- Returns: The NSURLProtocolClient of the receiver.
--
-- ObjC selector: @- client@
client :: IsNSURLProtocol nsurlProtocol => nsurlProtocol -> IO RawId
client nsurlProtocol =
  sendMessage nsurlProtocol clientSelector

-- | Returns the NSURLRequest of the receiver.
--
-- Returns: The NSURLRequest of the receiver.
--
-- ObjC selector: @- request@
request :: IsNSURLProtocol nsurlProtocol => nsurlProtocol -> IO (Id NSURLRequest)
request nsurlProtocol =
  sendMessage nsurlProtocol requestSelector

-- | Returns the NSCachedURLResponse of the receiver.
--
-- Returns: The NSCachedURLResponse of the receiver.
--
-- ObjC selector: @- cachedResponse@
cachedResponse :: IsNSURLProtocol nsurlProtocol => nsurlProtocol -> IO (Id NSCachedURLResponse)
cachedResponse nsurlProtocol =
  sendMessage nsurlProtocol cachedResponseSelector

-- | @- task@
task :: IsNSURLProtocol nsurlProtocol => nsurlProtocol -> IO (Id NSURLSessionTask)
task nsurlProtocol =
  sendMessage nsurlProtocol taskSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRequest:cachedResponse:client:@
initWithRequest_cachedResponse_clientSelector :: Selector '[Id NSURLRequest, Id NSCachedURLResponse, RawId] (Id NSURLProtocol)
initWithRequest_cachedResponse_clientSelector = mkSelector "initWithRequest:cachedResponse:client:"

-- | @Selector@ for @canInitWithRequest:@
canInitWithRequestSelector :: Selector '[Id NSURLRequest] Bool
canInitWithRequestSelector = mkSelector "canInitWithRequest:"

-- | @Selector@ for @canonicalRequestForRequest:@
canonicalRequestForRequestSelector :: Selector '[Id NSURLRequest] (Id NSURLRequest)
canonicalRequestForRequestSelector = mkSelector "canonicalRequestForRequest:"

-- | @Selector@ for @requestIsCacheEquivalent:toRequest:@
requestIsCacheEquivalent_toRequestSelector :: Selector '[Id NSURLRequest, Id NSURLRequest] Bool
requestIsCacheEquivalent_toRequestSelector = mkSelector "requestIsCacheEquivalent:toRequest:"

-- | @Selector@ for @startLoading@
startLoadingSelector :: Selector '[] ()
startLoadingSelector = mkSelector "startLoading"

-- | @Selector@ for @stopLoading@
stopLoadingSelector :: Selector '[] ()
stopLoadingSelector = mkSelector "stopLoading"

-- | @Selector@ for @propertyForKey:inRequest:@
propertyForKey_inRequestSelector :: Selector '[Id NSString, Id NSURLRequest] RawId
propertyForKey_inRequestSelector = mkSelector "propertyForKey:inRequest:"

-- | @Selector@ for @setProperty:forKey:inRequest:@
setProperty_forKey_inRequestSelector :: Selector '[RawId, Id NSString, Id NSMutableURLRequest] ()
setProperty_forKey_inRequestSelector = mkSelector "setProperty:forKey:inRequest:"

-- | @Selector@ for @removePropertyForKey:inRequest:@
removePropertyForKey_inRequestSelector :: Selector '[Id NSString, Id NSMutableURLRequest] ()
removePropertyForKey_inRequestSelector = mkSelector "removePropertyForKey:inRequest:"

-- | @Selector@ for @registerClass:@
registerClassSelector :: Selector '[Class] Bool
registerClassSelector = mkSelector "registerClass:"

-- | @Selector@ for @unregisterClass:@
unregisterClassSelector :: Selector '[Class] ()
unregisterClassSelector = mkSelector "unregisterClass:"

-- | @Selector@ for @canInitWithTask:@
canInitWithTaskSelector :: Selector '[Id NSURLSessionTask] Bool
canInitWithTaskSelector = mkSelector "canInitWithTask:"

-- | @Selector@ for @initWithTask:cachedResponse:client:@
initWithTask_cachedResponse_clientSelector :: Selector '[Id NSURLSessionTask, Id NSCachedURLResponse, RawId] (Id NSURLProtocol)
initWithTask_cachedResponse_clientSelector = mkSelector "initWithTask:cachedResponse:client:"

-- | @Selector@ for @client@
clientSelector :: Selector '[] RawId
clientSelector = mkSelector "client"

-- | @Selector@ for @request@
requestSelector :: Selector '[] (Id NSURLRequest)
requestSelector = mkSelector "request"

-- | @Selector@ for @cachedResponse@
cachedResponseSelector :: Selector '[] (Id NSCachedURLResponse)
cachedResponseSelector = mkSelector "cachedResponse"

-- | @Selector@ for @task@
taskSelector :: Selector '[] (Id NSURLSessionTask)
taskSelector = mkSelector "task"

