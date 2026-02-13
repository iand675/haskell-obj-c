{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSURLConnection
--
-- An NSURLConnection object provides support to perform        asynchronous loads of a URL request, providing data to a        client supplied delegate.
--
-- The interface for NSURLConnection is very sparse, providing        only the controls to start and cancel asynchronous loads of a        URL request.
--
-- An NSURLConnection may be used for loading of resource data        directly to memory, in which case an        NSURLConnectionDataDelegate should be supplied, or for        downloading of resource data directly to a file, in which case        an NSURLConnectionDownloadDelegate is used.  The delegate is        retained by the NSURLConnection until a terminal condition is        encountered.  These two delegates are logically subclasses of        the base protocol, NSURLConnectionDelegate.
--
-- A terminal condition produced by the loader will result in a        connection:didFailWithError: in the case of an error, or        connectionDidFinishLoading: or connectionDidFinishDownloading:        delegate message.
--
-- The -cancel message hints to the loader that a resource load        should be abandoned but does not guarantee that more delegate        messages will not be delivered.  If -cancel does cause the        load to be abandoned, the delegate will be released without        further messages.  In general, a caller should be prepared for        -cancel to have no effect, and internally ignore any delegate        callbacks until the delegate is released.
--
-- Scheduling of an NSURLConnection specifies the context in        which delegate callbacks will be made, but the actual IO may        occur on a separate thread and should be considered an        implementation detail.
--
-- When created, an NSURLConnection performs a deep-copy of the        NSURLRequest.  This copy is available through the        -originalRequest method.  As the connection performs the load,        this request may change as a result of protocol        canonicalization or due to following redirects.        -currentRequest can be used to retrieve this value.
--
-- An NSURLConnections created with the        +connectionWithRequest:delegate: or -initWithRequest:delegate:        methods are scheduled on the current runloop immediately, and        it is not necessary to send the -start message to begin the        resource load.
--
-- NSURLConnections created with        -initWithRequest:delegate:startImmediately: are not        automatically scheduled.  Use -scheduleWithRunLoop:forMode: or        -setDelegateQueue: to specify the context for delegate        callbacks, and -start to begin the load.  If you do not        explicitly schedule the connection before -start, it will be        scheduled on the current runloop and mode automatically.
--
-- The NSURLConnectionSynchronousLoading category adds        +sendSynchronousRequest:returningResponse:error, which blocks        the current thread until the resource data is available or an        error occurs.  It should be noted that using this method on an        applications main run loop may result in an unacceptably long        delay in a user interface and its use is strongly        discourage.
--
-- The NSURLConnectionQueuedLoading category implements        +sendAsynchronousRequest:queue:completionHandler, providing        similar simplicity but provides a mechanism where the current        runloop is not blocked.
--
-- Both of the immediate loading categories do not provide for        customization of resource load, and do not allow the caller to        respond to, e.g., authentication challenges.
--
-- Generated bindings for @NSURLConnection@.
module ObjC.Foundation.NSURLConnection
  ( NSURLConnection
  , IsNSURLConnection(..)
  , initWithRequest_delegate_startImmediately
  , initWithRequest_delegate
  , connectionWithRequest_delegate
  , start
  , cancel
  , scheduleInRunLoop_forMode
  , unscheduleFromRunLoop_forMode
  , setDelegateQueue
  , canHandleRequest
  , sendAsynchronousRequest_queue_completionHandler
  , sendSynchronousRequest_returningResponse_error
  , originalRequest
  , currentRequest
  , canHandleRequestSelector
  , cancelSelector
  , connectionWithRequest_delegateSelector
  , currentRequestSelector
  , initWithRequest_delegateSelector
  , initWithRequest_delegate_startImmediatelySelector
  , originalRequestSelector
  , scheduleInRunLoop_forModeSelector
  , sendAsynchronousRequest_queue_completionHandlerSelector
  , sendSynchronousRequest_returningResponse_errorSelector
  , setDelegateQueueSelector
  , startSelector
  , unscheduleFromRunLoop_forModeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithRequest:delegate:startImmediately:@
initWithRequest_delegate_startImmediately :: (IsNSURLConnection nsurlConnection, IsNSURLRequest request) => nsurlConnection -> request -> RawId -> Bool -> IO (Id NSURLConnection)
initWithRequest_delegate_startImmediately nsurlConnection request delegate startImmediately =
  sendOwnedMessage nsurlConnection initWithRequest_delegate_startImmediatelySelector (toNSURLRequest request) delegate startImmediately

-- | @- initWithRequest:delegate:@
initWithRequest_delegate :: (IsNSURLConnection nsurlConnection, IsNSURLRequest request) => nsurlConnection -> request -> RawId -> IO (Id NSURLConnection)
initWithRequest_delegate nsurlConnection request delegate =
  sendOwnedMessage nsurlConnection initWithRequest_delegateSelector (toNSURLRequest request) delegate

-- | @+ connectionWithRequest:delegate:@
connectionWithRequest_delegate :: IsNSURLRequest request => request -> RawId -> IO (Id NSURLConnection)
connectionWithRequest_delegate request delegate =
  do
    cls' <- getRequiredClass "NSURLConnection"
    sendClassMessage cls' connectionWithRequest_delegateSelector (toNSURLRequest request) delegate

-- | @- start@
start :: IsNSURLConnection nsurlConnection => nsurlConnection -> IO ()
start nsurlConnection =
  sendMessage nsurlConnection startSelector

-- | @- cancel@
cancel :: IsNSURLConnection nsurlConnection => nsurlConnection -> IO ()
cancel nsurlConnection =
  sendMessage nsurlConnection cancelSelector

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSURLConnection nsurlConnection, IsNSRunLoop aRunLoop, IsNSString mode) => nsurlConnection -> aRunLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsurlConnection aRunLoop mode =
  sendMessage nsurlConnection scheduleInRunLoop_forModeSelector (toNSRunLoop aRunLoop) (toNSString mode)

-- | @- unscheduleFromRunLoop:forMode:@
unscheduleFromRunLoop_forMode :: (IsNSURLConnection nsurlConnection, IsNSRunLoop aRunLoop, IsNSString mode) => nsurlConnection -> aRunLoop -> mode -> IO ()
unscheduleFromRunLoop_forMode nsurlConnection aRunLoop mode =
  sendMessage nsurlConnection unscheduleFromRunLoop_forModeSelector (toNSRunLoop aRunLoop) (toNSString mode)

-- | @- setDelegateQueue:@
setDelegateQueue :: (IsNSURLConnection nsurlConnection, IsNSOperationQueue queue) => nsurlConnection -> queue -> IO ()
setDelegateQueue nsurlConnection queue =
  sendMessage nsurlConnection setDelegateQueueSelector (toNSOperationQueue queue)

-- | canHandleRequest:
--
-- Performs a "preflight" operation that performs                    some speculative checks to see if a connection can                    be initialized, and the associated I/O that is                    started in the initializer methods can begin.
--
-- The result of this method is valid only as long as                    no protocols are registered or unregistered, and                    as long as the request is not mutated (if the                    request is mutable). Hence, clients should be                    prepared to handle failures even if they have                    performed request preflighting by calling this                    method.
--
-- @request@ — The request to preflight.
--
-- Returns: YES if it is likely that the given request can be used to                    initialize a connection and the associated I/O can be                    started, NO otherwise.
--
-- ObjC selector: @+ canHandleRequest:@
canHandleRequest :: IsNSURLRequest request => request -> IO Bool
canHandleRequest request =
  do
    cls' <- getRequiredClass "NSURLConnection"
    sendClassMessage cls' canHandleRequestSelector (toNSURLRequest request)

-- | sendAsynchronousRequest:queue:completionHandler:
--
-- Performs an asynchronous load of the given                  request. When the request has completed or failed,                  the block will be executed from the context of the                  specified NSOperationQueue.
--
-- This is a convenience routine that allows for                  asynchronous loading of a url-based resource.  If                  the resource load is successful, the data parameter                  to the callback will contain the resource data and                  the error parameter will be nil.  If the resource                  load fails, the data parameter will be nil and the                  error will contain information about the failure.
--
-- @request@ — The request to load. Note that the request is                   deep-copied as part of the initialization                   process. Changes made to the request argument after                   this method returns do not affect the request that                   is used for the loading process.
--
-- @queue@ — An NSOperationQueue upon which    the handler block will                   be dispatched.
--
-- @handler@ — A block which receives the results of the resource load.
--
-- ObjC selector: @+ sendAsynchronousRequest:queue:completionHandler:@
sendAsynchronousRequest_queue_completionHandler :: (IsNSURLRequest request, IsNSOperationQueue queue) => request -> queue -> Ptr () -> IO ()
sendAsynchronousRequest_queue_completionHandler request queue handler =
  do
    cls' <- getRequiredClass "NSURLConnection"
    sendClassMessage cls' sendAsynchronousRequest_queue_completionHandlerSelector (toNSURLRequest request) (toNSOperationQueue queue) handler

-- | sendSynchronousRequest:returningResponse:error:
--
-- Performs a synchronous load of the given request,                 returning an NSURLResponse in the given out                 parameter.
--
-- A synchronous load for the given request is built on                 top of the asynchronous loading code made available                 by the class.  The calling thread is blocked while                 the asynchronous loading system performs the URL load                 on a thread spawned specifically for this load                 request. No special threading or run loop                 configuration is necessary in the calling thread in                 order to perform a synchronous load. For instance,                 the calling thread need not be running its run loop.
--
-- @request@ — The request to load. Note that the request is                 deep-copied as part of the initialization                 process. Changes made to the request argument after                 this method returns do not affect the request that is                 used for the loading process.
--
-- @response@ — An out parameter which is filled in with the                 response generated by performing the load.
--
-- @error@ — Out parameter (may be NULL) used if an error occurs                 while processing the request. Will not be modified if the                  load succeeds.
--
-- Returns: The content of the URL resulting from performing the load,                 or nil if the load failed.
--
-- ObjC selector: @+ sendSynchronousRequest:returningResponse:error:@
sendSynchronousRequest_returningResponse_error :: (IsNSURLRequest request, IsNSURLResponse response, IsNSError error_) => request -> response -> error_ -> IO (Id NSData)
sendSynchronousRequest_returningResponse_error request response error_ =
  do
    cls' <- getRequiredClass "NSURLConnection"
    sendClassMessage cls' sendSynchronousRequest_returningResponse_errorSelector (toNSURLRequest request) (toNSURLResponse response) (toNSError error_)

-- | @- originalRequest@
originalRequest :: IsNSURLConnection nsurlConnection => nsurlConnection -> IO (Id NSURLRequest)
originalRequest nsurlConnection =
  sendMessage nsurlConnection originalRequestSelector

-- | @- currentRequest@
currentRequest :: IsNSURLConnection nsurlConnection => nsurlConnection -> IO (Id NSURLRequest)
currentRequest nsurlConnection =
  sendMessage nsurlConnection currentRequestSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRequest:delegate:startImmediately:@
initWithRequest_delegate_startImmediatelySelector :: Selector '[Id NSURLRequest, RawId, Bool] (Id NSURLConnection)
initWithRequest_delegate_startImmediatelySelector = mkSelector "initWithRequest:delegate:startImmediately:"

-- | @Selector@ for @initWithRequest:delegate:@
initWithRequest_delegateSelector :: Selector '[Id NSURLRequest, RawId] (Id NSURLConnection)
initWithRequest_delegateSelector = mkSelector "initWithRequest:delegate:"

-- | @Selector@ for @connectionWithRequest:delegate:@
connectionWithRequest_delegateSelector :: Selector '[Id NSURLRequest, RawId] (Id NSURLConnection)
connectionWithRequest_delegateSelector = mkSelector "connectionWithRequest:delegate:"

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @unscheduleFromRunLoop:forMode:@
unscheduleFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
unscheduleFromRunLoop_forModeSelector = mkSelector "unscheduleFromRunLoop:forMode:"

-- | @Selector@ for @setDelegateQueue:@
setDelegateQueueSelector :: Selector '[Id NSOperationQueue] ()
setDelegateQueueSelector = mkSelector "setDelegateQueue:"

-- | @Selector@ for @canHandleRequest:@
canHandleRequestSelector :: Selector '[Id NSURLRequest] Bool
canHandleRequestSelector = mkSelector "canHandleRequest:"

-- | @Selector@ for @sendAsynchronousRequest:queue:completionHandler:@
sendAsynchronousRequest_queue_completionHandlerSelector :: Selector '[Id NSURLRequest, Id NSOperationQueue, Ptr ()] ()
sendAsynchronousRequest_queue_completionHandlerSelector = mkSelector "sendAsynchronousRequest:queue:completionHandler:"

-- | @Selector@ for @sendSynchronousRequest:returningResponse:error:@
sendSynchronousRequest_returningResponse_errorSelector :: Selector '[Id NSURLRequest, Id NSURLResponse, Id NSError] (Id NSData)
sendSynchronousRequest_returningResponse_errorSelector = mkSelector "sendSynchronousRequest:returningResponse:error:"

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector '[] (Id NSURLRequest)
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @currentRequest@
currentRequestSelector :: Selector '[] (Id NSURLRequest)
currentRequestSelector = mkSelector "currentRequest"

