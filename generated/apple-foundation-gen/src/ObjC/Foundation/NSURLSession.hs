{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLSession@.
module ObjC.Foundation.NSURLSession
  ( NSURLSession
  , IsNSURLSession(..)
  , sessionWithConfiguration
  , sessionWithConfiguration_delegate_delegateQueue
  , finishTasksAndInvalidate
  , invalidateAndCancel
  , resetWithCompletionHandler
  , flushWithCompletionHandler
  , dataTaskWithRequest
  , dataTaskWithURL
  , uploadTaskWithRequest_fromFile
  , uploadTaskWithRequest_fromData
  , uploadTaskWithResumeData
  , uploadTaskWithStreamedRequest
  , downloadTaskWithRequest
  , downloadTaskWithURL
  , downloadTaskWithResumeData
  , streamTaskWithHostName_port
  , streamTaskWithNetService
  , webSocketTaskWithURL
  , webSocketTaskWithURL_protocols
  , webSocketTaskWithRequest
  , init_
  , new
  , dataTaskWithRequest_completionHandler
  , dataTaskWithURL_completionHandler
  , uploadTaskWithRequest_fromFile_completionHandler
  , uploadTaskWithRequest_fromData_completionHandler
  , uploadTaskWithResumeData_completionHandler
  , downloadTaskWithRequest_completionHandler
  , downloadTaskWithURL_completionHandler
  , downloadTaskWithResumeData_completionHandler
  , sharedSession
  , delegateQueue
  , delegate
  , configuration
  , sessionDescription
  , setSessionDescription
  , configurationSelector
  , dataTaskWithRequestSelector
  , dataTaskWithRequest_completionHandlerSelector
  , dataTaskWithURLSelector
  , dataTaskWithURL_completionHandlerSelector
  , delegateQueueSelector
  , delegateSelector
  , downloadTaskWithRequestSelector
  , downloadTaskWithRequest_completionHandlerSelector
  , downloadTaskWithResumeDataSelector
  , downloadTaskWithResumeData_completionHandlerSelector
  , downloadTaskWithURLSelector
  , downloadTaskWithURL_completionHandlerSelector
  , finishTasksAndInvalidateSelector
  , flushWithCompletionHandlerSelector
  , initSelector
  , invalidateAndCancelSelector
  , newSelector
  , resetWithCompletionHandlerSelector
  , sessionDescriptionSelector
  , sessionWithConfigurationSelector
  , sessionWithConfiguration_delegate_delegateQueueSelector
  , setSessionDescriptionSelector
  , sharedSessionSelector
  , streamTaskWithHostName_portSelector
  , streamTaskWithNetServiceSelector
  , uploadTaskWithRequest_fromDataSelector
  , uploadTaskWithRequest_fromData_completionHandlerSelector
  , uploadTaskWithRequest_fromFileSelector
  , uploadTaskWithRequest_fromFile_completionHandlerSelector
  , uploadTaskWithResumeDataSelector
  , uploadTaskWithResumeData_completionHandlerSelector
  , uploadTaskWithStreamedRequestSelector
  , webSocketTaskWithRequestSelector
  , webSocketTaskWithURLSelector
  , webSocketTaskWithURL_protocolsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ sessionWithConfiguration:@
sessionWithConfiguration :: IsNSURLSessionConfiguration configuration => configuration -> IO (Id NSURLSession)
sessionWithConfiguration configuration =
  do
    cls' <- getRequiredClass "NSURLSession"
    sendClassMessage cls' sessionWithConfigurationSelector (toNSURLSessionConfiguration configuration)

-- | @+ sessionWithConfiguration:delegate:delegateQueue:@
sessionWithConfiguration_delegate_delegateQueue :: (IsNSURLSessionConfiguration configuration, IsNSOperationQueue queue) => configuration -> RawId -> queue -> IO (Id NSURLSession)
sessionWithConfiguration_delegate_delegateQueue configuration delegate queue =
  do
    cls' <- getRequiredClass "NSURLSession"
    sendClassMessage cls' sessionWithConfiguration_delegate_delegateQueueSelector (toNSURLSessionConfiguration configuration) delegate (toNSOperationQueue queue)

-- | @- finishTasksAndInvalidate@
finishTasksAndInvalidate :: IsNSURLSession nsurlSession => nsurlSession -> IO ()
finishTasksAndInvalidate nsurlSession =
  sendMessage nsurlSession finishTasksAndInvalidateSelector

-- | @- invalidateAndCancel@
invalidateAndCancel :: IsNSURLSession nsurlSession => nsurlSession -> IO ()
invalidateAndCancel nsurlSession =
  sendMessage nsurlSession invalidateAndCancelSelector

-- | @- resetWithCompletionHandler:@
resetWithCompletionHandler :: IsNSURLSession nsurlSession => nsurlSession -> Ptr () -> IO ()
resetWithCompletionHandler nsurlSession completionHandler =
  sendMessage nsurlSession resetWithCompletionHandlerSelector completionHandler

-- | @- flushWithCompletionHandler:@
flushWithCompletionHandler :: IsNSURLSession nsurlSession => nsurlSession -> Ptr () -> IO ()
flushWithCompletionHandler nsurlSession completionHandler =
  sendMessage nsurlSession flushWithCompletionHandlerSelector completionHandler

-- | @- dataTaskWithRequest:@
dataTaskWithRequest :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> IO (Id NSURLSessionDataTask)
dataTaskWithRequest nsurlSession request =
  sendMessage nsurlSession dataTaskWithRequestSelector (toNSURLRequest request)

-- | @- dataTaskWithURL:@
dataTaskWithURL :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> IO (Id NSURLSessionDataTask)
dataTaskWithURL nsurlSession url =
  sendMessage nsurlSession dataTaskWithURLSelector (toNSURL url)

-- | @- uploadTaskWithRequest:fromFile:@
uploadTaskWithRequest_fromFile :: (IsNSURLSession nsurlSession, IsNSURLRequest request, IsNSURL fileURL) => nsurlSession -> request -> fileURL -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile nsurlSession request fileURL =
  sendMessage nsurlSession uploadTaskWithRequest_fromFileSelector (toNSURLRequest request) (toNSURL fileURL)

-- | @- uploadTaskWithRequest:fromData:@
uploadTaskWithRequest_fromData :: (IsNSURLSession nsurlSession, IsNSURLRequest request, IsNSData bodyData) => nsurlSession -> request -> bodyData -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData nsurlSession request bodyData =
  sendMessage nsurlSession uploadTaskWithRequest_fromDataSelector (toNSURLRequest request) (toNSData bodyData)

-- | Creates an upload task from a resume data blob. Requires the server to support the latest resumable uploads Internet-Draft from the HTTP Working Group, found at https://datatracker.ietf.org/doc/draft-ietf-httpbis-resumable-upload/ If resuming from an upload file, the file must still exist and be unmodified. If the upload cannot be successfully resumed, URLSession:task:didCompleteWithError: will be called.
--
-- - Parameter resumeData: Resume data blob from an incomplete upload, such as data returned by the cancelByProducingResumeData: method. - Returns: A new session upload task, or nil if the resumeData is invalid.
--
-- ObjC selector: @- uploadTaskWithResumeData:@
uploadTaskWithResumeData :: (IsNSURLSession nsurlSession, IsNSData resumeData) => nsurlSession -> resumeData -> IO (Id NSURLSessionUploadTask)
uploadTaskWithResumeData nsurlSession resumeData =
  sendMessage nsurlSession uploadTaskWithResumeDataSelector (toNSData resumeData)

-- | @- uploadTaskWithStreamedRequest:@
uploadTaskWithStreamedRequest :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> IO (Id NSURLSessionUploadTask)
uploadTaskWithStreamedRequest nsurlSession request =
  sendMessage nsurlSession uploadTaskWithStreamedRequestSelector (toNSURLRequest request)

-- | @- downloadTaskWithRequest:@
downloadTaskWithRequest :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithRequest nsurlSession request =
  sendMessage nsurlSession downloadTaskWithRequestSelector (toNSURLRequest request)

-- | @- downloadTaskWithURL:@
downloadTaskWithURL :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithURL nsurlSession url =
  sendMessage nsurlSession downloadTaskWithURLSelector (toNSURL url)

-- | @- downloadTaskWithResumeData:@
downloadTaskWithResumeData :: (IsNSURLSession nsurlSession, IsNSData resumeData) => nsurlSession -> resumeData -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData nsurlSession resumeData =
  sendMessage nsurlSession downloadTaskWithResumeDataSelector (toNSData resumeData)

-- | @- streamTaskWithHostName:port:@
streamTaskWithHostName_port :: (IsNSURLSession nsurlSession, IsNSString hostname) => nsurlSession -> hostname -> CLong -> IO (Id NSURLSessionStreamTask)
streamTaskWithHostName_port nsurlSession hostname port =
  sendMessage nsurlSession streamTaskWithHostName_portSelector (toNSString hostname) port

-- | @- streamTaskWithNetService:@
streamTaskWithNetService :: (IsNSURLSession nsurlSession, IsNSNetService service) => nsurlSession -> service -> IO (Id NSURLSessionStreamTask)
streamTaskWithNetService nsurlSession service =
  sendMessage nsurlSession streamTaskWithNetServiceSelector (toNSNetService service)

-- | @- webSocketTaskWithURL:@
webSocketTaskWithURL :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> IO (Id NSURLSessionWebSocketTask)
webSocketTaskWithURL nsurlSession url =
  sendMessage nsurlSession webSocketTaskWithURLSelector (toNSURL url)

-- | @- webSocketTaskWithURL:protocols:@
webSocketTaskWithURL_protocols :: (IsNSURLSession nsurlSession, IsNSURL url, IsNSArray protocols) => nsurlSession -> url -> protocols -> IO (Id NSURLSessionWebSocketTask)
webSocketTaskWithURL_protocols nsurlSession url protocols =
  sendMessage nsurlSession webSocketTaskWithURL_protocolsSelector (toNSURL url) (toNSArray protocols)

-- | @- webSocketTaskWithRequest:@
webSocketTaskWithRequest :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> IO (Id NSURLSessionWebSocketTask)
webSocketTaskWithRequest nsurlSession request =
  sendMessage nsurlSession webSocketTaskWithRequestSelector (toNSURLRequest request)

-- | @- init@
init_ :: IsNSURLSession nsurlSession => nsurlSession -> IO (Id NSURLSession)
init_ nsurlSession =
  sendOwnedMessage nsurlSession initSelector

-- | @+ new@
new :: IO (Id NSURLSession)
new  =
  do
    cls' <- getRequiredClass "NSURLSession"
    sendOwnedClassMessage cls' newSelector

-- | @- dataTaskWithRequest:completionHandler:@
dataTaskWithRequest_completionHandler :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> Ptr () -> IO (Id NSURLSessionDataTask)
dataTaskWithRequest_completionHandler nsurlSession request completionHandler =
  sendMessage nsurlSession dataTaskWithRequest_completionHandlerSelector (toNSURLRequest request) completionHandler

-- | @- dataTaskWithURL:completionHandler:@
dataTaskWithURL_completionHandler :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> Ptr () -> IO (Id NSURLSessionDataTask)
dataTaskWithURL_completionHandler nsurlSession url completionHandler =
  sendMessage nsurlSession dataTaskWithURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- uploadTaskWithRequest:fromFile:completionHandler:@
uploadTaskWithRequest_fromFile_completionHandler :: (IsNSURLSession nsurlSession, IsNSURLRequest request, IsNSURL fileURL) => nsurlSession -> request -> fileURL -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile_completionHandler nsurlSession request fileURL completionHandler =
  sendMessage nsurlSession uploadTaskWithRequest_fromFile_completionHandlerSelector (toNSURLRequest request) (toNSURL fileURL) completionHandler

-- | @- uploadTaskWithRequest:fromData:completionHandler:@
uploadTaskWithRequest_fromData_completionHandler :: (IsNSURLSession nsurlSession, IsNSURLRequest request, IsNSData bodyData) => nsurlSession -> request -> bodyData -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData_completionHandler nsurlSession request bodyData completionHandler =
  sendMessage nsurlSession uploadTaskWithRequest_fromData_completionHandlerSelector (toNSURLRequest request) (toNSData bodyData) completionHandler

-- | Creates a URLSessionUploadTask from a resume data blob. If resuming from an upload file, the file must still exist and be unmodified.
--
-- - Parameter resumeData: Resume data blob from an incomplete upload, such as data returned by the cancelByProducingResumeData: method. - Parameter completionHandler: The completion handler to call when the load request is complete. - Returns: A new session upload task, or nil if the resumeData is invalid.
--
-- ObjC selector: @- uploadTaskWithResumeData:completionHandler:@
uploadTaskWithResumeData_completionHandler :: (IsNSURLSession nsurlSession, IsNSData resumeData) => nsurlSession -> resumeData -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithResumeData_completionHandler nsurlSession resumeData completionHandler =
  sendMessage nsurlSession uploadTaskWithResumeData_completionHandlerSelector (toNSData resumeData) completionHandler

-- | @- downloadTaskWithRequest:completionHandler:@
downloadTaskWithRequest_completionHandler :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithRequest_completionHandler nsurlSession request completionHandler =
  sendMessage nsurlSession downloadTaskWithRequest_completionHandlerSelector (toNSURLRequest request) completionHandler

-- | @- downloadTaskWithURL:completionHandler:@
downloadTaskWithURL_completionHandler :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithURL_completionHandler nsurlSession url completionHandler =
  sendMessage nsurlSession downloadTaskWithURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- downloadTaskWithResumeData:completionHandler:@
downloadTaskWithResumeData_completionHandler :: (IsNSURLSession nsurlSession, IsNSData resumeData) => nsurlSession -> resumeData -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData_completionHandler nsurlSession resumeData completionHandler =
  sendMessage nsurlSession downloadTaskWithResumeData_completionHandlerSelector (toNSData resumeData) completionHandler

-- | @+ sharedSession@
sharedSession :: IO (Id NSURLSession)
sharedSession  =
  do
    cls' <- getRequiredClass "NSURLSession"
    sendClassMessage cls' sharedSessionSelector

-- | @- delegateQueue@
delegateQueue :: IsNSURLSession nsurlSession => nsurlSession -> IO (Id NSOperationQueue)
delegateQueue nsurlSession =
  sendMessage nsurlSession delegateQueueSelector

-- | @- delegate@
delegate :: IsNSURLSession nsurlSession => nsurlSession -> IO RawId
delegate nsurlSession =
  sendMessage nsurlSession delegateSelector

-- | @- configuration@
configuration :: IsNSURLSession nsurlSession => nsurlSession -> IO (Id NSURLSessionConfiguration)
configuration nsurlSession =
  sendMessage nsurlSession configurationSelector

-- | @- sessionDescription@
sessionDescription :: IsNSURLSession nsurlSession => nsurlSession -> IO (Id NSString)
sessionDescription nsurlSession =
  sendMessage nsurlSession sessionDescriptionSelector

-- | @- setSessionDescription:@
setSessionDescription :: (IsNSURLSession nsurlSession, IsNSString value) => nsurlSession -> value -> IO ()
setSessionDescription nsurlSession value =
  sendMessage nsurlSession setSessionDescriptionSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionWithConfiguration:@
sessionWithConfigurationSelector :: Selector '[Id NSURLSessionConfiguration] (Id NSURLSession)
sessionWithConfigurationSelector = mkSelector "sessionWithConfiguration:"

-- | @Selector@ for @sessionWithConfiguration:delegate:delegateQueue:@
sessionWithConfiguration_delegate_delegateQueueSelector :: Selector '[Id NSURLSessionConfiguration, RawId, Id NSOperationQueue] (Id NSURLSession)
sessionWithConfiguration_delegate_delegateQueueSelector = mkSelector "sessionWithConfiguration:delegate:delegateQueue:"

-- | @Selector@ for @finishTasksAndInvalidate@
finishTasksAndInvalidateSelector :: Selector '[] ()
finishTasksAndInvalidateSelector = mkSelector "finishTasksAndInvalidate"

-- | @Selector@ for @invalidateAndCancel@
invalidateAndCancelSelector :: Selector '[] ()
invalidateAndCancelSelector = mkSelector "invalidateAndCancel"

-- | @Selector@ for @resetWithCompletionHandler:@
resetWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
resetWithCompletionHandlerSelector = mkSelector "resetWithCompletionHandler:"

-- | @Selector@ for @flushWithCompletionHandler:@
flushWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
flushWithCompletionHandlerSelector = mkSelector "flushWithCompletionHandler:"

-- | @Selector@ for @dataTaskWithRequest:@
dataTaskWithRequestSelector :: Selector '[Id NSURLRequest] (Id NSURLSessionDataTask)
dataTaskWithRequestSelector = mkSelector "dataTaskWithRequest:"

-- | @Selector@ for @dataTaskWithURL:@
dataTaskWithURLSelector :: Selector '[Id NSURL] (Id NSURLSessionDataTask)
dataTaskWithURLSelector = mkSelector "dataTaskWithURL:"

-- | @Selector@ for @uploadTaskWithRequest:fromFile:@
uploadTaskWithRequest_fromFileSelector :: Selector '[Id NSURLRequest, Id NSURL] (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFileSelector = mkSelector "uploadTaskWithRequest:fromFile:"

-- | @Selector@ for @uploadTaskWithRequest:fromData:@
uploadTaskWithRequest_fromDataSelector :: Selector '[Id NSURLRequest, Id NSData] (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromDataSelector = mkSelector "uploadTaskWithRequest:fromData:"

-- | @Selector@ for @uploadTaskWithResumeData:@
uploadTaskWithResumeDataSelector :: Selector '[Id NSData] (Id NSURLSessionUploadTask)
uploadTaskWithResumeDataSelector = mkSelector "uploadTaskWithResumeData:"

-- | @Selector@ for @uploadTaskWithStreamedRequest:@
uploadTaskWithStreamedRequestSelector :: Selector '[Id NSURLRequest] (Id NSURLSessionUploadTask)
uploadTaskWithStreamedRequestSelector = mkSelector "uploadTaskWithStreamedRequest:"

-- | @Selector@ for @downloadTaskWithRequest:@
downloadTaskWithRequestSelector :: Selector '[Id NSURLRequest] (Id NSURLSessionDownloadTask)
downloadTaskWithRequestSelector = mkSelector "downloadTaskWithRequest:"

-- | @Selector@ for @downloadTaskWithURL:@
downloadTaskWithURLSelector :: Selector '[Id NSURL] (Id NSURLSessionDownloadTask)
downloadTaskWithURLSelector = mkSelector "downloadTaskWithURL:"

-- | @Selector@ for @downloadTaskWithResumeData:@
downloadTaskWithResumeDataSelector :: Selector '[Id NSData] (Id NSURLSessionDownloadTask)
downloadTaskWithResumeDataSelector = mkSelector "downloadTaskWithResumeData:"

-- | @Selector@ for @streamTaskWithHostName:port:@
streamTaskWithHostName_portSelector :: Selector '[Id NSString, CLong] (Id NSURLSessionStreamTask)
streamTaskWithHostName_portSelector = mkSelector "streamTaskWithHostName:port:"

-- | @Selector@ for @streamTaskWithNetService:@
streamTaskWithNetServiceSelector :: Selector '[Id NSNetService] (Id NSURLSessionStreamTask)
streamTaskWithNetServiceSelector = mkSelector "streamTaskWithNetService:"

-- | @Selector@ for @webSocketTaskWithURL:@
webSocketTaskWithURLSelector :: Selector '[Id NSURL] (Id NSURLSessionWebSocketTask)
webSocketTaskWithURLSelector = mkSelector "webSocketTaskWithURL:"

-- | @Selector@ for @webSocketTaskWithURL:protocols:@
webSocketTaskWithURL_protocolsSelector :: Selector '[Id NSURL, Id NSArray] (Id NSURLSessionWebSocketTask)
webSocketTaskWithURL_protocolsSelector = mkSelector "webSocketTaskWithURL:protocols:"

-- | @Selector@ for @webSocketTaskWithRequest:@
webSocketTaskWithRequestSelector :: Selector '[Id NSURLRequest] (Id NSURLSessionWebSocketTask)
webSocketTaskWithRequestSelector = mkSelector "webSocketTaskWithRequest:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSURLSession)
newSelector = mkSelector "new"

-- | @Selector@ for @dataTaskWithRequest:completionHandler:@
dataTaskWithRequest_completionHandlerSelector :: Selector '[Id NSURLRequest, Ptr ()] (Id NSURLSessionDataTask)
dataTaskWithRequest_completionHandlerSelector = mkSelector "dataTaskWithRequest:completionHandler:"

-- | @Selector@ for @dataTaskWithURL:completionHandler:@
dataTaskWithURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] (Id NSURLSessionDataTask)
dataTaskWithURL_completionHandlerSelector = mkSelector "dataTaskWithURL:completionHandler:"

-- | @Selector@ for @uploadTaskWithRequest:fromFile:completionHandler:@
uploadTaskWithRequest_fromFile_completionHandlerSelector :: Selector '[Id NSURLRequest, Id NSURL, Ptr ()] (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile_completionHandlerSelector = mkSelector "uploadTaskWithRequest:fromFile:completionHandler:"

-- | @Selector@ for @uploadTaskWithRequest:fromData:completionHandler:@
uploadTaskWithRequest_fromData_completionHandlerSelector :: Selector '[Id NSURLRequest, Id NSData, Ptr ()] (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData_completionHandlerSelector = mkSelector "uploadTaskWithRequest:fromData:completionHandler:"

-- | @Selector@ for @uploadTaskWithResumeData:completionHandler:@
uploadTaskWithResumeData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] (Id NSURLSessionUploadTask)
uploadTaskWithResumeData_completionHandlerSelector = mkSelector "uploadTaskWithResumeData:completionHandler:"

-- | @Selector@ for @downloadTaskWithRequest:completionHandler:@
downloadTaskWithRequest_completionHandlerSelector :: Selector '[Id NSURLRequest, Ptr ()] (Id NSURLSessionDownloadTask)
downloadTaskWithRequest_completionHandlerSelector = mkSelector "downloadTaskWithRequest:completionHandler:"

-- | @Selector@ for @downloadTaskWithURL:completionHandler:@
downloadTaskWithURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] (Id NSURLSessionDownloadTask)
downloadTaskWithURL_completionHandlerSelector = mkSelector "downloadTaskWithURL:completionHandler:"

-- | @Selector@ for @downloadTaskWithResumeData:completionHandler:@
downloadTaskWithResumeData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData_completionHandlerSelector = mkSelector "downloadTaskWithResumeData:completionHandler:"

-- | @Selector@ for @sharedSession@
sharedSessionSelector :: Selector '[] (Id NSURLSession)
sharedSessionSelector = mkSelector "sharedSession"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector '[] (Id NSOperationQueue)
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id NSURLSessionConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @sessionDescription@
sessionDescriptionSelector :: Selector '[] (Id NSString)
sessionDescriptionSelector = mkSelector "sessionDescription"

-- | @Selector@ for @setSessionDescription:@
setSessionDescriptionSelector :: Selector '[Id NSString] ()
setSessionDescriptionSelector = mkSelector "setSessionDescription:"

