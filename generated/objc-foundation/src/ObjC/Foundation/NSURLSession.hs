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
  , configuration
  , sessionDescription
  , setSessionDescription
  , sessionWithConfigurationSelector
  , sessionWithConfiguration_delegate_delegateQueueSelector
  , finishTasksAndInvalidateSelector
  , invalidateAndCancelSelector
  , resetWithCompletionHandlerSelector
  , flushWithCompletionHandlerSelector
  , dataTaskWithRequestSelector
  , dataTaskWithURLSelector
  , uploadTaskWithRequest_fromFileSelector
  , uploadTaskWithRequest_fromDataSelector
  , uploadTaskWithResumeDataSelector
  , uploadTaskWithStreamedRequestSelector
  , downloadTaskWithRequestSelector
  , downloadTaskWithURLSelector
  , downloadTaskWithResumeDataSelector
  , streamTaskWithHostName_portSelector
  , streamTaskWithNetServiceSelector
  , webSocketTaskWithURLSelector
  , webSocketTaskWithURL_protocolsSelector
  , webSocketTaskWithRequestSelector
  , initSelector
  , newSelector
  , dataTaskWithRequest_completionHandlerSelector
  , dataTaskWithURL_completionHandlerSelector
  , uploadTaskWithRequest_fromFile_completionHandlerSelector
  , uploadTaskWithRequest_fromData_completionHandlerSelector
  , uploadTaskWithResumeData_completionHandlerSelector
  , downloadTaskWithRequest_completionHandlerSelector
  , downloadTaskWithURL_completionHandlerSelector
  , downloadTaskWithResumeData_completionHandlerSelector
  , sharedSessionSelector
  , delegateQueueSelector
  , configurationSelector
  , sessionDescriptionSelector
  , setSessionDescriptionSelector


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

-- | @+ sessionWithConfiguration:@
sessionWithConfiguration :: IsNSURLSessionConfiguration configuration => configuration -> IO (Id NSURLSession)
sessionWithConfiguration configuration =
  do
    cls' <- getRequiredClass "NSURLSession"
    withObjCPtr configuration $ \raw_configuration ->
      sendClassMsg cls' (mkSelector "sessionWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sessionWithConfiguration:delegate:delegateQueue:@
sessionWithConfiguration_delegate_delegateQueue :: (IsNSURLSessionConfiguration configuration, IsNSOperationQueue queue) => configuration -> RawId -> queue -> IO (Id NSURLSession)
sessionWithConfiguration_delegate_delegateQueue configuration delegate queue =
  do
    cls' <- getRequiredClass "NSURLSession"
    withObjCPtr configuration $ \raw_configuration ->
      withObjCPtr queue $ \raw_queue ->
        sendClassMsg cls' (mkSelector "sessionWithConfiguration:delegate:delegateQueue:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= retainedObject . castPtr

-- | @- finishTasksAndInvalidate@
finishTasksAndInvalidate :: IsNSURLSession nsurlSession => nsurlSession -> IO ()
finishTasksAndInvalidate nsurlSession  =
  sendMsg nsurlSession (mkSelector "finishTasksAndInvalidate") retVoid []

-- | @- invalidateAndCancel@
invalidateAndCancel :: IsNSURLSession nsurlSession => nsurlSession -> IO ()
invalidateAndCancel nsurlSession  =
  sendMsg nsurlSession (mkSelector "invalidateAndCancel") retVoid []

-- | @- resetWithCompletionHandler:@
resetWithCompletionHandler :: IsNSURLSession nsurlSession => nsurlSession -> Ptr () -> IO ()
resetWithCompletionHandler nsurlSession  completionHandler =
  sendMsg nsurlSession (mkSelector "resetWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- flushWithCompletionHandler:@
flushWithCompletionHandler :: IsNSURLSession nsurlSession => nsurlSession -> Ptr () -> IO ()
flushWithCompletionHandler nsurlSession  completionHandler =
  sendMsg nsurlSession (mkSelector "flushWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- dataTaskWithRequest:@
dataTaskWithRequest :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> IO (Id NSURLSessionDataTask)
dataTaskWithRequest nsurlSession  request =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlSession (mkSelector "dataTaskWithRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | @- dataTaskWithURL:@
dataTaskWithURL :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> IO (Id NSURLSessionDataTask)
dataTaskWithURL nsurlSession  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsurlSession (mkSelector "dataTaskWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithRequest:fromFile:@
uploadTaskWithRequest_fromFile :: (IsNSURLSession nsurlSession, IsNSURLRequest request, IsNSURL fileURL) => nsurlSession -> request -> fileURL -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile nsurlSession  request fileURL =
withObjCPtr request $ \raw_request ->
  withObjCPtr fileURL $ \raw_fileURL ->
      sendMsg nsurlSession (mkSelector "uploadTaskWithRequest:fromFile:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_fileURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithRequest:fromData:@
uploadTaskWithRequest_fromData :: (IsNSURLSession nsurlSession, IsNSURLRequest request, IsNSData bodyData) => nsurlSession -> request -> bodyData -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData nsurlSession  request bodyData =
withObjCPtr request $ \raw_request ->
  withObjCPtr bodyData $ \raw_bodyData ->
      sendMsg nsurlSession (mkSelector "uploadTaskWithRequest:fromData:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_bodyData :: Ptr ())] >>= retainedObject . castPtr

-- | Creates an upload task from a resume data blob. Requires the server to support the latest resumable uploads Internet-Draft from the HTTP Working Group, found at https://datatracker.ietf.org/doc/draft-ietf-httpbis-resumable-upload/ If resuming from an upload file, the file must still exist and be unmodified. If the upload cannot be successfully resumed, URLSession:task:didCompleteWithError: will be called.
--
-- - Parameter resumeData: Resume data blob from an incomplete upload, such as data returned by the cancelByProducingResumeData: method. - Returns: A new session upload task, or nil if the resumeData is invalid.
--
-- ObjC selector: @- uploadTaskWithResumeData:@
uploadTaskWithResumeData :: (IsNSURLSession nsurlSession, IsNSData resumeData) => nsurlSession -> resumeData -> IO (Id NSURLSessionUploadTask)
uploadTaskWithResumeData nsurlSession  resumeData =
withObjCPtr resumeData $ \raw_resumeData ->
    sendMsg nsurlSession (mkSelector "uploadTaskWithResumeData:") (retPtr retVoid) [argPtr (castPtr raw_resumeData :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithStreamedRequest:@
uploadTaskWithStreamedRequest :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> IO (Id NSURLSessionUploadTask)
uploadTaskWithStreamedRequest nsurlSession  request =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlSession (mkSelector "uploadTaskWithStreamedRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithRequest:@
downloadTaskWithRequest :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithRequest nsurlSession  request =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlSession (mkSelector "downloadTaskWithRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithURL:@
downloadTaskWithURL :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithURL nsurlSession  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsurlSession (mkSelector "downloadTaskWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithResumeData:@
downloadTaskWithResumeData :: (IsNSURLSession nsurlSession, IsNSData resumeData) => nsurlSession -> resumeData -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData nsurlSession  resumeData =
withObjCPtr resumeData $ \raw_resumeData ->
    sendMsg nsurlSession (mkSelector "downloadTaskWithResumeData:") (retPtr retVoid) [argPtr (castPtr raw_resumeData :: Ptr ())] >>= retainedObject . castPtr

-- | @- streamTaskWithHostName:port:@
streamTaskWithHostName_port :: (IsNSURLSession nsurlSession, IsNSString hostname) => nsurlSession -> hostname -> CLong -> IO (Id NSURLSessionStreamTask)
streamTaskWithHostName_port nsurlSession  hostname port =
withObjCPtr hostname $ \raw_hostname ->
    sendMsg nsurlSession (mkSelector "streamTaskWithHostName:port:") (retPtr retVoid) [argPtr (castPtr raw_hostname :: Ptr ()), argCLong (fromIntegral port)] >>= retainedObject . castPtr

-- | @- streamTaskWithNetService:@
streamTaskWithNetService :: (IsNSURLSession nsurlSession, IsNSNetService service) => nsurlSession -> service -> IO (Id NSURLSessionStreamTask)
streamTaskWithNetService nsurlSession  service =
withObjCPtr service $ \raw_service ->
    sendMsg nsurlSession (mkSelector "streamTaskWithNetService:") (retPtr retVoid) [argPtr (castPtr raw_service :: Ptr ())] >>= retainedObject . castPtr

-- | @- webSocketTaskWithURL:@
webSocketTaskWithURL :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> IO (Id NSURLSessionWebSocketTask)
webSocketTaskWithURL nsurlSession  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsurlSession (mkSelector "webSocketTaskWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- webSocketTaskWithURL:protocols:@
webSocketTaskWithURL_protocols :: (IsNSURLSession nsurlSession, IsNSURL url, IsNSArray protocols) => nsurlSession -> url -> protocols -> IO (Id NSURLSessionWebSocketTask)
webSocketTaskWithURL_protocols nsurlSession  url protocols =
withObjCPtr url $ \raw_url ->
  withObjCPtr protocols $ \raw_protocols ->
      sendMsg nsurlSession (mkSelector "webSocketTaskWithURL:protocols:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_protocols :: Ptr ())] >>= retainedObject . castPtr

-- | @- webSocketTaskWithRequest:@
webSocketTaskWithRequest :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> IO (Id NSURLSessionWebSocketTask)
webSocketTaskWithRequest nsurlSession  request =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlSession (mkSelector "webSocketTaskWithRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSURLSession nsurlSession => nsurlSession -> IO (Id NSURLSession)
init_ nsurlSession  =
  sendMsg nsurlSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSURLSession)
new  =
  do
    cls' <- getRequiredClass "NSURLSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- dataTaskWithRequest:completionHandler:@
dataTaskWithRequest_completionHandler :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> Ptr () -> IO (Id NSURLSessionDataTask)
dataTaskWithRequest_completionHandler nsurlSession  request completionHandler =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlSession (mkSelector "dataTaskWithRequest:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- dataTaskWithURL:completionHandler:@
dataTaskWithURL_completionHandler :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> Ptr () -> IO (Id NSURLSessionDataTask)
dataTaskWithURL_completionHandler nsurlSession  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsurlSession (mkSelector "dataTaskWithURL:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithRequest:fromFile:completionHandler:@
uploadTaskWithRequest_fromFile_completionHandler :: (IsNSURLSession nsurlSession, IsNSURLRequest request, IsNSURL fileURL) => nsurlSession -> request -> fileURL -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile_completionHandler nsurlSession  request fileURL completionHandler =
withObjCPtr request $ \raw_request ->
  withObjCPtr fileURL $ \raw_fileURL ->
      sendMsg nsurlSession (mkSelector "uploadTaskWithRequest:fromFile:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithRequest:fromData:completionHandler:@
uploadTaskWithRequest_fromData_completionHandler :: (IsNSURLSession nsurlSession, IsNSURLRequest request, IsNSData bodyData) => nsurlSession -> request -> bodyData -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData_completionHandler nsurlSession  request bodyData completionHandler =
withObjCPtr request $ \raw_request ->
  withObjCPtr bodyData $ \raw_bodyData ->
      sendMsg nsurlSession (mkSelector "uploadTaskWithRequest:fromData:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_bodyData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a URLSessionUploadTask from a resume data blob. If resuming from an upload file, the file must still exist and be unmodified.
--
-- - Parameter resumeData: Resume data blob from an incomplete upload, such as data returned by the cancelByProducingResumeData: method. - Parameter completionHandler: The completion handler to call when the load request is complete. - Returns: A new session upload task, or nil if the resumeData is invalid.
--
-- ObjC selector: @- uploadTaskWithResumeData:completionHandler:@
uploadTaskWithResumeData_completionHandler :: (IsNSURLSession nsurlSession, IsNSData resumeData) => nsurlSession -> resumeData -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithResumeData_completionHandler nsurlSession  resumeData completionHandler =
withObjCPtr resumeData $ \raw_resumeData ->
    sendMsg nsurlSession (mkSelector "uploadTaskWithResumeData:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_resumeData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithRequest:completionHandler:@
downloadTaskWithRequest_completionHandler :: (IsNSURLSession nsurlSession, IsNSURLRequest request) => nsurlSession -> request -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithRequest_completionHandler nsurlSession  request completionHandler =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlSession (mkSelector "downloadTaskWithRequest:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithURL:completionHandler:@
downloadTaskWithURL_completionHandler :: (IsNSURLSession nsurlSession, IsNSURL url) => nsurlSession -> url -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithURL_completionHandler nsurlSession  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsurlSession (mkSelector "downloadTaskWithURL:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithResumeData:completionHandler:@
downloadTaskWithResumeData_completionHandler :: (IsNSURLSession nsurlSession, IsNSData resumeData) => nsurlSession -> resumeData -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData_completionHandler nsurlSession  resumeData completionHandler =
withObjCPtr resumeData $ \raw_resumeData ->
    sendMsg nsurlSession (mkSelector "downloadTaskWithResumeData:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_resumeData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sharedSession@
sharedSession :: IO (Id NSURLSession)
sharedSession  =
  do
    cls' <- getRequiredClass "NSURLSession"
    sendClassMsg cls' (mkSelector "sharedSession") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegateQueue@
delegateQueue :: IsNSURLSession nsurlSession => nsurlSession -> IO (Id NSOperationQueue)
delegateQueue nsurlSession  =
  sendMsg nsurlSession (mkSelector "delegateQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- configuration@
configuration :: IsNSURLSession nsurlSession => nsurlSession -> IO (Id NSURLSessionConfiguration)
configuration nsurlSession  =
  sendMsg nsurlSession (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sessionDescription@
sessionDescription :: IsNSURLSession nsurlSession => nsurlSession -> IO (Id NSString)
sessionDescription nsurlSession  =
  sendMsg nsurlSession (mkSelector "sessionDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSessionDescription:@
setSessionDescription :: (IsNSURLSession nsurlSession, IsNSString value) => nsurlSession -> value -> IO ()
setSessionDescription nsurlSession  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlSession (mkSelector "setSessionDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionWithConfiguration:@
sessionWithConfigurationSelector :: Selector
sessionWithConfigurationSelector = mkSelector "sessionWithConfiguration:"

-- | @Selector@ for @sessionWithConfiguration:delegate:delegateQueue:@
sessionWithConfiguration_delegate_delegateQueueSelector :: Selector
sessionWithConfiguration_delegate_delegateQueueSelector = mkSelector "sessionWithConfiguration:delegate:delegateQueue:"

-- | @Selector@ for @finishTasksAndInvalidate@
finishTasksAndInvalidateSelector :: Selector
finishTasksAndInvalidateSelector = mkSelector "finishTasksAndInvalidate"

-- | @Selector@ for @invalidateAndCancel@
invalidateAndCancelSelector :: Selector
invalidateAndCancelSelector = mkSelector "invalidateAndCancel"

-- | @Selector@ for @resetWithCompletionHandler:@
resetWithCompletionHandlerSelector :: Selector
resetWithCompletionHandlerSelector = mkSelector "resetWithCompletionHandler:"

-- | @Selector@ for @flushWithCompletionHandler:@
flushWithCompletionHandlerSelector :: Selector
flushWithCompletionHandlerSelector = mkSelector "flushWithCompletionHandler:"

-- | @Selector@ for @dataTaskWithRequest:@
dataTaskWithRequestSelector :: Selector
dataTaskWithRequestSelector = mkSelector "dataTaskWithRequest:"

-- | @Selector@ for @dataTaskWithURL:@
dataTaskWithURLSelector :: Selector
dataTaskWithURLSelector = mkSelector "dataTaskWithURL:"

-- | @Selector@ for @uploadTaskWithRequest:fromFile:@
uploadTaskWithRequest_fromFileSelector :: Selector
uploadTaskWithRequest_fromFileSelector = mkSelector "uploadTaskWithRequest:fromFile:"

-- | @Selector@ for @uploadTaskWithRequest:fromData:@
uploadTaskWithRequest_fromDataSelector :: Selector
uploadTaskWithRequest_fromDataSelector = mkSelector "uploadTaskWithRequest:fromData:"

-- | @Selector@ for @uploadTaskWithResumeData:@
uploadTaskWithResumeDataSelector :: Selector
uploadTaskWithResumeDataSelector = mkSelector "uploadTaskWithResumeData:"

-- | @Selector@ for @uploadTaskWithStreamedRequest:@
uploadTaskWithStreamedRequestSelector :: Selector
uploadTaskWithStreamedRequestSelector = mkSelector "uploadTaskWithStreamedRequest:"

-- | @Selector@ for @downloadTaskWithRequest:@
downloadTaskWithRequestSelector :: Selector
downloadTaskWithRequestSelector = mkSelector "downloadTaskWithRequest:"

-- | @Selector@ for @downloadTaskWithURL:@
downloadTaskWithURLSelector :: Selector
downloadTaskWithURLSelector = mkSelector "downloadTaskWithURL:"

-- | @Selector@ for @downloadTaskWithResumeData:@
downloadTaskWithResumeDataSelector :: Selector
downloadTaskWithResumeDataSelector = mkSelector "downloadTaskWithResumeData:"

-- | @Selector@ for @streamTaskWithHostName:port:@
streamTaskWithHostName_portSelector :: Selector
streamTaskWithHostName_portSelector = mkSelector "streamTaskWithHostName:port:"

-- | @Selector@ for @streamTaskWithNetService:@
streamTaskWithNetServiceSelector :: Selector
streamTaskWithNetServiceSelector = mkSelector "streamTaskWithNetService:"

-- | @Selector@ for @webSocketTaskWithURL:@
webSocketTaskWithURLSelector :: Selector
webSocketTaskWithURLSelector = mkSelector "webSocketTaskWithURL:"

-- | @Selector@ for @webSocketTaskWithURL:protocols:@
webSocketTaskWithURL_protocolsSelector :: Selector
webSocketTaskWithURL_protocolsSelector = mkSelector "webSocketTaskWithURL:protocols:"

-- | @Selector@ for @webSocketTaskWithRequest:@
webSocketTaskWithRequestSelector :: Selector
webSocketTaskWithRequestSelector = mkSelector "webSocketTaskWithRequest:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @dataTaskWithRequest:completionHandler:@
dataTaskWithRequest_completionHandlerSelector :: Selector
dataTaskWithRequest_completionHandlerSelector = mkSelector "dataTaskWithRequest:completionHandler:"

-- | @Selector@ for @dataTaskWithURL:completionHandler:@
dataTaskWithURL_completionHandlerSelector :: Selector
dataTaskWithURL_completionHandlerSelector = mkSelector "dataTaskWithURL:completionHandler:"

-- | @Selector@ for @uploadTaskWithRequest:fromFile:completionHandler:@
uploadTaskWithRequest_fromFile_completionHandlerSelector :: Selector
uploadTaskWithRequest_fromFile_completionHandlerSelector = mkSelector "uploadTaskWithRequest:fromFile:completionHandler:"

-- | @Selector@ for @uploadTaskWithRequest:fromData:completionHandler:@
uploadTaskWithRequest_fromData_completionHandlerSelector :: Selector
uploadTaskWithRequest_fromData_completionHandlerSelector = mkSelector "uploadTaskWithRequest:fromData:completionHandler:"

-- | @Selector@ for @uploadTaskWithResumeData:completionHandler:@
uploadTaskWithResumeData_completionHandlerSelector :: Selector
uploadTaskWithResumeData_completionHandlerSelector = mkSelector "uploadTaskWithResumeData:completionHandler:"

-- | @Selector@ for @downloadTaskWithRequest:completionHandler:@
downloadTaskWithRequest_completionHandlerSelector :: Selector
downloadTaskWithRequest_completionHandlerSelector = mkSelector "downloadTaskWithRequest:completionHandler:"

-- | @Selector@ for @downloadTaskWithURL:completionHandler:@
downloadTaskWithURL_completionHandlerSelector :: Selector
downloadTaskWithURL_completionHandlerSelector = mkSelector "downloadTaskWithURL:completionHandler:"

-- | @Selector@ for @downloadTaskWithResumeData:completionHandler:@
downloadTaskWithResumeData_completionHandlerSelector :: Selector
downloadTaskWithResumeData_completionHandlerSelector = mkSelector "downloadTaskWithResumeData:completionHandler:"

-- | @Selector@ for @sharedSession@
sharedSessionSelector :: Selector
sharedSessionSelector = mkSelector "sharedSession"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @sessionDescription@
sessionDescriptionSelector :: Selector
sessionDescriptionSelector = mkSelector "sessionDescription"

-- | @Selector@ for @setSessionDescription:@
setSessionDescriptionSelector :: Selector
setSessionDescriptionSelector = mkSelector "setSessionDescription:"

