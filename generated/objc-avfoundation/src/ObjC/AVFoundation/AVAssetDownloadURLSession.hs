{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A subclass of NSURLSession to support AVAssetDownloadTask.
--
-- Generated bindings for @AVAssetDownloadURLSession@.
module ObjC.AVFoundation.AVAssetDownloadURLSession
  ( AVAssetDownloadURLSession
  , IsAVAssetDownloadURLSession(..)
  , sessionWithConfiguration_assetDownloadDelegate_delegateQueue
  , assetDownloadTaskWithURLAsset_destinationURL_options
  , assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_options
  , aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_options
  , assetDownloadTaskWithConfiguration
  , init_
  , new
  , sharedSession
  , sessionWithConfiguration
  , sessionWithConfiguration_delegate_delegateQueue
  , dataTaskWithRequest
  , dataTaskWithURL
  , uploadTaskWithRequest_fromFile
  , uploadTaskWithRequest_fromData
  , uploadTaskWithStreamedRequest
  , downloadTaskWithRequest
  , downloadTaskWithURL
  , downloadTaskWithResumeData
  , dataTaskWithRequest_completionHandler
  , dataTaskWithURL_completionHandler
  , uploadTaskWithRequest_fromFile_completionHandler
  , uploadTaskWithRequest_fromData_completionHandler
  , downloadTaskWithRequest_completionHandler
  , downloadTaskWithURL_completionHandler
  , downloadTaskWithResumeData_completionHandler
  , sessionWithConfiguration_assetDownloadDelegate_delegateQueueSelector
  , assetDownloadTaskWithURLAsset_destinationURL_optionsSelector
  , assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_optionsSelector
  , aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_optionsSelector
  , assetDownloadTaskWithConfigurationSelector
  , initSelector
  , newSelector
  , sharedSessionSelector
  , sessionWithConfigurationSelector
  , sessionWithConfiguration_delegate_delegateQueueSelector
  , dataTaskWithRequestSelector
  , dataTaskWithURLSelector
  , uploadTaskWithRequest_fromFileSelector
  , uploadTaskWithRequest_fromDataSelector
  , uploadTaskWithStreamedRequestSelector
  , downloadTaskWithRequestSelector
  , downloadTaskWithURLSelector
  , downloadTaskWithResumeDataSelector
  , dataTaskWithRequest_completionHandlerSelector
  , dataTaskWithURL_completionHandlerSelector
  , uploadTaskWithRequest_fromFile_completionHandlerSelector
  , uploadTaskWithRequest_fromData_completionHandlerSelector
  , downloadTaskWithRequest_completionHandlerSelector
  , downloadTaskWithURL_completionHandlerSelector
  , downloadTaskWithResumeData_completionHandlerSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates and initializes an AVAssetDownloadURLSession for use with AVAssetDownloadTasks.
--
-- - Parameter configuration: The configuration for this URLSession. Must be a background configuration. - Parameter delegate: The delegate object to handle asset download progress updates and other session related events. - Parameter delegateQueue: The queue to receive delegate callbacks on. If nil, a serial queue will be provided.
--
-- ObjC selector: @+ sessionWithConfiguration:assetDownloadDelegate:delegateQueue:@
sessionWithConfiguration_assetDownloadDelegate_delegateQueue :: (IsNSURLSessionConfiguration configuration, IsNSOperationQueue delegateQueue) => configuration -> RawId -> delegateQueue -> IO (Id AVAssetDownloadURLSession)
sessionWithConfiguration_assetDownloadDelegate_delegateQueue configuration delegate delegateQueue =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    withObjCPtr configuration $ \raw_configuration ->
      withObjCPtr delegateQueue $ \raw_delegateQueue ->
        sendClassMsg cls' (mkSelector "sessionWithConfiguration:assetDownloadDelegate:delegateQueue:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_delegateQueue :: Ptr ())] >>= retainedObject . castPtr

-- | Creates and initializes an AVAssetDownloadTask to be used with this AVAssetDownloadURLSession.
--
-- This method may return nil if the URLSession has been invalidated.
--
-- - Parameter URLAsset: The AVURLAsset to download locally. - Parameter destinationURL: The local URL to download the asset to. This must be a file URL. - Parameter options: See AVAssetDownloadTask*Key above. Configures non-default behavior for the download task. Using this parameter is required for downloading non-default media selections for HLS assets.
--
-- ObjC selector: @- assetDownloadTaskWithURLAsset:destinationURL:options:@
assetDownloadTaskWithURLAsset_destinationURL_options :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsAVURLAsset urlAsset, IsNSURL destinationURL, IsNSDictionary options) => avAssetDownloadURLSession -> urlAsset -> destinationURL -> options -> IO (Id AVAssetDownloadTask)
assetDownloadTaskWithURLAsset_destinationURL_options avAssetDownloadURLSession  urlAsset destinationURL options =
withObjCPtr urlAsset $ \raw_urlAsset ->
  withObjCPtr destinationURL $ \raw_destinationURL ->
    withObjCPtr options $ \raw_options ->
        sendMsg avAssetDownloadURLSession (mkSelector "assetDownloadTaskWithURLAsset:destinationURL:options:") (retPtr retVoid) [argPtr (castPtr raw_urlAsset :: Ptr ()), argPtr (castPtr raw_destinationURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | Creates and initializes an AVAssetDownloadTask to be used with this AVAssetDownloadURLSession.
--
-- This method may return nil if the URLSession has been invalidated.
--
-- - Parameter URLAsset: The AVURLAsset to download locally. - Parameter title: A human readable title for this asset, expected to be as suitable as possible for the user's preferred languages. Will show up in the usage pane of the settings app. - Parameter artworkData: NSData representing artwork data for this asset. Optional. Will show up in the usage pane of the settings app. Must work with +[UIImage imageWithData:]. - Parameter options: See AVAssetDownloadTask*Key above. Configures non-default behavior for the download task. Using this parameter is required for downloading non-default media selections for HLS assets.
--
-- ObjC selector: @- assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:@
assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_options :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsAVURLAsset urlAsset, IsNSString title, IsNSData artworkData, IsNSDictionary options) => avAssetDownloadURLSession -> urlAsset -> title -> artworkData -> options -> IO (Id AVAssetDownloadTask)
assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_options avAssetDownloadURLSession  urlAsset title artworkData options =
withObjCPtr urlAsset $ \raw_urlAsset ->
  withObjCPtr title $ \raw_title ->
    withObjCPtr artworkData $ \raw_artworkData ->
      withObjCPtr options $ \raw_options ->
          sendMsg avAssetDownloadURLSession (mkSelector "assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:") (retPtr retVoid) [argPtr (castPtr raw_urlAsset :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_artworkData :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | Creates and initializes an AVAggregateAssetDownloadTask to download multiple AVMediaSelections on an AVURLAsset.
--
-- This method may return nil if the URLSession has been invalidated. The value of AVAssetDownloadTaskMediaSelectionKey will be ignored.
--
-- - Parameter URLAsset: The AVURLAsset to download locally. - Parameter mediaSelections: A list of AVMediaSelections. Each AVMediaSelection will correspond to a childAssetDownloadTask. Use -[AVAsset allMediaSelections] to download all AVMediaSelections on this AVAsset. - Parameter title: A human readable title for this asset, expected to be as suitable as possible for the user's preferred languages. Will show up in the usage pane of the settings app. - Parameter artworkData: Artwork data for this asset. Optional. Will show up in the usage pane of the settings app. - Parameter options: See AVAssetDownloadTask*Key above. Configures non-default behavior for the download task.
--
-- ObjC selector: @- aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:@
aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_options :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsAVURLAsset urlAsset, IsNSArray mediaSelections, IsNSString title, IsNSData artworkData, IsNSDictionary options) => avAssetDownloadURLSession -> urlAsset -> mediaSelections -> title -> artworkData -> options -> IO (Id AVAggregateAssetDownloadTask)
aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_options avAssetDownloadURLSession  urlAsset mediaSelections title artworkData options =
withObjCPtr urlAsset $ \raw_urlAsset ->
  withObjCPtr mediaSelections $ \raw_mediaSelections ->
    withObjCPtr title $ \raw_title ->
      withObjCPtr artworkData $ \raw_artworkData ->
        withObjCPtr options $ \raw_options ->
            sendMsg avAssetDownloadURLSession (mkSelector "aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:") (retPtr retVoid) [argPtr (castPtr raw_urlAsset :: Ptr ()), argPtr (castPtr raw_mediaSelections :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_artworkData :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | Creates and initializes an AVAssetDownloadTask to be used with this AVAssetDownloadURLSession.
--
-- This method will throw an exception if the URLSession has been invalidated.
--
-- - Parameter downloadConfiguration: The configuration to be used to create the download task.
--
-- ObjC selector: @- assetDownloadTaskWithConfiguration:@
assetDownloadTaskWithConfiguration :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsAVAssetDownloadConfiguration downloadConfiguration) => avAssetDownloadURLSession -> downloadConfiguration -> IO (Id AVAssetDownloadTask)
assetDownloadTaskWithConfiguration avAssetDownloadURLSession  downloadConfiguration =
withObjCPtr downloadConfiguration $ \raw_downloadConfiguration ->
    sendMsg avAssetDownloadURLSession (mkSelector "assetDownloadTaskWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_downloadConfiguration :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsAVAssetDownloadURLSession avAssetDownloadURLSession => avAssetDownloadURLSession -> IO (Id AVAssetDownloadURLSession)
init_ avAssetDownloadURLSession  =
  sendMsg avAssetDownloadURLSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetDownloadURLSession)
new  =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ sharedSession@
sharedSession :: IO (Id NSURLSession)
sharedSession  =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    sendClassMsg cls' (mkSelector "sharedSession") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sessionWithConfiguration:@
sessionWithConfiguration :: IsNSURLSessionConfiguration configuration => configuration -> IO (Id NSURLSession)
sessionWithConfiguration configuration =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    withObjCPtr configuration $ \raw_configuration ->
      sendClassMsg cls' (mkSelector "sessionWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sessionWithConfiguration:delegate:delegateQueue:@
sessionWithConfiguration_delegate_delegateQueue :: (IsNSURLSessionConfiguration configuration, IsNSOperationQueue queue) => configuration -> RawId -> queue -> IO (Id NSURLSession)
sessionWithConfiguration_delegate_delegateQueue configuration delegate queue =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    withObjCPtr configuration $ \raw_configuration ->
      withObjCPtr queue $ \raw_queue ->
        sendClassMsg cls' (mkSelector "sessionWithConfiguration:delegate:delegateQueue:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= retainedObject . castPtr

-- | @- dataTaskWithRequest:@
dataTaskWithRequest :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> IO (Id NSURLSessionDataTask)
dataTaskWithRequest avAssetDownloadURLSession  request =
withObjCPtr request $ \raw_request ->
    sendMsg avAssetDownloadURLSession (mkSelector "dataTaskWithRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | @- dataTaskWithURL:@
dataTaskWithURL :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURL url) => avAssetDownloadURLSession -> url -> IO (Id NSURLSessionDataTask)
dataTaskWithURL avAssetDownloadURLSession  url =
withObjCPtr url $ \raw_url ->
    sendMsg avAssetDownloadURLSession (mkSelector "dataTaskWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithRequest:fromFile:@
uploadTaskWithRequest_fromFile :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request, IsNSURL fileURL) => avAssetDownloadURLSession -> request -> fileURL -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile avAssetDownloadURLSession  request fileURL =
withObjCPtr request $ \raw_request ->
  withObjCPtr fileURL $ \raw_fileURL ->
      sendMsg avAssetDownloadURLSession (mkSelector "uploadTaskWithRequest:fromFile:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_fileURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithRequest:fromData:@
uploadTaskWithRequest_fromData :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request, IsNSData bodyData) => avAssetDownloadURLSession -> request -> bodyData -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData avAssetDownloadURLSession  request bodyData =
withObjCPtr request $ \raw_request ->
  withObjCPtr bodyData $ \raw_bodyData ->
      sendMsg avAssetDownloadURLSession (mkSelector "uploadTaskWithRequest:fromData:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_bodyData :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithStreamedRequest:@
uploadTaskWithStreamedRequest :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> IO (Id NSURLSessionUploadTask)
uploadTaskWithStreamedRequest avAssetDownloadURLSession  request =
withObjCPtr request $ \raw_request ->
    sendMsg avAssetDownloadURLSession (mkSelector "uploadTaskWithStreamedRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithRequest:@
downloadTaskWithRequest :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithRequest avAssetDownloadURLSession  request =
withObjCPtr request $ \raw_request ->
    sendMsg avAssetDownloadURLSession (mkSelector "downloadTaskWithRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithURL:@
downloadTaskWithURL :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURL url) => avAssetDownloadURLSession -> url -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithURL avAssetDownloadURLSession  url =
withObjCPtr url $ \raw_url ->
    sendMsg avAssetDownloadURLSession (mkSelector "downloadTaskWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithResumeData:@
downloadTaskWithResumeData :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSData resumeData) => avAssetDownloadURLSession -> resumeData -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData avAssetDownloadURLSession  resumeData =
withObjCPtr resumeData $ \raw_resumeData ->
    sendMsg avAssetDownloadURLSession (mkSelector "downloadTaskWithResumeData:") (retPtr retVoid) [argPtr (castPtr raw_resumeData :: Ptr ())] >>= retainedObject . castPtr

-- | @- dataTaskWithRequest:completionHandler:@
dataTaskWithRequest_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> Ptr () -> IO (Id NSURLSessionDataTask)
dataTaskWithRequest_completionHandler avAssetDownloadURLSession  request completionHandler =
withObjCPtr request $ \raw_request ->
    sendMsg avAssetDownloadURLSession (mkSelector "dataTaskWithRequest:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- dataTaskWithURL:completionHandler:@
dataTaskWithURL_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURL url) => avAssetDownloadURLSession -> url -> Ptr () -> IO (Id NSURLSessionDataTask)
dataTaskWithURL_completionHandler avAssetDownloadURLSession  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg avAssetDownloadURLSession (mkSelector "dataTaskWithURL:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithRequest:fromFile:completionHandler:@
uploadTaskWithRequest_fromFile_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request, IsNSURL fileURL) => avAssetDownloadURLSession -> request -> fileURL -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile_completionHandler avAssetDownloadURLSession  request fileURL completionHandler =
withObjCPtr request $ \raw_request ->
  withObjCPtr fileURL $ \raw_fileURL ->
      sendMsg avAssetDownloadURLSession (mkSelector "uploadTaskWithRequest:fromFile:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- uploadTaskWithRequest:fromData:completionHandler:@
uploadTaskWithRequest_fromData_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request, IsNSData bodyData) => avAssetDownloadURLSession -> request -> bodyData -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData_completionHandler avAssetDownloadURLSession  request bodyData completionHandler =
withObjCPtr request $ \raw_request ->
  withObjCPtr bodyData $ \raw_bodyData ->
      sendMsg avAssetDownloadURLSession (mkSelector "uploadTaskWithRequest:fromData:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_bodyData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithRequest:completionHandler:@
downloadTaskWithRequest_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithRequest_completionHandler avAssetDownloadURLSession  request completionHandler =
withObjCPtr request $ \raw_request ->
    sendMsg avAssetDownloadURLSession (mkSelector "downloadTaskWithRequest:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithURL:completionHandler:@
downloadTaskWithURL_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURL url) => avAssetDownloadURLSession -> url -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithURL_completionHandler avAssetDownloadURLSession  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg avAssetDownloadURLSession (mkSelector "downloadTaskWithURL:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- downloadTaskWithResumeData:completionHandler:@
downloadTaskWithResumeData_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSData resumeData) => avAssetDownloadURLSession -> resumeData -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData_completionHandler avAssetDownloadURLSession  resumeData completionHandler =
withObjCPtr resumeData $ \raw_resumeData ->
    sendMsg avAssetDownloadURLSession (mkSelector "downloadTaskWithResumeData:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_resumeData :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionWithConfiguration:assetDownloadDelegate:delegateQueue:@
sessionWithConfiguration_assetDownloadDelegate_delegateQueueSelector :: Selector
sessionWithConfiguration_assetDownloadDelegate_delegateQueueSelector = mkSelector "sessionWithConfiguration:assetDownloadDelegate:delegateQueue:"

-- | @Selector@ for @assetDownloadTaskWithURLAsset:destinationURL:options:@
assetDownloadTaskWithURLAsset_destinationURL_optionsSelector :: Selector
assetDownloadTaskWithURLAsset_destinationURL_optionsSelector = mkSelector "assetDownloadTaskWithURLAsset:destinationURL:options:"

-- | @Selector@ for @assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:@
assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_optionsSelector :: Selector
assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_optionsSelector = mkSelector "assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:"

-- | @Selector@ for @aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:@
aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_optionsSelector :: Selector
aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_optionsSelector = mkSelector "aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:"

-- | @Selector@ for @assetDownloadTaskWithConfiguration:@
assetDownloadTaskWithConfigurationSelector :: Selector
assetDownloadTaskWithConfigurationSelector = mkSelector "assetDownloadTaskWithConfiguration:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sharedSession@
sharedSessionSelector :: Selector
sharedSessionSelector = mkSelector "sharedSession"

-- | @Selector@ for @sessionWithConfiguration:@
sessionWithConfigurationSelector :: Selector
sessionWithConfigurationSelector = mkSelector "sessionWithConfiguration:"

-- | @Selector@ for @sessionWithConfiguration:delegate:delegateQueue:@
sessionWithConfiguration_delegate_delegateQueueSelector :: Selector
sessionWithConfiguration_delegate_delegateQueueSelector = mkSelector "sessionWithConfiguration:delegate:delegateQueue:"

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

-- | @Selector@ for @downloadTaskWithRequest:completionHandler:@
downloadTaskWithRequest_completionHandlerSelector :: Selector
downloadTaskWithRequest_completionHandlerSelector = mkSelector "downloadTaskWithRequest:completionHandler:"

-- | @Selector@ for @downloadTaskWithURL:completionHandler:@
downloadTaskWithURL_completionHandlerSelector :: Selector
downloadTaskWithURL_completionHandlerSelector = mkSelector "downloadTaskWithURL:completionHandler:"

-- | @Selector@ for @downloadTaskWithResumeData:completionHandler:@
downloadTaskWithResumeData_completionHandlerSelector :: Selector
downloadTaskWithResumeData_completionHandlerSelector = mkSelector "downloadTaskWithResumeData:completionHandler:"

