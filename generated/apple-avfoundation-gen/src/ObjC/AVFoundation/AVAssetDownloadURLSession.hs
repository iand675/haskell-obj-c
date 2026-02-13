{-# LANGUAGE DataKinds #-}
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
  , aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_optionsSelector
  , assetDownloadTaskWithConfigurationSelector
  , assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_optionsSelector
  , assetDownloadTaskWithURLAsset_destinationURL_optionsSelector
  , dataTaskWithRequestSelector
  , dataTaskWithRequest_completionHandlerSelector
  , dataTaskWithURLSelector
  , dataTaskWithURL_completionHandlerSelector
  , downloadTaskWithRequestSelector
  , downloadTaskWithRequest_completionHandlerSelector
  , downloadTaskWithResumeDataSelector
  , downloadTaskWithResumeData_completionHandlerSelector
  , downloadTaskWithURLSelector
  , downloadTaskWithURL_completionHandlerSelector
  , initSelector
  , newSelector
  , sessionWithConfigurationSelector
  , sessionWithConfiguration_assetDownloadDelegate_delegateQueueSelector
  , sessionWithConfiguration_delegate_delegateQueueSelector
  , sharedSessionSelector
  , uploadTaskWithRequest_fromDataSelector
  , uploadTaskWithRequest_fromData_completionHandlerSelector
  , uploadTaskWithRequest_fromFileSelector
  , uploadTaskWithRequest_fromFile_completionHandlerSelector
  , uploadTaskWithStreamedRequestSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sessionWithConfiguration_assetDownloadDelegate_delegateQueueSelector (toNSURLSessionConfiguration configuration) delegate (toNSOperationQueue delegateQueue)

-- | Creates and initializes an AVAssetDownloadTask to be used with this AVAssetDownloadURLSession.
--
-- This method may return nil if the URLSession has been invalidated.
--
-- - Parameter URLAsset: The AVURLAsset to download locally. - Parameter destinationURL: The local URL to download the asset to. This must be a file URL. - Parameter options: See AVAssetDownloadTask*Key above. Configures non-default behavior for the download task. Using this parameter is required for downloading non-default media selections for HLS assets.
--
-- ObjC selector: @- assetDownloadTaskWithURLAsset:destinationURL:options:@
assetDownloadTaskWithURLAsset_destinationURL_options :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsAVURLAsset urlAsset, IsNSURL destinationURL, IsNSDictionary options) => avAssetDownloadURLSession -> urlAsset -> destinationURL -> options -> IO (Id AVAssetDownloadTask)
assetDownloadTaskWithURLAsset_destinationURL_options avAssetDownloadURLSession urlAsset destinationURL options =
  sendMessage avAssetDownloadURLSession assetDownloadTaskWithURLAsset_destinationURL_optionsSelector (toAVURLAsset urlAsset) (toNSURL destinationURL) (toNSDictionary options)

-- | Creates and initializes an AVAssetDownloadTask to be used with this AVAssetDownloadURLSession.
--
-- This method may return nil if the URLSession has been invalidated.
--
-- - Parameter URLAsset: The AVURLAsset to download locally. - Parameter title: A human readable title for this asset, expected to be as suitable as possible for the user's preferred languages. Will show up in the usage pane of the settings app. - Parameter artworkData: NSData representing artwork data for this asset. Optional. Will show up in the usage pane of the settings app. Must work with +[UIImage imageWithData:]. - Parameter options: See AVAssetDownloadTask*Key above. Configures non-default behavior for the download task. Using this parameter is required for downloading non-default media selections for HLS assets.
--
-- ObjC selector: @- assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:@
assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_options :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsAVURLAsset urlAsset, IsNSString title, IsNSData artworkData, IsNSDictionary options) => avAssetDownloadURLSession -> urlAsset -> title -> artworkData -> options -> IO (Id AVAssetDownloadTask)
assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_options avAssetDownloadURLSession urlAsset title artworkData options =
  sendMessage avAssetDownloadURLSession assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_optionsSelector (toAVURLAsset urlAsset) (toNSString title) (toNSData artworkData) (toNSDictionary options)

-- | Creates and initializes an AVAggregateAssetDownloadTask to download multiple AVMediaSelections on an AVURLAsset.
--
-- This method may return nil if the URLSession has been invalidated. The value of AVAssetDownloadTaskMediaSelectionKey will be ignored.
--
-- - Parameter URLAsset: The AVURLAsset to download locally. - Parameter mediaSelections: A list of AVMediaSelections. Each AVMediaSelection will correspond to a childAssetDownloadTask. Use -[AVAsset allMediaSelections] to download all AVMediaSelections on this AVAsset. - Parameter title: A human readable title for this asset, expected to be as suitable as possible for the user's preferred languages. Will show up in the usage pane of the settings app. - Parameter artworkData: Artwork data for this asset. Optional. Will show up in the usage pane of the settings app. - Parameter options: See AVAssetDownloadTask*Key above. Configures non-default behavior for the download task.
--
-- ObjC selector: @- aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:@
aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_options :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsAVURLAsset urlAsset, IsNSArray mediaSelections, IsNSString title, IsNSData artworkData, IsNSDictionary options) => avAssetDownloadURLSession -> urlAsset -> mediaSelections -> title -> artworkData -> options -> IO (Id AVAggregateAssetDownloadTask)
aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_options avAssetDownloadURLSession urlAsset mediaSelections title artworkData options =
  sendMessage avAssetDownloadURLSession aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_optionsSelector (toAVURLAsset urlAsset) (toNSArray mediaSelections) (toNSString title) (toNSData artworkData) (toNSDictionary options)

-- | Creates and initializes an AVAssetDownloadTask to be used with this AVAssetDownloadURLSession.
--
-- This method will throw an exception if the URLSession has been invalidated.
--
-- - Parameter downloadConfiguration: The configuration to be used to create the download task.
--
-- ObjC selector: @- assetDownloadTaskWithConfiguration:@
assetDownloadTaskWithConfiguration :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsAVAssetDownloadConfiguration downloadConfiguration) => avAssetDownloadURLSession -> downloadConfiguration -> IO (Id AVAssetDownloadTask)
assetDownloadTaskWithConfiguration avAssetDownloadURLSession downloadConfiguration =
  sendMessage avAssetDownloadURLSession assetDownloadTaskWithConfigurationSelector (toAVAssetDownloadConfiguration downloadConfiguration)

-- | @- init@
init_ :: IsAVAssetDownloadURLSession avAssetDownloadURLSession => avAssetDownloadURLSession -> IO (Id AVAssetDownloadURLSession)
init_ avAssetDownloadURLSession =
  sendOwnedMessage avAssetDownloadURLSession initSelector

-- | @+ new@
new :: IO (Id AVAssetDownloadURLSession)
new  =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    sendOwnedClassMessage cls' newSelector

-- | @+ sharedSession@
sharedSession :: IO (Id NSURLSession)
sharedSession  =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    sendClassMessage cls' sharedSessionSelector

-- | @+ sessionWithConfiguration:@
sessionWithConfiguration :: IsNSURLSessionConfiguration configuration => configuration -> IO (Id NSURLSession)
sessionWithConfiguration configuration =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    sendClassMessage cls' sessionWithConfigurationSelector (toNSURLSessionConfiguration configuration)

-- | @+ sessionWithConfiguration:delegate:delegateQueue:@
sessionWithConfiguration_delegate_delegateQueue :: (IsNSURLSessionConfiguration configuration, IsNSOperationQueue queue) => configuration -> RawId -> queue -> IO (Id NSURLSession)
sessionWithConfiguration_delegate_delegateQueue configuration delegate queue =
  do
    cls' <- getRequiredClass "AVAssetDownloadURLSession"
    sendClassMessage cls' sessionWithConfiguration_delegate_delegateQueueSelector (toNSURLSessionConfiguration configuration) delegate (toNSOperationQueue queue)

-- | @- dataTaskWithRequest:@
dataTaskWithRequest :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> IO (Id NSURLSessionDataTask)
dataTaskWithRequest avAssetDownloadURLSession request =
  sendMessage avAssetDownloadURLSession dataTaskWithRequestSelector (toNSURLRequest request)

-- | @- dataTaskWithURL:@
dataTaskWithURL :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURL url) => avAssetDownloadURLSession -> url -> IO (Id NSURLSessionDataTask)
dataTaskWithURL avAssetDownloadURLSession url =
  sendMessage avAssetDownloadURLSession dataTaskWithURLSelector (toNSURL url)

-- | @- uploadTaskWithRequest:fromFile:@
uploadTaskWithRequest_fromFile :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request, IsNSURL fileURL) => avAssetDownloadURLSession -> request -> fileURL -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile avAssetDownloadURLSession request fileURL =
  sendMessage avAssetDownloadURLSession uploadTaskWithRequest_fromFileSelector (toNSURLRequest request) (toNSURL fileURL)

-- | @- uploadTaskWithRequest:fromData:@
uploadTaskWithRequest_fromData :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request, IsNSData bodyData) => avAssetDownloadURLSession -> request -> bodyData -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData avAssetDownloadURLSession request bodyData =
  sendMessage avAssetDownloadURLSession uploadTaskWithRequest_fromDataSelector (toNSURLRequest request) (toNSData bodyData)

-- | @- uploadTaskWithStreamedRequest:@
uploadTaskWithStreamedRequest :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> IO (Id NSURLSessionUploadTask)
uploadTaskWithStreamedRequest avAssetDownloadURLSession request =
  sendMessage avAssetDownloadURLSession uploadTaskWithStreamedRequestSelector (toNSURLRequest request)

-- | @- downloadTaskWithRequest:@
downloadTaskWithRequest :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithRequest avAssetDownloadURLSession request =
  sendMessage avAssetDownloadURLSession downloadTaskWithRequestSelector (toNSURLRequest request)

-- | @- downloadTaskWithURL:@
downloadTaskWithURL :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURL url) => avAssetDownloadURLSession -> url -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithURL avAssetDownloadURLSession url =
  sendMessage avAssetDownloadURLSession downloadTaskWithURLSelector (toNSURL url)

-- | @- downloadTaskWithResumeData:@
downloadTaskWithResumeData :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSData resumeData) => avAssetDownloadURLSession -> resumeData -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData avAssetDownloadURLSession resumeData =
  sendMessage avAssetDownloadURLSession downloadTaskWithResumeDataSelector (toNSData resumeData)

-- | @- dataTaskWithRequest:completionHandler:@
dataTaskWithRequest_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> Ptr () -> IO (Id NSURLSessionDataTask)
dataTaskWithRequest_completionHandler avAssetDownloadURLSession request completionHandler =
  sendMessage avAssetDownloadURLSession dataTaskWithRequest_completionHandlerSelector (toNSURLRequest request) completionHandler

-- | @- dataTaskWithURL:completionHandler:@
dataTaskWithURL_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURL url) => avAssetDownloadURLSession -> url -> Ptr () -> IO (Id NSURLSessionDataTask)
dataTaskWithURL_completionHandler avAssetDownloadURLSession url completionHandler =
  sendMessage avAssetDownloadURLSession dataTaskWithURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- uploadTaskWithRequest:fromFile:completionHandler:@
uploadTaskWithRequest_fromFile_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request, IsNSURL fileURL) => avAssetDownloadURLSession -> request -> fileURL -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromFile_completionHandler avAssetDownloadURLSession request fileURL completionHandler =
  sendMessage avAssetDownloadURLSession uploadTaskWithRequest_fromFile_completionHandlerSelector (toNSURLRequest request) (toNSURL fileURL) completionHandler

-- | @- uploadTaskWithRequest:fromData:completionHandler:@
uploadTaskWithRequest_fromData_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request, IsNSData bodyData) => avAssetDownloadURLSession -> request -> bodyData -> Ptr () -> IO (Id NSURLSessionUploadTask)
uploadTaskWithRequest_fromData_completionHandler avAssetDownloadURLSession request bodyData completionHandler =
  sendMessage avAssetDownloadURLSession uploadTaskWithRequest_fromData_completionHandlerSelector (toNSURLRequest request) (toNSData bodyData) completionHandler

-- | @- downloadTaskWithRequest:completionHandler:@
downloadTaskWithRequest_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURLRequest request) => avAssetDownloadURLSession -> request -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithRequest_completionHandler avAssetDownloadURLSession request completionHandler =
  sendMessage avAssetDownloadURLSession downloadTaskWithRequest_completionHandlerSelector (toNSURLRequest request) completionHandler

-- | @- downloadTaskWithURL:completionHandler:@
downloadTaskWithURL_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSURL url) => avAssetDownloadURLSession -> url -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithURL_completionHandler avAssetDownloadURLSession url completionHandler =
  sendMessage avAssetDownloadURLSession downloadTaskWithURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- downloadTaskWithResumeData:completionHandler:@
downloadTaskWithResumeData_completionHandler :: (IsAVAssetDownloadURLSession avAssetDownloadURLSession, IsNSData resumeData) => avAssetDownloadURLSession -> resumeData -> Ptr () -> IO (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData_completionHandler avAssetDownloadURLSession resumeData completionHandler =
  sendMessage avAssetDownloadURLSession downloadTaskWithResumeData_completionHandlerSelector (toNSData resumeData) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionWithConfiguration:assetDownloadDelegate:delegateQueue:@
sessionWithConfiguration_assetDownloadDelegate_delegateQueueSelector :: Selector '[Id NSURLSessionConfiguration, RawId, Id NSOperationQueue] (Id AVAssetDownloadURLSession)
sessionWithConfiguration_assetDownloadDelegate_delegateQueueSelector = mkSelector "sessionWithConfiguration:assetDownloadDelegate:delegateQueue:"

-- | @Selector@ for @assetDownloadTaskWithURLAsset:destinationURL:options:@
assetDownloadTaskWithURLAsset_destinationURL_optionsSelector :: Selector '[Id AVURLAsset, Id NSURL, Id NSDictionary] (Id AVAssetDownloadTask)
assetDownloadTaskWithURLAsset_destinationURL_optionsSelector = mkSelector "assetDownloadTaskWithURLAsset:destinationURL:options:"

-- | @Selector@ for @assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:@
assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_optionsSelector :: Selector '[Id AVURLAsset, Id NSString, Id NSData, Id NSDictionary] (Id AVAssetDownloadTask)
assetDownloadTaskWithURLAsset_assetTitle_assetArtworkData_optionsSelector = mkSelector "assetDownloadTaskWithURLAsset:assetTitle:assetArtworkData:options:"

-- | @Selector@ for @aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:@
aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_optionsSelector :: Selector '[Id AVURLAsset, Id NSArray, Id NSString, Id NSData, Id NSDictionary] (Id AVAggregateAssetDownloadTask)
aggregateAssetDownloadTaskWithURLAsset_mediaSelections_assetTitle_assetArtworkData_optionsSelector = mkSelector "aggregateAssetDownloadTaskWithURLAsset:mediaSelections:assetTitle:assetArtworkData:options:"

-- | @Selector@ for @assetDownloadTaskWithConfiguration:@
assetDownloadTaskWithConfigurationSelector :: Selector '[Id AVAssetDownloadConfiguration] (Id AVAssetDownloadTask)
assetDownloadTaskWithConfigurationSelector = mkSelector "assetDownloadTaskWithConfiguration:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetDownloadURLSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetDownloadURLSession)
newSelector = mkSelector "new"

-- | @Selector@ for @sharedSession@
sharedSessionSelector :: Selector '[] (Id NSURLSession)
sharedSessionSelector = mkSelector "sharedSession"

-- | @Selector@ for @sessionWithConfiguration:@
sessionWithConfigurationSelector :: Selector '[Id NSURLSessionConfiguration] (Id NSURLSession)
sessionWithConfigurationSelector = mkSelector "sessionWithConfiguration:"

-- | @Selector@ for @sessionWithConfiguration:delegate:delegateQueue:@
sessionWithConfiguration_delegate_delegateQueueSelector :: Selector '[Id NSURLSessionConfiguration, RawId, Id NSOperationQueue] (Id NSURLSession)
sessionWithConfiguration_delegate_delegateQueueSelector = mkSelector "sessionWithConfiguration:delegate:delegateQueue:"

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

-- | @Selector@ for @downloadTaskWithRequest:completionHandler:@
downloadTaskWithRequest_completionHandlerSelector :: Selector '[Id NSURLRequest, Ptr ()] (Id NSURLSessionDownloadTask)
downloadTaskWithRequest_completionHandlerSelector = mkSelector "downloadTaskWithRequest:completionHandler:"

-- | @Selector@ for @downloadTaskWithURL:completionHandler:@
downloadTaskWithURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] (Id NSURLSessionDownloadTask)
downloadTaskWithURL_completionHandlerSelector = mkSelector "downloadTaskWithURL:completionHandler:"

-- | @Selector@ for @downloadTaskWithResumeData:completionHandler:@
downloadTaskWithResumeData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] (Id NSURLSessionDownloadTask)
downloadTaskWithResumeData_completionHandlerSelector = mkSelector "downloadTaskWithResumeData:completionHandler:"

