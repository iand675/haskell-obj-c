{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerItem carries a reference to an AVAsset as well as presentation settings for that asset.
--
-- Note that inspection of media assets is provided by AVAsset. This class is intended to represent presentation state for an asset that's played by an AVPlayer and to permit observation of that state.
--
-- It is important to avoid key-value observation with a key path containing the asset's property. Observe the AVPlayerItem's property instead. For example, use the "duration" key path instead of the "asset.duration" key path.
--
-- To allow clients to add and remove their objects as key-value observers safely, AVPlayerItem serializes notifications of changes that occur dynamically during playback on the same dispatch queue on which notifications of playback state changes are serialized by its associated AVPlayer. By default, this queue is the main queue. See dispatch_get_main_queue().
--
-- Generated bindings for @AVPlayerItem@.
module ObjC.AVFoundation.AVPlayerItem
  ( AVPlayerItem
  , IsAVPlayerItem(..)
  , init_
  , new
  , playerItemWithURL
  , playerItemWithAsset
  , playerItemWithAsset_automaticallyLoadedAssetKeys
  , initWithURL
  , initWithAsset
  , initWithAsset_automaticallyLoadedAssetKeys
  , copyWithZone
  , copy
  , requestContentAuthorizationAsynchronouslyWithTimeoutInterval_completionHandler
  , cancelContentAuthorizationRequest
  , seekToDate
  , selectedMediaOptionInMediaSelectionGroup
  , addMediaDataCollector
  , removeMediaDataCollector
  , addOutput
  , removeOutput
  , accessLog
  , errorLog
  , selectMediaPresentationLanguage_forMediaSelectionGroup
  , selectedMediaPresentationLanguageForMediaSelectionGroup
  , selectMediaPresentationSetting_forMediaSelectionGroup
  , selectedMediaPresentationSettingsForMediaSelectionGroup
  , effectiveMediaPresentationSettingsForMediaSelectionGroup
  , selectMediaOption_inMediaSelectionGroup
  , selectMediaOptionAutomaticallyInMediaSelectionGroup
  , cancelPendingSeeks
  , currentDate
  , seekToDate_completionHandler
  , stepByCount
  , status
  , error_
  , integratedTimeline
  , automaticallyHandlesInterstitialEvents
  , setAutomaticallyHandlesInterstitialEvents
  , templatePlayerItem
  , authorizationRequiredForPlayback
  , applicationAuthorizedForPlayback
  , contentAuthorizedForPlayback
  , contentAuthorizationRequestStatus
  , mediaDataCollectors
  , outputs
  , preferredCustomMediaSelectionSchemes
  , setPreferredCustomMediaSelectionSchemes
  , currentMediaSelection
  , preferredPeakBitRate
  , setPreferredPeakBitRate
  , preferredPeakBitRateForExpensiveNetworks
  , setPreferredPeakBitRateForExpensiveNetworks
  , startsOnFirstEligibleVariant
  , setStartsOnFirstEligibleVariant
  , variantPreferences
  , setVariantPreferences
  , loadedTimeRanges
  , playbackLikelyToKeepUp
  , playbackBufferFull
  , playbackBufferEmpty
  , canUseNetworkResourcesForLiveStreamingWhilePaused
  , setCanUseNetworkResourcesForLiveStreamingWhilePaused
  , preferredForwardBufferDuration
  , setPreferredForwardBufferDuration
  , audioTimePitchAlgorithm
  , setAudioTimePitchAlgorithm
  , audioSpatializationAllowed
  , setAudioSpatializationAllowed
  , allowedAudioSpatializationFormats
  , setAllowedAudioSpatializationFormats
  , audioMix
  , setAudioMix
  , seekingWaitsForVideoCompositionRendering
  , setSeekingWaitsForVideoCompositionRendering
  , videoApertureMode
  , setVideoApertureMode
  , appliesPerFrameHDRDisplayMetadata
  , setAppliesPerFrameHDRDisplayMetadata
  , seekableTimeRanges
  , timebase
  , canPlayFastForward
  , canPlaySlowForward
  , canPlayReverse
  , canPlaySlowReverse
  , canPlayFastReverse
  , canStepForward
  , canStepBackward
  , automaticallyPreservesTimeOffsetFromLive
  , setAutomaticallyPreservesTimeOffsetFromLive
  , asset
  , tracks
  , automaticallyLoadedAssetKeys
  , initSelector
  , newSelector
  , playerItemWithURLSelector
  , playerItemWithAssetSelector
  , playerItemWithAsset_automaticallyLoadedAssetKeysSelector
  , initWithURLSelector
  , initWithAssetSelector
  , initWithAsset_automaticallyLoadedAssetKeysSelector
  , copyWithZoneSelector
  , copySelector
  , requestContentAuthorizationAsynchronouslyWithTimeoutInterval_completionHandlerSelector
  , cancelContentAuthorizationRequestSelector
  , seekToDateSelector
  , selectedMediaOptionInMediaSelectionGroupSelector
  , addMediaDataCollectorSelector
  , removeMediaDataCollectorSelector
  , addOutputSelector
  , removeOutputSelector
  , accessLogSelector
  , errorLogSelector
  , selectMediaPresentationLanguage_forMediaSelectionGroupSelector
  , selectedMediaPresentationLanguageForMediaSelectionGroupSelector
  , selectMediaPresentationSetting_forMediaSelectionGroupSelector
  , selectedMediaPresentationSettingsForMediaSelectionGroupSelector
  , effectiveMediaPresentationSettingsForMediaSelectionGroupSelector
  , selectMediaOption_inMediaSelectionGroupSelector
  , selectMediaOptionAutomaticallyInMediaSelectionGroupSelector
  , cancelPendingSeeksSelector
  , currentDateSelector
  , seekToDate_completionHandlerSelector
  , stepByCountSelector
  , statusSelector
  , errorSelector
  , integratedTimelineSelector
  , automaticallyHandlesInterstitialEventsSelector
  , setAutomaticallyHandlesInterstitialEventsSelector
  , templatePlayerItemSelector
  , authorizationRequiredForPlaybackSelector
  , applicationAuthorizedForPlaybackSelector
  , contentAuthorizedForPlaybackSelector
  , contentAuthorizationRequestStatusSelector
  , mediaDataCollectorsSelector
  , outputsSelector
  , preferredCustomMediaSelectionSchemesSelector
  , setPreferredCustomMediaSelectionSchemesSelector
  , currentMediaSelectionSelector
  , preferredPeakBitRateSelector
  , setPreferredPeakBitRateSelector
  , preferredPeakBitRateForExpensiveNetworksSelector
  , setPreferredPeakBitRateForExpensiveNetworksSelector
  , startsOnFirstEligibleVariantSelector
  , setStartsOnFirstEligibleVariantSelector
  , variantPreferencesSelector
  , setVariantPreferencesSelector
  , loadedTimeRangesSelector
  , playbackLikelyToKeepUpSelector
  , playbackBufferFullSelector
  , playbackBufferEmptySelector
  , canUseNetworkResourcesForLiveStreamingWhilePausedSelector
  , setCanUseNetworkResourcesForLiveStreamingWhilePausedSelector
  , preferredForwardBufferDurationSelector
  , setPreferredForwardBufferDurationSelector
  , audioTimePitchAlgorithmSelector
  , setAudioTimePitchAlgorithmSelector
  , audioSpatializationAllowedSelector
  , setAudioSpatializationAllowedSelector
  , allowedAudioSpatializationFormatsSelector
  , setAllowedAudioSpatializationFormatsSelector
  , audioMixSelector
  , setAudioMixSelector
  , seekingWaitsForVideoCompositionRenderingSelector
  , setSeekingWaitsForVideoCompositionRenderingSelector
  , videoApertureModeSelector
  , setVideoApertureModeSelector
  , appliesPerFrameHDRDisplayMetadataSelector
  , setAppliesPerFrameHDRDisplayMetadataSelector
  , seekableTimeRangesSelector
  , timebaseSelector
  , canPlayFastForwardSelector
  , canPlaySlowForwardSelector
  , canPlayReverseSelector
  , canPlaySlowReverseSelector
  , canPlayFastReverseSelector
  , canStepForwardSelector
  , canStepBackwardSelector
  , automaticallyPreservesTimeOffsetFromLiveSelector
  , setAutomaticallyPreservesTimeOffsetFromLiveSelector
  , assetSelector
  , tracksSelector
  , automaticallyLoadedAssetKeysSelector

  -- * Enum types
  , AVAudioSpatializationFormats(AVAudioSpatializationFormats)
  , pattern AVAudioSpatializationFormatNone
  , pattern AVAudioSpatializationFormatMonoAndStereo
  , pattern AVAudioSpatializationFormatMultichannel
  , pattern AVAudioSpatializationFormatMonoStereoAndMultichannel
  , AVContentAuthorizationStatus(AVContentAuthorizationStatus)
  , pattern AVContentAuthorizationUnknown
  , pattern AVContentAuthorizationCompleted
  , pattern AVContentAuthorizationCancelled
  , pattern AVContentAuthorizationTimedOut
  , pattern AVContentAuthorizationBusy
  , pattern AVContentAuthorizationNotAvailable
  , pattern AVContentAuthorizationNotPossible
  , AVPlayerItemStatus(AVPlayerItemStatus)
  , pattern AVPlayerItemStatusUnknown
  , pattern AVPlayerItemStatusReadyToPlay
  , pattern AVPlayerItemStatusFailed
  , AVVariantPreferences(AVVariantPreferences)
  , pattern AVVariantPreferenceNone
  , pattern AVVariantPreferenceScalabilityToLosslessAudio

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id AVPlayerItem)
init_ avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerItem)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns an instance of AVPlayerItem for playing a resource at the specified location.
--
-- Equivalent to +playerItemWithAsset:, passing [AVAsset assetWithURL:URL] as the value of asset.
--
-- - Parameter URL:
--
-- - Returns: An instance of AVPlayerItem.
--
-- ObjC selector: @+ playerItemWithURL:@
playerItemWithURL :: IsNSURL url => url -> IO (Id AVPlayerItem)
playerItemWithURL url =
  do
    cls' <- getRequiredClass "AVPlayerItem"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "playerItemWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an instance of AVPlayerItem for playing an AVAsset.
--
-- Equivalent to +playerItemWithAsset:automaticallyLoadedAssetKeys:, passing \@[ "duration" ] as the value of automaticallyLoadedAssetKeys.
--
-- This method, along with the companion @asset@ property, is MainActor-isolated for Swift clients because AVAsset is not Sendable. If you are using a Sendable subclass of AVAsset, such as AVURLAsset, an overload of this initializer will be chosen automatically to allow you to initialize an AVPlayerItem while not running on the main actor.
--
-- - Parameter asset:
--
-- - Returns: An instance of AVPlayerItem.
--
-- ObjC selector: @+ playerItemWithAsset:@
playerItemWithAsset :: IsAVAsset asset => asset -> IO (Id AVPlayerItem)
playerItemWithAsset asset =
  do
    cls' <- getRequiredClass "AVPlayerItem"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "playerItemWithAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an instance of AVPlayerItem for playing an AVAsset.
--
-- The value of each key in automaticallyLoadedAssetKeys will be automatically be loaded by the underlying AVAsset before the receiver achieves the status AVPlayerItemStatusReadyToPlay; i.e. when the item is ready to play, the value of -[[AVPlayerItem asset] statusOfValueForKey:error:] will be one of the terminal status values greater than AVKeyValueStatusLoading.
--
-- This method, along with the companion @asset@ property, is MainActor-isolated for Swift clients because AVAsset is not Sendable. If you are using a Sendable subclass of AVAsset, such as AVURLAsset, you can use @init(asset:automaticallyLoadedAssetKeys:)@ to initialize an AVPlayerItem while not running on the main actor.
--
-- - Parameter asset: - Parameter automaticallyLoadedAssetKeys: An NSArray of NSStrings, each representing a property key defined by AVAsset. See AVAsset.h for property keys, e.g. duration.
--
-- - Returns: An instance of AVPlayerItem.
--
-- ObjC selector: @+ playerItemWithAsset:automaticallyLoadedAssetKeys:@
playerItemWithAsset_automaticallyLoadedAssetKeys :: (IsAVAsset asset, IsNSArray automaticallyLoadedAssetKeys) => asset -> automaticallyLoadedAssetKeys -> IO (Id AVPlayerItem)
playerItemWithAsset_automaticallyLoadedAssetKeys asset automaticallyLoadedAssetKeys =
  do
    cls' <- getRequiredClass "AVPlayerItem"
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr automaticallyLoadedAssetKeys $ \raw_automaticallyLoadedAssetKeys ->
        sendClassMsg cls' (mkSelector "playerItemWithAsset:automaticallyLoadedAssetKeys:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_automaticallyLoadedAssetKeys :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an AVPlayerItem with an NSURL.
--
-- Equivalent to -initWithAsset:, passing [AVAsset assetWithURL:URL] as the value of asset.
--
-- - Parameter URL:
--
-- - Returns: An instance of AVPlayerItem
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsAVPlayerItem avPlayerItem, IsNSURL url) => avPlayerItem -> url -> IO (Id AVPlayerItem)
initWithURL avPlayerItem  url =
withObjCPtr url $ \raw_url ->
    sendMsg avPlayerItem (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes an AVPlayerItem with an AVAsset.
--
-- Equivalent to -initWithAsset:automaticallyLoadedAssetKeys:, passing \@[ "duration" ] as the value of automaticallyLoadedAssetKeys.
--
-- This method, along with the companion @asset@ property, is MainActor-isolated for Swift clients because AVAsset is not Sendable. If you are using a Sendable subclass of AVAsset, such as AVURLAsset, an overload of this initializer will be chosen automatically to allow you to initialize an AVPlayerItem while not running on the main actor.
--
-- - Parameter asset:
--
-- - Returns: An instance of AVPlayerItem
--
-- ObjC selector: @- initWithAsset:@
initWithAsset :: (IsAVPlayerItem avPlayerItem, IsAVAsset asset) => avPlayerItem -> asset -> IO (Id AVPlayerItem)
initWithAsset avPlayerItem  asset =
withObjCPtr asset $ \raw_asset ->
    sendMsg avPlayerItem (mkSelector "initWithAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes an AVPlayerItem with an AVAsset.
--
-- The value of each key in automaticallyLoadedAssetKeys will be automatically be loaded by the underlying AVAsset before the receiver achieves the status AVPlayerItemStatusReadyToPlay; i.e. when the item is ready to play, the value of -[[AVPlayerItem asset] statusOfValueForKey:error:] will be one of the terminal status values greater than AVKeyValueStatusLoading.
--
-- This method, along with the companion @asset@ property, is MainActor-isolated for Swift clients because AVAsset is not Sendable. If you are using a Sendable subclass of AVAsset, such as AVURLAsset, you can use @init(asset:automaticallyLoadedAssetKeys:)@ to initialize an AVPlayerItem while not running on the main actor.
--
-- - Parameter asset: An instance of AVAsset. - Parameter automaticallyLoadedAssetKeys: An NSArray of NSStrings, each representing a property key defined by AVAsset. See AVAsset.h for property keys, e.g. duration.
--
-- - Returns: An instance of AVPlayerItem
--
-- ObjC selector: @- initWithAsset:automaticallyLoadedAssetKeys:@
initWithAsset_automaticallyLoadedAssetKeys :: (IsAVPlayerItem avPlayerItem, IsAVAsset asset, IsNSArray automaticallyLoadedAssetKeys) => avPlayerItem -> asset -> automaticallyLoadedAssetKeys -> IO (Id AVPlayerItem)
initWithAsset_automaticallyLoadedAssetKeys avPlayerItem  asset automaticallyLoadedAssetKeys =
withObjCPtr asset $ \raw_asset ->
  withObjCPtr automaticallyLoadedAssetKeys $ \raw_automaticallyLoadedAssetKeys ->
      sendMsg avPlayerItem (mkSelector "initWithAsset:automaticallyLoadedAssetKeys:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_automaticallyLoadedAssetKeys :: Ptr ())] >>= ownedObject . castPtr

-- | @- copyWithZone:@
copyWithZone :: IsAVPlayerItem avPlayerItem => avPlayerItem -> Ptr () -> IO RawId
copyWithZone avPlayerItem  zone =
  fmap (RawId . castPtr) $ sendMsg avPlayerItem (mkSelector "copyWithZone:") (retPtr retVoid) [argPtr zone]

-- | @- copy@
copy :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO RawId
copy avPlayerItem  =
  fmap (RawId . castPtr) $ sendMsg avPlayerItem (mkSelector "copy") (retPtr retVoid) []

-- | requestContentAuthorizationAsynchronouslyWithTimeoutInterval:completionHandler:
--
-- Causes appropriate action to be taken to allow the user to authorize the content for playback.
--
-- Calling this method will present the user with the opportunity to authorize the content (e.g. by launching iTunes and prompting the user to enter their Apple ID and password). When the user has taken action (or the timeout has elapsted), the completion handler will be invoked.  The status of the authorization attempt can be determined by checking the value of the contentAuthorizationRequestStatus property.  Note that even if the status indicates a completed authorization, the content may still not be authorized (e.g. if the user authorizes an Apple ID other than that associated with the content).  The contentAuthorizedForPlayback property should be re-checked to verify whether the content has actually been authorized before continuing.  It is not necessary to call this method if the value of contentAuthorizedForPlayback is already true.
--
-- @timeoutInterval@ — The maximum amount of time to wait for the user to authorize the content in seconds before calling the handler block with a timeout result.
--
-- @handler@ — Block to be called upon completion.
--
-- ObjC selector: @- requestContentAuthorizationAsynchronouslyWithTimeoutInterval:completionHandler:@
requestContentAuthorizationAsynchronouslyWithTimeoutInterval_completionHandler :: IsAVPlayerItem avPlayerItem => avPlayerItem -> CDouble -> Ptr () -> IO ()
requestContentAuthorizationAsynchronouslyWithTimeoutInterval_completionHandler avPlayerItem  timeoutInterval handler =
  sendMsg avPlayerItem (mkSelector "requestContentAuthorizationAsynchronouslyWithTimeoutInterval:completionHandler:") retVoid [argCDouble (fromIntegral timeoutInterval), argPtr (castPtr handler :: Ptr ())]

-- | cancelContentAuthorizationRequest
--
-- Causes the currently outstanding content authorization request to be cancelled.
--
-- Calling this method while a content authorization request is pending will cause that request to be cancelled and its completion handler to be invoked with a status of AVContentAuthorizationCancelled.  This call does not block.
--
-- ObjC selector: @- cancelContentAuthorizationRequest@
cancelContentAuthorizationRequest :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO ()
cancelContentAuthorizationRequest avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "cancelContentAuthorizationRequest") retVoid []

-- | move playhead to a point corresponding to a particular date.
--
-- For playback content that is associated with a range of dates, move the playhead to point within that range. Will fail if the supplied date is outside the range or if the content is not associated with a range of dates.
--
-- - Parameter date: The new position for the playhead.
--
-- - Returns: Returns true if the playhead was moved to the supplied date.
--
-- ObjC selector: @- seekToDate:@
seekToDate :: (IsAVPlayerItem avPlayerItem, IsNSDate date) => avPlayerItem -> date -> IO Bool
seekToDate avPlayerItem  date =
withObjCPtr date $ \raw_date ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "seekToDate:") retCULong [argPtr (castPtr raw_date :: Ptr ())]

-- | Indicates the media selection option that's currently selected from the specified group. May be nil.
--
-- If the value of the property allowsEmptySelection of the AVMediaSelectionGroup is YES, the currently selected option in the group may be nil.
--
-- - Parameter mediaSelectionGroup: A media selection group obtained from the receiver's asset.
--
-- - Returns: An instance of AVMediaSelectionOption that describes the currently selection option in the group.
--
-- ObjC selector: @- selectedMediaOptionInMediaSelectionGroup:@
selectedMediaOptionInMediaSelectionGroup :: (IsAVPlayerItem avPlayerItem, IsAVMediaSelectionGroup mediaSelectionGroup) => avPlayerItem -> mediaSelectionGroup -> IO (Id AVMediaSelectionOption)
selectedMediaOptionInMediaSelectionGroup avPlayerItem  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avPlayerItem (mkSelector "selectedMediaOptionInMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | Adds the specified instance of AVPlayerItemMediaDataCollector to the receiver's collection of mediaDataCollectors.
--
-- This method may incur additional I/O to collect the requested media data asynchronously.
--
-- - Parameter collector: An instance of AVPlayerItemMediaDataCollector
--
-- ObjC selector: @- addMediaDataCollector:@
addMediaDataCollector :: (IsAVPlayerItem avPlayerItem, IsAVPlayerItemMediaDataCollector collector) => avPlayerItem -> collector -> IO ()
addMediaDataCollector avPlayerItem  collector =
withObjCPtr collector $ \raw_collector ->
    sendMsg avPlayerItem (mkSelector "addMediaDataCollector:") retVoid [argPtr (castPtr raw_collector :: Ptr ())]

-- | Removes the specified instance of AVPlayerItemMediaDataCollector from the receiver's collection of mediaDataCollectors.
--
-- - Parameter collector: An instance of AVPlayerItemMediaDataCollector
--
-- ObjC selector: @- removeMediaDataCollector:@
removeMediaDataCollector :: (IsAVPlayerItem avPlayerItem, IsAVPlayerItemMediaDataCollector collector) => avPlayerItem -> collector -> IO ()
removeMediaDataCollector avPlayerItem  collector =
withObjCPtr collector $ \raw_collector ->
    sendMsg avPlayerItem (mkSelector "removeMediaDataCollector:") retVoid [argPtr (castPtr raw_collector :: Ptr ())]

-- | Adds the specified instance of AVPlayerItemOutput to the receiver's collection of outputs.
--
-- The class of AVPlayerItemOutput provided dictates the data structure that decoded samples are vended in.
--
-- When an AVPlayerItemOutput is associated with an AVPlayerItem, samples are provided for a media type in accordance with the rules for mixing, composition, or exclusion that the AVPlayer honors among multiple enabled tracks of that media type for its own rendering purposes. For example, video media will be composed according to the instructions provided via AVPlayerItem.videoComposition, if present. Audio media will be mixed according to the parameters provided via AVPlayerItem.audioMix, if present.
--
-- - Parameter output: An instance of AVPlayerItemOutput
--
-- ObjC selector: @- addOutput:@
addOutput :: (IsAVPlayerItem avPlayerItem, IsAVPlayerItemOutput output) => avPlayerItem -> output -> IO ()
addOutput avPlayerItem  output =
withObjCPtr output $ \raw_output ->
    sendMsg avPlayerItem (mkSelector "addOutput:") retVoid [argPtr (castPtr raw_output :: Ptr ())]

-- | Removes the specified instance of AVPlayerItemOutput from the receiver's collection of outputs.
--
-- - Parameter output: An instance of AVPlayerItemOutput
--
-- ObjC selector: @- removeOutput:@
removeOutput :: (IsAVPlayerItem avPlayerItem, IsAVPlayerItemOutput output) => avPlayerItem -> output -> IO ()
removeOutput avPlayerItem  output =
withObjCPtr output $ \raw_output ->
    sendMsg avPlayerItem (mkSelector "removeOutput:") retVoid [argPtr (castPtr raw_output :: Ptr ())]

-- | Returns an object that represents a snapshot of the network access log. Can be nil.
--
-- An AVPlayerItemAccessLog provides methods to retrieve the network access log in a format suitable for serialization. If nil is returned then there is no logging information currently available for this AVPlayerItem. An AVPlayerItemNewAccessLogEntryNotification will be posted when new logging information becomes available. However, accessLog might already return a non-nil value even before the first notification is posted.
--
-- In certain situations, this method may temporarily block the calling thread during the ongoing log collection process. It is strongly recommended that the caller take appropriate measures to prevent blocking essential services such as the user interface, for example, by avoiding calling this method in the main thread.
--
-- - Returns: An autoreleased AVPlayerItemAccessLog instance.
--
-- ObjC selector: @- accessLog@
accessLog :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id AVPlayerItemAccessLog)
accessLog avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "accessLog") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns an object that represents a snapshot of the error log. Can be nil.
--
-- An AVPlayerItemErrorLog provides methods to retrieve the error log in a format suitable for serialization. If nil is returned then there is no logging information currently available for this AVPlayerItem.
--
-- In certain situations, this method may temporarily block the calling thread during the ongoing log collection process. It is strongly recommended that the caller take appropriate measures to prevent blocking essential services such as the user interface, for example, by avoiding calling this method in the main thread.
--
-- - Returns: An autoreleased AVPlayerItemErrorLog instance.
--
-- ObjC selector: @- errorLog@
errorLog :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id AVPlayerItemErrorLog)
errorLog avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "errorLog") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | When the associated AVPlayer's appliesMediaSelectionCriteriaAutomatically property is set to YES, configures the player item to prefer a particular language, replacing any previous preference for available languages of the specified group's custom media selection scheme.
--
-- Overrides preferences for languages specified by the AVPlayer's current media selection criteria. This method has no effect when the associated AVPlayer's appliesMediaSelectionCriteriaAutomatically property has a value of NO, in which case you must use -selectMediaOption:inMediaSelectionGroup: instead in order to alter the presentation state of the media.
--
-- - Parameter languages: A BCP 47 language tag, typically obtained from the availableLanguages of the AVCustomMediaSelectionScheme of the specified AVMediaSelectionGroup. - Parameter mediaSelectionGroup: The media selection group, obtained from the receiver's asset, to which the specified setting is to be applied.
--
-- ObjC selector: @- selectMediaPresentationLanguage:forMediaSelectionGroup:@
selectMediaPresentationLanguage_forMediaSelectionGroup :: (IsAVPlayerItem avPlayerItem, IsNSString language, IsAVMediaSelectionGroup mediaSelectionGroup) => avPlayerItem -> language -> mediaSelectionGroup -> IO ()
selectMediaPresentationLanguage_forMediaSelectionGroup avPlayerItem  language mediaSelectionGroup =
withObjCPtr language $ \raw_language ->
  withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
      sendMsg avPlayerItem (mkSelector "selectMediaPresentationLanguage:forMediaSelectionGroup:") retVoid [argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())]

-- | Returns the selected media presentation language for the specified media selection group, if any language has previously been selected via use of -selectMediaPresentationLanguages:forMediaSelectionGroup:.
--
-- - Parameter mediaSelectionGroup: The media selection group, obtained from the receiver's asset, for which the selected media presentation language is requested.
--
-- ObjC selector: @- selectedMediaPresentationLanguageForMediaSelectionGroup:@
selectedMediaPresentationLanguageForMediaSelectionGroup :: (IsAVPlayerItem avPlayerItem, IsAVMediaSelectionGroup mediaSelectionGroup) => avPlayerItem -> mediaSelectionGroup -> IO (Id NSString)
selectedMediaPresentationLanguageForMediaSelectionGroup avPlayerItem  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avPlayerItem (mkSelector "selectedMediaPresentationLanguageForMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | When the associated AVPlayer's appliesMediaSelectionCriteriaAutomatically property is set to YES, configures the player item to prefer a particular presentation setting, replacing any previous preference for settings of the same media presentation selector.
--
-- Note that preferences for media characteristics indicated by selected AVMediaPresentationSettings are treated as supplemental to the associated AVPlayer's media selection criteria for the AVMediaSelectionGroup. An AVPlayer's default media selection criteria can also indicate preferences for media characteristics, such as those indicating the availability of accessibility affordances such as audio descriptions, and these media characteristics can be left up to the AVPlayer to manage even when an AVCustomMediaSelectionScheme is in use. But if you wish to do so, you can use AVMediaPresentationSettings offered by a AVCustomMediaSelectionScheme in combination with custom AVPlayerMediaSelectionCriteria. If the specified setting isn't offered by an AVMediaPresentationSelector of the AVCustomMediaSelectionScheme of the specified AVMediaSelectionGroup, no change in the presentation of the media will result. This method has no effect when the associated AVPlayer's appliesMediaSelectionCriteriaAutomatically property has a value of NO, in which case you must use -selectMediaOption:inMediaSelectionGroup: instead in order to alter the presentation state of the media.
--
-- - Parameter mediaPresentationSetting: The setting to select. - Parameter mediaSelectionGroup: The media selection group, obtained from the receiver's asset, to which the specified setting is to be applied.
--
-- ObjC selector: @- selectMediaPresentationSetting:forMediaSelectionGroup:@
selectMediaPresentationSetting_forMediaSelectionGroup :: (IsAVPlayerItem avPlayerItem, IsAVMediaPresentationSetting mediaPresentationSetting, IsAVMediaSelectionGroup mediaSelectionGroup) => avPlayerItem -> mediaPresentationSetting -> mediaSelectionGroup -> IO ()
selectMediaPresentationSetting_forMediaSelectionGroup avPlayerItem  mediaPresentationSetting mediaSelectionGroup =
withObjCPtr mediaPresentationSetting $ \raw_mediaPresentationSetting ->
  withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
      sendMsg avPlayerItem (mkSelector "selectMediaPresentationSetting:forMediaSelectionGroup:") retVoid [argPtr (castPtr raw_mediaPresentationSetting :: Ptr ()), argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())]

-- | Indicates the media presentation settings that have most recently been selected for each AVMediaPresentationSelector of the AVCustomMediaSelectionScheme of the specified AVMediaSelectionGroup.
--
-- - Parameter mediaSelectionGroup: An AVMediaSelectionGroup obtained from the receiver's asset for which the currently selected media presentation settings are desired.
--
-- - Returns: A dictionary with AVMediaPresentationSelectors as keys and AVMediaPresentationSettings as values, providing the most recently selected setting for each selector or, if no setting has previously been selected, NSNull.
--
-- ObjC selector: @- selectedMediaPresentationSettingsForMediaSelectionGroup:@
selectedMediaPresentationSettingsForMediaSelectionGroup :: (IsAVPlayerItem avPlayerItem, IsAVMediaSelectionGroup mediaSelectionGroup) => avPlayerItem -> mediaSelectionGroup -> IO (Id NSDictionary)
selectedMediaPresentationSettingsForMediaSelectionGroup avPlayerItem  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avPlayerItem (mkSelector "selectedMediaPresentationSettingsForMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | Indicates the media presentation settings with media characteristics that are possessed by the currently selected AVMediaSelectionOption in the specified AVMediaSelectionGroup.
--
-- Effective media presentation settings can differ from the currently effective media presentation settings if no AVMediaSelectionOption of the specified AVMediaSelectionGroup with the currently selected media presentation language possesses all of the characteristics associated with the currently selected settings.  A value of NSNull for an AVMediaPresentationSelector can occur if either the content is inappropriately authored for the use of the AVCustomMediaSelectionScheme or if the currently selected AVMediaSelectionOption has been selected by means other than through the use of AVMediaPresentationSettings.
--
-- - Parameter mediaSelectionGroup: An AVMediaSelectionGroup obtained from the receiver's asset for which the currently effective media presentation settings are desired.
--
-- - Returns: A dictionary with AVMediaPresentationSelectors as keys and AVMediaPresentationSettings as values, unless the AVMediaSelectionOption currently selected in the group possesses none of the characteristics associated with the selector's settings. In that case the dictionary value will be NSNull.
--
-- ObjC selector: @- effectiveMediaPresentationSettingsForMediaSelectionGroup:@
effectiveMediaPresentationSettingsForMediaSelectionGroup :: (IsAVPlayerItem avPlayerItem, IsAVMediaSelectionGroup mediaSelectionGroup) => avPlayerItem -> mediaSelectionGroup -> IO (Id NSDictionary)
effectiveMediaPresentationSettingsForMediaSelectionGroup avPlayerItem  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avPlayerItem (mkSelector "effectiveMediaPresentationSettingsForMediaSelectionGroup:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())] >>= retainedObject . castPtr

-- | Selects the media option described by the specified instance of AVMediaSelectionOption in the specified AVMediaSelectionGroup and deselects all other options in that group.
--
-- If the specified media selection option isn't a member of the specified media selection group, no change in presentation state will result. If the value of the property allowsEmptySelection of the AVMediaSelectionGroup is YES, you can pass nil for mediaSelectionOption to deselect all media selection options in the group. Note that if multiple options within a group meet your criteria for selection according to locale or other considerations, and if these options are otherwise indistinguishable to you according to media characteristics that are meaningful for your application, content is typically authored so that the first available option that meets your criteria is appropriate for selection.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this method must be invoked on the main thread/queue.
--
-- - Parameter mediaSelectionOption: The option to select. - Parameter mediaSelectionGroup: The media selection group, obtained from the receiver's asset, that contains the specified option.
--
-- ObjC selector: @- selectMediaOption:inMediaSelectionGroup:@
selectMediaOption_inMediaSelectionGroup :: (IsAVPlayerItem avPlayerItem, IsAVMediaSelectionOption mediaSelectionOption, IsAVMediaSelectionGroup mediaSelectionGroup) => avPlayerItem -> mediaSelectionOption -> mediaSelectionGroup -> IO ()
selectMediaOption_inMediaSelectionGroup avPlayerItem  mediaSelectionOption mediaSelectionGroup =
withObjCPtr mediaSelectionOption $ \raw_mediaSelectionOption ->
  withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
      sendMsg avPlayerItem (mkSelector "selectMediaOption:inMediaSelectionGroup:") retVoid [argPtr (castPtr raw_mediaSelectionOption :: Ptr ()), argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())]

-- | Selects the media option in the specified media selection group that best matches the AVPlayer's current automatic selection criteria. Also allows automatic selection to be re-applied to the specified group subsequently if the relevant criteria are changed.
--
-- Has no effect unless the appliesMediaSelectionCriteriaAutomatically property of the associated AVPlayer is YES and unless automatic media selection has previously been overridden via -[AVPlayerItem selectMediaOption:inMediaSelectionGroup:].
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this method must be invoked on the main thread/queue.
--
-- - Parameter mediaSelectionGroup: The media selection group, obtained from the receiver's asset, that contains the specified option.
--
-- ObjC selector: @- selectMediaOptionAutomaticallyInMediaSelectionGroup:@
selectMediaOptionAutomaticallyInMediaSelectionGroup :: (IsAVPlayerItem avPlayerItem, IsAVMediaSelectionGroup mediaSelectionGroup) => avPlayerItem -> mediaSelectionGroup -> IO ()
selectMediaOptionAutomaticallyInMediaSelectionGroup avPlayerItem  mediaSelectionGroup =
withObjCPtr mediaSelectionGroup $ \raw_mediaSelectionGroup ->
    sendMsg avPlayerItem (mkSelector "selectMediaOptionAutomaticallyInMediaSelectionGroup:") retVoid [argPtr (castPtr raw_mediaSelectionGroup :: Ptr ())]

-- | Cancel any pending seek requests and invoke the corresponding completion handlers if present.
--
-- Use this method to cancel and release the completion handlers of pending seeks. The finished parameter of the completion handlers will be set to NO.
--
-- ObjC selector: @- cancelPendingSeeks@
cancelPendingSeeks :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO ()
cancelPendingSeeks avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "cancelPendingSeeks") retVoid []

-- | If currentTime is mapped to a particular (real-time) date, return that date.
--
-- - Returns: Returns the date of current playback, or nil if playback is not mapped to any date.
--
-- ObjC selector: @- currentDate@
currentDate :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSDate)
currentDate avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "currentDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | move playhead to a point corresponding to a particular date, and invokes the specified block when the seek operation has either been completed or been interrupted.
--
-- For playback content that is associated with a range of dates, move the playhead to point within that range and invokes the completion handler when the seek operation is complete.  Will fail if the supplied date is outside the range or if the content is not associated with a range of dates.  The completion handler for any prior seek request that is still in process will be invoked immediately with the finished parameter  set to NO. If the new request completes without being interrupted by another seek request or by any other operation, the specified  completion handler will be invoked with the finished parameter set to YES.
--
-- - Parameter date: The new position for the playhead. - Parameter completionHandler: The block to invoke when seek operation is complete
--
-- - Returns: Returns true if the playhead was moved to the supplied date.
--
-- ObjC selector: @- seekToDate:completionHandler:@
seekToDate_completionHandler :: (IsAVPlayerItem avPlayerItem, IsNSDate date) => avPlayerItem -> date -> Ptr () -> IO Bool
seekToDate_completionHandler avPlayerItem  date completionHandler =
withObjCPtr date $ \raw_date ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "seekToDate:completionHandler:") retCULong [argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Moves player's current item's current time forward or backward by the specified number of steps.
--
-- The size of each step depends on the enabled AVPlayerItemTracks of the AVPlayerItem.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this method must be invoked on the main thread/queue.
--
-- - Parameter stepCount: The number of steps by which to move. A positive number results in stepping forward, a negative number in stepping backward.
--
-- ObjC selector: @- stepByCount:@
stepByCount :: IsAVPlayerItem avPlayerItem => avPlayerItem -> CLong -> IO ()
stepByCount avPlayerItem  stepCount =
  sendMsg avPlayerItem (mkSelector "stepByCount:") retVoid [argCLong (fromIntegral stepCount)]

-- | The ability of the receiver to be used for playback.
--
-- The value of this property is an AVPlayerItemStatus that indicates whether the receiver can be used for playback. When the value of this property is AVPlayerItemStatusFailed, the receiver can no longer be used for playback and a new instance needs to be created in its place. When this happens, clients can check the value of the error property to determine the nature of the failure. The value of this property will not be updated after the receiver is removed from an AVPlayer. This property is key value observable.
--
-- ObjC selector: @- status@
status :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO AVPlayerItemStatus
status avPlayerItem  =
  fmap (coerce :: CLong -> AVPlayerItemStatus) $ sendMsg avPlayerItem (mkSelector "status") retCLong []

-- | If the receiver's status is AVPlayerItemStatusFailed, this describes the error that caused the failure.
--
-- The value of this property is an NSError that describes what caused the receiver to no longer be able to be played. If the receiver's status is not AVPlayerItemStatusFailed, the value of this property is nil.
--
-- ObjC selector: @- error@
error_ :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSError)
error_ avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | integratedTimeline
--
-- Obtain an instance of AVPlayerItemIntegratedTimeline representing the timing and control of playback of the item with its scheduled AVPlayerInterstitialEvents. This value will return nil for AVPlayerItems in an interstitial player.
--
-- ObjC selector: @- integratedTimeline@
integratedTimeline :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id AVPlayerItemIntegratedTimeline)
integratedTimeline avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "integratedTimeline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Allows interstitials to be played according to a schedule that's specified by server-side directives. The default value is YES. A value of NO prevents automatic scheduling of future server-side interstitial events. Events specified by an AVPlayerInterstitialEventController override server-side events, regardless of the value of this property.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- automaticallyHandlesInterstitialEvents@
automaticallyHandlesInterstitialEvents :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
automaticallyHandlesInterstitialEvents avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "automaticallyHandlesInterstitialEvents") retCULong []

-- | Allows interstitials to be played according to a schedule that's specified by server-side directives. The default value is YES. A value of NO prevents automatic scheduling of future server-side interstitial events. Events specified by an AVPlayerInterstitialEventController override server-side events, regardless of the value of this property.
--
-- Before macOS 13, iOS 16, tvOS 16, and watchOS 9, this property must be accessed on the main thread/queue.
--
-- ObjC selector: @- setAutomaticallyHandlesInterstitialEvents:@
setAutomaticallyHandlesInterstitialEvents :: IsAVPlayerItem avPlayerItem => avPlayerItem -> Bool -> IO ()
setAutomaticallyHandlesInterstitialEvents avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setAutomaticallyHandlesInterstitialEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | If the item was created automatically according to a template item for looping, for interstitial playback, or for other purposes, indicates the AVPlayerItem that was used as the template.
--
-- ObjC selector: @- templatePlayerItem@
templatePlayerItem :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id AVPlayerItem)
templatePlayerItem avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "templatePlayerItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | authorizationRequiredForPlayback
--
-- Indicates whether or not authorization is required to play the content.
--
-- This property reports whether or not authorization is required for the receiver's content to be played.  If it does not require authorization, then none of the other methods or properties in the AVPlayerItemProtectedContent category apply (though they will return sensible values where possible). This value is NOT key-value observable.
--
-- ObjC selector: @- authorizationRequiredForPlayback@
authorizationRequiredForPlayback :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
authorizationRequiredForPlayback avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "authorizationRequiredForPlayback") retCULong []

-- | applicationAuthorizedForPlayback
--
-- Indicates whether the calling application can be used to play the content.
--
-- This property reports whether or not the calling application is authorized to play the content associated with the receiver.  Note that application authorization is independent of content authorization (see contentAuthorizedForPlayback) and that both must be granted in order for an application to be allowed to play protected content. Also, unlike content authorization, application authorization is not dependent on user credentials (i.e. if applicationAuthorizedForPlayback is NO, there are no means to obtain authorization). This value is NOT key-value observable.
--
-- ObjC selector: @- applicationAuthorizedForPlayback@
applicationAuthorizedForPlayback :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
applicationAuthorizedForPlayback avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "applicationAuthorizedForPlayback") retCULong []

-- | contentAuthorizedForPlayback
--
-- Indicates whether the content has been authorized by the user (e.g. by authorizing the content's associated account in iTunes).
--
-- This property reports whether or not the user has provided the necessary credentials to the system in order for the content to be decrypted for playback. Note that content authorization is independent of application authorization (see applicationAuthorizedForPlayback) and that both must be  granted in order for an application to be allowed to play protected content. This value is NOT key-value observable.
--
-- ObjC selector: @- contentAuthorizedForPlayback@
contentAuthorizedForPlayback :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
contentAuthorizedForPlayback avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "contentAuthorizedForPlayback") retCULong []

-- | contentAuthorizationRequestStatus
--
-- Indicates the status of the most recent call to requestContentAuthorizationAsynchronouslyWithTimeoutInterval:CompletionHandler:
--
-- This property reports the authorization status as determined by the most recent call to requestContentAuthorizationAsynchronouslyWithTimeoutInterval:CompletionHandler:. The value will be AVContentAuthorizationUnknown before the first call and between the time a request call is made and just prior to the completion handler being executed (i.e. it is safe to query this property from the completion handler). This value is NOT key-value observable.
--
-- ObjC selector: @- contentAuthorizationRequestStatus@
contentAuthorizationRequestStatus :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO AVContentAuthorizationStatus
contentAuthorizationRequestStatus avPlayerItem  =
  fmap (coerce :: CLong -> AVContentAuthorizationStatus) $ sendMsg avPlayerItem (mkSelector "contentAuthorizationRequestStatus") retCLong []

-- | The collection of associated mediaDataCollectors.
--
-- ObjC selector: @- mediaDataCollectors@
mediaDataCollectors :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSArray)
mediaDataCollectors avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "mediaDataCollectors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The collection of associated outputs.
--
-- ObjC selector: @- outputs@
outputs :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSArray)
outputs avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "outputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the AVCustomMediaSelectionSchemes of AVMediaSelectionGroups of the receiver's asset with which an associated UI implementation should configure its interface for media selection.
--
-- Recommended usage: if use of a custom media selection scheme is desired, set this property before either replacing an AVPlayer's current item with the receiver or adding the receiver to an AVQueuePlayer's play queue. This will satisfy requirements of UI implementations that commit to a configuration of UI elements as the receiver becomes ready to play.
--
-- ObjC selector: @- preferredCustomMediaSelectionSchemes@
preferredCustomMediaSelectionSchemes :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSArray)
preferredCustomMediaSelectionSchemes avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "preferredCustomMediaSelectionSchemes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the AVCustomMediaSelectionSchemes of AVMediaSelectionGroups of the receiver's asset with which an associated UI implementation should configure its interface for media selection.
--
-- Recommended usage: if use of a custom media selection scheme is desired, set this property before either replacing an AVPlayer's current item with the receiver or adding the receiver to an AVQueuePlayer's play queue. This will satisfy requirements of UI implementations that commit to a configuration of UI elements as the receiver becomes ready to play.
--
-- ObjC selector: @- setPreferredCustomMediaSelectionSchemes:@
setPreferredCustomMediaSelectionSchemes :: (IsAVPlayerItem avPlayerItem, IsNSArray value) => avPlayerItem -> value -> IO ()
setPreferredCustomMediaSelectionSchemes avPlayerItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerItem (mkSelector "setPreferredCustomMediaSelectionSchemes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides an instance of AVMediaSelection carrying current selections for each of the receiver's media selection groups.
--
-- ObjC selector: @- currentMediaSelection@
currentMediaSelection :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id AVMediaSelection)
currentMediaSelection avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "currentMediaSelection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the desired limit of network bandwidth consumption for this item.
--
-- Set preferredPeakBitRate to non-zero to indicate that the player should attempt to limit item playback to that bit rate, expressed in bits per second.
--
-- If network bandwidth consumption cannot be lowered to meet the preferredPeakBitRate, it will be reduced as much as possible while continuing to play the item.
--
-- ObjC selector: @- preferredPeakBitRate@
preferredPeakBitRate :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO CDouble
preferredPeakBitRate avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "preferredPeakBitRate") retCDouble []

-- | Indicates the desired limit of network bandwidth consumption for this item.
--
-- Set preferredPeakBitRate to non-zero to indicate that the player should attempt to limit item playback to that bit rate, expressed in bits per second.
--
-- If network bandwidth consumption cannot be lowered to meet the preferredPeakBitRate, it will be reduced as much as possible while continuing to play the item.
--
-- ObjC selector: @- setPreferredPeakBitRate:@
setPreferredPeakBitRate :: IsAVPlayerItem avPlayerItem => avPlayerItem -> CDouble -> IO ()
setPreferredPeakBitRate avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setPreferredPeakBitRate:") retVoid [argCDouble (fromIntegral value)]

-- | Indicates the desired limit of network bandwidth consumption for this item over expensive networks.
--
-- When preferredPeakBitRateForExpensiveNetworks is set to non-zero, the player will attempt to limit item playback to that bit rate  when streaming over an expensive network, such as when using a cellular data plan. (See -[NWPath isExpensive])
--
-- If network bandwidth consumption cannot be lowered to meet the preferredPeakBitRateForExpensiveNetworks, it will be reduced as much as possible while continuing to play the item.
--
-- Note that preferredPeakBitRate still applies unconditionally. If preferredPeakBitRateForExpensiveNetworks is less restrictive (greater) than preferredPeakBitRate, preferredPeakBitRateForExpensiveNetworks has no practical effect.
--
-- ObjC selector: @- preferredPeakBitRateForExpensiveNetworks@
preferredPeakBitRateForExpensiveNetworks :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO CDouble
preferredPeakBitRateForExpensiveNetworks avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "preferredPeakBitRateForExpensiveNetworks") retCDouble []

-- | Indicates the desired limit of network bandwidth consumption for this item over expensive networks.
--
-- When preferredPeakBitRateForExpensiveNetworks is set to non-zero, the player will attempt to limit item playback to that bit rate  when streaming over an expensive network, such as when using a cellular data plan. (See -[NWPath isExpensive])
--
-- If network bandwidth consumption cannot be lowered to meet the preferredPeakBitRateForExpensiveNetworks, it will be reduced as much as possible while continuing to play the item.
--
-- Note that preferredPeakBitRate still applies unconditionally. If preferredPeakBitRateForExpensiveNetworks is less restrictive (greater) than preferredPeakBitRate, preferredPeakBitRateForExpensiveNetworks has no practical effect.
--
-- ObjC selector: @- setPreferredPeakBitRateForExpensiveNetworks:@
setPreferredPeakBitRateForExpensiveNetworks :: IsAVPlayerItem avPlayerItem => avPlayerItem -> CDouble -> IO ()
setPreferredPeakBitRateForExpensiveNetworks avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setPreferredPeakBitRateForExpensiveNetworks:") retVoid [argCDouble (fromIntegral value)]

-- | Directs the player to start playback with the first eligible variant that appears in the stream's master playlist.
--
-- This property influences AVPlayer's algorithm for selecting which of the eligible variant streams in an HTTP Live Streaming master playlist is selected when playback first begins. In all cases, AVPlayer may switch to other variants during playback.
--
-- On releases prior to macOS 10.15, iOS 13, tvOS 13 and watchOS 6, AVPlayer starts HLS playback with the first eligible variant in the master playlist. On releases starting with macOS 10.15, iOS 13, tvOS 13 and watchOS 6, AVPlayer starts HLS playback by choosing an initial variant that optimizes the startup experience. On releases starting with macOS 11.0, iOS 14, tvOS 14 and watchOS 7, applications may set this property to YES to request that AVPlayer use the previous behaviour of using the first eligible variant in the master playlist. This would be appropriate, for example, for applications which wish to control initial variant selection by ordering the variants in the master playlist.
--
-- Note that changing this property may impact stream startup performance and quality. In order to be effective this property must be set before initial variant selection occurs. This property only applies to HTTP Live Streaming assets. The default value of this property is NO.
--
-- ObjC selector: @- startsOnFirstEligibleVariant@
startsOnFirstEligibleVariant :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
startsOnFirstEligibleVariant avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "startsOnFirstEligibleVariant") retCULong []

-- | Directs the player to start playback with the first eligible variant that appears in the stream's master playlist.
--
-- This property influences AVPlayer's algorithm for selecting which of the eligible variant streams in an HTTP Live Streaming master playlist is selected when playback first begins. In all cases, AVPlayer may switch to other variants during playback.
--
-- On releases prior to macOS 10.15, iOS 13, tvOS 13 and watchOS 6, AVPlayer starts HLS playback with the first eligible variant in the master playlist. On releases starting with macOS 10.15, iOS 13, tvOS 13 and watchOS 6, AVPlayer starts HLS playback by choosing an initial variant that optimizes the startup experience. On releases starting with macOS 11.0, iOS 14, tvOS 14 and watchOS 7, applications may set this property to YES to request that AVPlayer use the previous behaviour of using the first eligible variant in the master playlist. This would be appropriate, for example, for applications which wish to control initial variant selection by ordering the variants in the master playlist.
--
-- Note that changing this property may impact stream startup performance and quality. In order to be effective this property must be set before initial variant selection occurs. This property only applies to HTTP Live Streaming assets. The default value of this property is NO.
--
-- ObjC selector: @- setStartsOnFirstEligibleVariant:@
setStartsOnFirstEligibleVariant :: IsAVPlayerItem avPlayerItem => avPlayerItem -> Bool -> IO ()
setStartsOnFirstEligibleVariant avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setStartsOnFirstEligibleVariant:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates preferences for variant switching.
--
-- Changing variant preferences during playback may result in a variant switch. The default value is AVVariantPreferenceNone.
--
-- ObjC selector: @- variantPreferences@
variantPreferences :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO AVVariantPreferences
variantPreferences avPlayerItem  =
  fmap (coerce :: CULong -> AVVariantPreferences) $ sendMsg avPlayerItem (mkSelector "variantPreferences") retCULong []

-- | Indicates preferences for variant switching.
--
-- Changing variant preferences during playback may result in a variant switch. The default value is AVVariantPreferenceNone.
--
-- ObjC selector: @- setVariantPreferences:@
setVariantPreferences :: IsAVPlayerItem avPlayerItem => avPlayerItem -> AVVariantPreferences -> IO ()
setVariantPreferences avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setVariantPreferences:") retVoid [argCULong (coerce value)]

-- | This property provides a collection of time ranges for which the player has the media data readily available. The ranges provided might be discontinuous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges.
--
-- ObjC selector: @- loadedTimeRanges@
loadedTimeRanges :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSArray)
loadedTimeRanges avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "loadedTimeRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether the item will likely play through without stalling.
--
-- This property communicates a prediction of playability. Factors considered in this prediction include I/O throughput and media decode performance. It is possible for playbackLikelyToKeepUp to indicate NO while the property playbackBufferFull indicates YES. In this event the playback buffer has reached capacity but there isn't the statistical data to support a prediction that playback is likely to  keep up. It is left to the application programmer to decide to continue media playback or not.  See playbackBufferFull below.
--
-- ObjC selector: @- playbackLikelyToKeepUp@
playbackLikelyToKeepUp :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
playbackLikelyToKeepUp avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "playbackLikelyToKeepUp") retCULong []

-- | Indicates that the internal media buffer is full and that further I/O is suspended.
--
-- This property reports that the data buffer used for playback has reach capacity. Despite the playback buffer reaching capacity there might not exist sufficient statistical  data to support a playbackLikelyToKeepUp prediction of YES. See playbackLikelyToKeepUp above.
--
-- ObjC selector: @- playbackBufferFull@
playbackBufferFull :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
playbackBufferFull avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "playbackBufferFull") retCULong []

-- | @- playbackBufferEmpty@
playbackBufferEmpty :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
playbackBufferEmpty avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "playbackBufferEmpty") retCULong []

-- | Indicates whether the player item can use network resources to keep playback state up to date while paused
--
-- For live streaming content, the player item may need to use extra networking and power resources to keep playback state up to date when paused. For example, when this property is set to YES, the seekableTimeRanges property will be periodically updated to reflect the current state of the live stream.
--
-- For clients linked on or after macOS 10.11 or iOS 9.0, the default value is NO. To minimize power usage, avoid setting this property to YES when you do not need playback state to stay up to date while paused.
--
-- ObjC selector: @- canUseNetworkResourcesForLiveStreamingWhilePaused@
canUseNetworkResourcesForLiveStreamingWhilePaused :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
canUseNetworkResourcesForLiveStreamingWhilePaused avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "canUseNetworkResourcesForLiveStreamingWhilePaused") retCULong []

-- | Indicates whether the player item can use network resources to keep playback state up to date while paused
--
-- For live streaming content, the player item may need to use extra networking and power resources to keep playback state up to date when paused. For example, when this property is set to YES, the seekableTimeRanges property will be periodically updated to reflect the current state of the live stream.
--
-- For clients linked on or after macOS 10.11 or iOS 9.0, the default value is NO. To minimize power usage, avoid setting this property to YES when you do not need playback state to stay up to date while paused.
--
-- ObjC selector: @- setCanUseNetworkResourcesForLiveStreamingWhilePaused:@
setCanUseNetworkResourcesForLiveStreamingWhilePaused :: IsAVPlayerItem avPlayerItem => avPlayerItem -> Bool -> IO ()
setCanUseNetworkResourcesForLiveStreamingWhilePaused avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setCanUseNetworkResourcesForLiveStreamingWhilePaused:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates the media duration the caller prefers the player to buffer from the network ahead of the playhead to guard against playback disruption.
--
-- The value is in seconds. If it is set to 0, the player will choose an appropriate level of buffering for most use cases. Note that setting this property to a low value will increase the chance that playback will stall and re-buffer, while setting it to a high value will increase demand on system resources. Note that the system may buffer less than the value of this property in order to manage resource consumption.
--
-- ObjC selector: @- preferredForwardBufferDuration@
preferredForwardBufferDuration :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO CDouble
preferredForwardBufferDuration avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "preferredForwardBufferDuration") retCDouble []

-- | Indicates the media duration the caller prefers the player to buffer from the network ahead of the playhead to guard against playback disruption.
--
-- The value is in seconds. If it is set to 0, the player will choose an appropriate level of buffering for most use cases. Note that setting this property to a low value will increase the chance that playback will stall and re-buffer, while setting it to a high value will increase demand on system resources. Note that the system may buffer less than the value of this property in order to manage resource consumption.
--
-- ObjC selector: @- setPreferredForwardBufferDuration:@
setPreferredForwardBufferDuration :: IsAVPlayerItem avPlayerItem => avPlayerItem -> CDouble -> IO ()
setPreferredForwardBufferDuration avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setPreferredForwardBufferDuration:") retVoid [argCDouble (fromIntegral value)]

-- | Indicates the processing algorithm used to manage audio pitch at varying rates and for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h. The default value for applications linked on or after iOS 15.0 or macOS 12.0 is AVAudioTimePitchAlgorithmTimeDomain. For iOS versions prior to 15.0 the default value is AVAudioTimePitchAlgorithmLowQualityZeroLatency. For macOS versions prior to 12.0 the default value is AVAudioTimePitchAlgorithmSpectral.
--
-- ObjC selector: @- audioTimePitchAlgorithm@
audioTimePitchAlgorithm :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSString)
audioTimePitchAlgorithm avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "audioTimePitchAlgorithm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the processing algorithm used to manage audio pitch at varying rates and for scaled audio edits.
--
-- Constants for various time pitch algorithms, e.g. AVAudioTimePitchSpectral, are defined in AVAudioProcessingSettings.h. The default value for applications linked on or after iOS 15.0 or macOS 12.0 is AVAudioTimePitchAlgorithmTimeDomain. For iOS versions prior to 15.0 the default value is AVAudioTimePitchAlgorithmLowQualityZeroLatency. For macOS versions prior to 12.0 the default value is AVAudioTimePitchAlgorithmSpectral.
--
-- ObjC selector: @- setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithm :: (IsAVPlayerItem avPlayerItem, IsNSString value) => avPlayerItem -> value -> IO ()
setAudioTimePitchAlgorithm avPlayerItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerItem (mkSelector "setAudioTimePitchAlgorithm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates whether audio spatialization is allowed
--
-- When audio spatialization is allowed for an AVPlayerItem, the AVPlayer may render multichannel audio if available even if the output device doesn't support multichannel audio on its own, via use of a synthetic channel layout. When audio spatialization is not allowed, the AVPlayer must render audio with a channel layout that best matches the capabilities of the output device. This property is not observable. Defaults to YES.
--
-- ObjC selector: @- audioSpatializationAllowed@
audioSpatializationAllowed :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
audioSpatializationAllowed avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "audioSpatializationAllowed") retCULong []

-- | Indicates whether audio spatialization is allowed
--
-- When audio spatialization is allowed for an AVPlayerItem, the AVPlayer may render multichannel audio if available even if the output device doesn't support multichannel audio on its own, via use of a synthetic channel layout. When audio spatialization is not allowed, the AVPlayer must render audio with a channel layout that best matches the capabilities of the output device. This property is not observable. Defaults to YES.
--
-- ObjC selector: @- setAudioSpatializationAllowed:@
setAudioSpatializationAllowed :: IsAVPlayerItem avPlayerItem => avPlayerItem -> Bool -> IO ()
setAudioSpatializationAllowed avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setAudioSpatializationAllowed:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates the source audio channel layouts allowed by the receiver for spatialization.
--
-- Spatialization uses psychoacoustic methods to create a more immersive audio rendering when the content is played on specialized headphones and speaker arrangements. When an AVPlayerItem's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMonoAndStereo the AVPlayer will attempt to spatialize content tagged with a stereo channel layout, two-channel content with no layout specified as well as mono. It is considered incorrect to render a binaural recording with spatialization. A binaural recording is captured using two carefully placed microphones at each ear where the intent, when played on headphones, is to reproduce a naturally occurring spatial effect. Content tagged with a binaural channel layout will ignore this property value. When an AVPlayerItem's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMultichannel the AVPlayer will attempt to spatialize any decodable multichannel layout. Setting this property to AVAudioSpatializationFormatMonoStereoAndMultichannel indicates that the sender allows the AVPlayer to spatialize any decodable mono, stereo or multichannel layout. This property is not observable. The default value for this property with video content is AVAudioSpatializationFormatMonoStereoAndMultichannel. Otherwise, audio only content default value is AVAudioSpatializationFormatMultichannel.
--
-- ObjC selector: @- allowedAudioSpatializationFormats@
allowedAudioSpatializationFormats :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO AVAudioSpatializationFormats
allowedAudioSpatializationFormats avPlayerItem  =
  fmap (coerce :: CULong -> AVAudioSpatializationFormats) $ sendMsg avPlayerItem (mkSelector "allowedAudioSpatializationFormats") retCULong []

-- | Indicates the source audio channel layouts allowed by the receiver for spatialization.
--
-- Spatialization uses psychoacoustic methods to create a more immersive audio rendering when the content is played on specialized headphones and speaker arrangements. When an AVPlayerItem's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMonoAndStereo the AVPlayer will attempt to spatialize content tagged with a stereo channel layout, two-channel content with no layout specified as well as mono. It is considered incorrect to render a binaural recording with spatialization. A binaural recording is captured using two carefully placed microphones at each ear where the intent, when played on headphones, is to reproduce a naturally occurring spatial effect. Content tagged with a binaural channel layout will ignore this property value. When an AVPlayerItem's allowedAudioSpatializationFormats property is set to AVAudioSpatializationFormatMultichannel the AVPlayer will attempt to spatialize any decodable multichannel layout. Setting this property to AVAudioSpatializationFormatMonoStereoAndMultichannel indicates that the sender allows the AVPlayer to spatialize any decodable mono, stereo or multichannel layout. This property is not observable. The default value for this property with video content is AVAudioSpatializationFormatMonoStereoAndMultichannel. Otherwise, audio only content default value is AVAudioSpatializationFormatMultichannel.
--
-- ObjC selector: @- setAllowedAudioSpatializationFormats:@
setAllowedAudioSpatializationFormats :: IsAVPlayerItem avPlayerItem => avPlayerItem -> AVAudioSpatializationFormats -> IO ()
setAllowedAudioSpatializationFormats avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setAllowedAudioSpatializationFormats:") retVoid [argCULong (coerce value)]

-- | Indicates the audio mix parameters to be applied during playback
--
-- The inputParameters of the AVAudioMix must have trackIDs that correspond to a track of the receiver's asset. Otherwise they will be ignored. (See AVAudioMix.h for the declaration of AVAudioMixInputParameters and AVPlayerItem's asset property.)
--
-- ObjC selector: @- audioMix@
audioMix :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id AVAudioMix)
audioMix avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "audioMix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates the audio mix parameters to be applied during playback
--
-- The inputParameters of the AVAudioMix must have trackIDs that correspond to a track of the receiver's asset. Otherwise they will be ignored. (See AVAudioMix.h for the declaration of AVAudioMixInputParameters and AVPlayerItem's asset property.)
--
-- ObjC selector: @- setAudioMix:@
setAudioMix :: (IsAVPlayerItem avPlayerItem, IsAVAudioMix value) => avPlayerItem -> value -> IO ()
setAudioMix avPlayerItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerItem (mkSelector "setAudioMix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates whether the item's timing follows the displayed video frame when seeking with a video composition
--
-- By default, item timing is updated as quickly as possible, not waiting for media at new times to be rendered when seeking or  during normal playback. The latency that occurs, for example, between the completion of a seek operation and the display of a  video frame at a new time is negligible in most situations. However, when video compositions are in use, the processing of  video for any particular time may introduce noticeable latency. Therefore it may be desirable when a video composition is in  use for the item's timing be updated only after the video frame for a time has been displayed. This allows, for instance, an  AVSynchronizedLayer associated with an AVPlayerItem to remain in synchronization with the displayed video and for the  currentTime property to return the time of the displayed video.
--
-- This property has no effect on items for which videoComposition is nil.
--
-- ObjC selector: @- seekingWaitsForVideoCompositionRendering@
seekingWaitsForVideoCompositionRendering :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
seekingWaitsForVideoCompositionRendering avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "seekingWaitsForVideoCompositionRendering") retCULong []

-- | Indicates whether the item's timing follows the displayed video frame when seeking with a video composition
--
-- By default, item timing is updated as quickly as possible, not waiting for media at new times to be rendered when seeking or  during normal playback. The latency that occurs, for example, between the completion of a seek operation and the display of a  video frame at a new time is negligible in most situations. However, when video compositions are in use, the processing of  video for any particular time may introduce noticeable latency. Therefore it may be desirable when a video composition is in  use for the item's timing be updated only after the video frame for a time has been displayed. This allows, for instance, an  AVSynchronizedLayer associated with an AVPlayerItem to remain in synchronization with the displayed video and for the  currentTime property to return the time of the displayed video.
--
-- This property has no effect on items for which videoComposition is nil.
--
-- ObjC selector: @- setSeekingWaitsForVideoCompositionRendering:@
setSeekingWaitsForVideoCompositionRendering :: IsAVPlayerItem avPlayerItem => avPlayerItem -> Bool -> IO ()
setSeekingWaitsForVideoCompositionRendering avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setSeekingWaitsForVideoCompositionRendering:") retVoid [argCULong (if value then 1 else 0)]

-- | Specifies the video aperture mode to apply during playback.
--
-- See AVVideoApertureMode constants defined in AVVideoSettings.h. Default is AVVideoApertureModeCleanAperture.
--
-- ObjC selector: @- videoApertureMode@
videoApertureMode :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSString)
videoApertureMode avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "videoApertureMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies the video aperture mode to apply during playback.
--
-- See AVVideoApertureMode constants defined in AVVideoSettings.h. Default is AVVideoApertureModeCleanAperture.
--
-- ObjC selector: @- setVideoApertureMode:@
setVideoApertureMode :: (IsAVPlayerItem avPlayerItem, IsNSString value) => avPlayerItem -> value -> IO ()
setVideoApertureMode avPlayerItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerItem (mkSelector "setVideoApertureMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether or not to apply the per frame HDR display metadata of the source during playback.
--
-- ObjC selector: @- appliesPerFrameHDRDisplayMetadata@
appliesPerFrameHDRDisplayMetadata :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
appliesPerFrameHDRDisplayMetadata avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "appliesPerFrameHDRDisplayMetadata") retCULong []

-- | Controls whether or not to apply the per frame HDR display metadata of the source during playback.
--
-- ObjC selector: @- setAppliesPerFrameHDRDisplayMetadata:@
setAppliesPerFrameHDRDisplayMetadata :: IsAVPlayerItem avPlayerItem => avPlayerItem -> Bool -> IO ()
setAppliesPerFrameHDRDisplayMetadata avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setAppliesPerFrameHDRDisplayMetadata:") retVoid [argCULong (if value then 1 else 0)]

-- | This property provides a collection of time ranges that the player item can seek to. The ranges provided might be discontinous.
--
-- Returns an NSArray of NSValues containing CMTimeRanges.
--
-- ObjC selector: @- seekableTimeRanges@
seekableTimeRanges :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSArray)
seekableTimeRanges avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "seekableTimeRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The item's timebase.
--
-- You can examine the timebase to discover the relationship between the item's time and the source clock used for drift synchronization. This timebase is read-only; you cannot set its time or rate to affect playback.
--
-- ObjC selector: @- timebase@
timebase :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Ptr ())
timebase avPlayerItem  =
  fmap castPtr $ sendMsg avPlayerItem (mkSelector "timebase") (retPtr retVoid) []

-- | For releases of macOS prior to 10.9 and releases of iOS prior to 7.0, indicates whether the item can be played at rates greater than 1.0. Starting with macOS 10.9 and iOS 7.0, all AVPlayerItems with status AVPlayerItemReadyToPlay can be played at rates between 1.0 and 2.0, inclusive, even if canPlayFastForward is NO; for those releases canPlayFastForward indicates whether the item can be played at rates greater than 2.0.
--
-- ObjC selector: @- canPlayFastForward@
canPlayFastForward :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
canPlayFastForward avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "canPlayFastForward") retCULong []

-- | Indicates whether the item can be played at rates between 0.0 and 1.0
--
-- ObjC selector: @- canPlaySlowForward@
canPlaySlowForward :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
canPlaySlowForward avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "canPlaySlowForward") retCULong []

-- | Indicates whether the item can be played at rate -1.0
--
-- ObjC selector: @- canPlayReverse@
canPlayReverse :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
canPlayReverse avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "canPlayReverse") retCULong []

-- | Indicates whether the item can be played at rates less between 0.0 and -1.0
--
-- ObjC selector: @- canPlaySlowReverse@
canPlaySlowReverse :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
canPlaySlowReverse avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "canPlaySlowReverse") retCULong []

-- | Indicates whether the item can be played at rates less than -1.0
--
-- ObjC selector: @- canPlayFastReverse@
canPlayFastReverse :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
canPlayFastReverse avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "canPlayFastReverse") retCULong []

-- | Indicates whether the item supports stepping forward; see -stepByCount:. Once the item has become ready to play, the value of canStepForward does not change even when boundary conditions are reached, such as when the item's currentTime is its end time.
--
-- ObjC selector: @- canStepForward@
canStepForward :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
canStepForward avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "canStepForward") retCULong []

-- | Indicates whether the item supports stepping backward; see -stepByCount:. Once the item has become ready to play, the value of canStepBackward does not change even when boundary conditions are reached, such as when the item's currentTime is equal to kCMTimeZero.
--
-- ObjC selector: @- canStepBackward@
canStepBackward :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
canStepBackward avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "canStepBackward") retCULong []

-- | Indicates that after the player spends a period of time buffering media, it will skip forward if necessary to restore the playhead's distance from the live edge of the presentation to what it was when buffering began.
--
-- If the value of this property is YES and the player must buffer media from the network in order to resume playback, the player will seek forward if necessary before resuming playback to restore the position that the playhead had when rebuffering began, relative to the end of the current AVPlayerItem's seekableTimeRange.
--
-- This behavior applies to media buffering that occurs as a consequence of starting playback, seeking, and recovering from a playback stall.
--
-- Note that if the network cannot deliver media quickly enough to maintain the playback rate, playback may stall interminably.
--
-- This property value has no effect if the asset is not a live stream. The default value of this property is NO.
--
-- ObjC selector: @- automaticallyPreservesTimeOffsetFromLive@
automaticallyPreservesTimeOffsetFromLive :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO Bool
automaticallyPreservesTimeOffsetFromLive avPlayerItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avPlayerItem (mkSelector "automaticallyPreservesTimeOffsetFromLive") retCULong []

-- | Indicates that after the player spends a period of time buffering media, it will skip forward if necessary to restore the playhead's distance from the live edge of the presentation to what it was when buffering began.
--
-- If the value of this property is YES and the player must buffer media from the network in order to resume playback, the player will seek forward if necessary before resuming playback to restore the position that the playhead had when rebuffering began, relative to the end of the current AVPlayerItem's seekableTimeRange.
--
-- This behavior applies to media buffering that occurs as a consequence of starting playback, seeking, and recovering from a playback stall.
--
-- Note that if the network cannot deliver media quickly enough to maintain the playback rate, playback may stall interminably.
--
-- This property value has no effect if the asset is not a live stream. The default value of this property is NO.
--
-- ObjC selector: @- setAutomaticallyPreservesTimeOffsetFromLive:@
setAutomaticallyPreservesTimeOffsetFromLive :: IsAVPlayerItem avPlayerItem => avPlayerItem -> Bool -> IO ()
setAutomaticallyPreservesTimeOffsetFromLive avPlayerItem  value =
  sendMsg avPlayerItem (mkSelector "setAutomaticallyPreservesTimeOffsetFromLive:") retVoid [argCULong (if value then 1 else 0)]

-- | Accessor for underlying AVAsset.
--
-- ObjC selector: @- asset@
asset :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id AVAsset)
asset avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "asset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides array of AVPlayerItem tracks. Observable (can change dynamically during playback).
--
-- The value of this property will accord with the properties of the underlying media resource when the receiver becomes ready to play. Before the underlying media resource has been sufficiently loaded, its value is an empty NSArray. Use key-value observation to obtain a valid array of tracks as soon as it becomes available.
--
-- ObjC selector: @- tracks@
tracks :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSArray)
tracks avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "tracks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of property keys defined on AVAsset. The value of each key in the array is automatically loaded while the receiver is being made ready to play.
--
-- The value of each key in automaticallyLoadedAssetKeys will be automatically be loaded by the underlying AVAsset before the receiver achieves the status AVPlayerItemStatusReadyToPlay; i.e. when the item is ready to play, the value of -[[AVPlayerItem asset] statusOfValueForKey:error:] will be AVKeyValueStatusLoaded. If loading of any of the values fails, the status of the AVPlayerItem will change instead to AVPlayerItemStatusFailed..
--
-- ObjC selector: @- automaticallyLoadedAssetKeys@
automaticallyLoadedAssetKeys :: IsAVPlayerItem avPlayerItem => avPlayerItem -> IO (Id NSArray)
automaticallyLoadedAssetKeys avPlayerItem  =
  sendMsg avPlayerItem (mkSelector "automaticallyLoadedAssetKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @playerItemWithURL:@
playerItemWithURLSelector :: Selector
playerItemWithURLSelector = mkSelector "playerItemWithURL:"

-- | @Selector@ for @playerItemWithAsset:@
playerItemWithAssetSelector :: Selector
playerItemWithAssetSelector = mkSelector "playerItemWithAsset:"

-- | @Selector@ for @playerItemWithAsset:automaticallyLoadedAssetKeys:@
playerItemWithAsset_automaticallyLoadedAssetKeysSelector :: Selector
playerItemWithAsset_automaticallyLoadedAssetKeysSelector = mkSelector "playerItemWithAsset:automaticallyLoadedAssetKeys:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithAsset:@
initWithAssetSelector :: Selector
initWithAssetSelector = mkSelector "initWithAsset:"

-- | @Selector@ for @initWithAsset:automaticallyLoadedAssetKeys:@
initWithAsset_automaticallyLoadedAssetKeysSelector :: Selector
initWithAsset_automaticallyLoadedAssetKeysSelector = mkSelector "initWithAsset:automaticallyLoadedAssetKeys:"

-- | @Selector@ for @copyWithZone:@
copyWithZoneSelector :: Selector
copyWithZoneSelector = mkSelector "copyWithZone:"

-- | @Selector@ for @copy@
copySelector :: Selector
copySelector = mkSelector "copy"

-- | @Selector@ for @requestContentAuthorizationAsynchronouslyWithTimeoutInterval:completionHandler:@
requestContentAuthorizationAsynchronouslyWithTimeoutInterval_completionHandlerSelector :: Selector
requestContentAuthorizationAsynchronouslyWithTimeoutInterval_completionHandlerSelector = mkSelector "requestContentAuthorizationAsynchronouslyWithTimeoutInterval:completionHandler:"

-- | @Selector@ for @cancelContentAuthorizationRequest@
cancelContentAuthorizationRequestSelector :: Selector
cancelContentAuthorizationRequestSelector = mkSelector "cancelContentAuthorizationRequest"

-- | @Selector@ for @seekToDate:@
seekToDateSelector :: Selector
seekToDateSelector = mkSelector "seekToDate:"

-- | @Selector@ for @selectedMediaOptionInMediaSelectionGroup:@
selectedMediaOptionInMediaSelectionGroupSelector :: Selector
selectedMediaOptionInMediaSelectionGroupSelector = mkSelector "selectedMediaOptionInMediaSelectionGroup:"

-- | @Selector@ for @addMediaDataCollector:@
addMediaDataCollectorSelector :: Selector
addMediaDataCollectorSelector = mkSelector "addMediaDataCollector:"

-- | @Selector@ for @removeMediaDataCollector:@
removeMediaDataCollectorSelector :: Selector
removeMediaDataCollectorSelector = mkSelector "removeMediaDataCollector:"

-- | @Selector@ for @addOutput:@
addOutputSelector :: Selector
addOutputSelector = mkSelector "addOutput:"

-- | @Selector@ for @removeOutput:@
removeOutputSelector :: Selector
removeOutputSelector = mkSelector "removeOutput:"

-- | @Selector@ for @accessLog@
accessLogSelector :: Selector
accessLogSelector = mkSelector "accessLog"

-- | @Selector@ for @errorLog@
errorLogSelector :: Selector
errorLogSelector = mkSelector "errorLog"

-- | @Selector@ for @selectMediaPresentationLanguage:forMediaSelectionGroup:@
selectMediaPresentationLanguage_forMediaSelectionGroupSelector :: Selector
selectMediaPresentationLanguage_forMediaSelectionGroupSelector = mkSelector "selectMediaPresentationLanguage:forMediaSelectionGroup:"

-- | @Selector@ for @selectedMediaPresentationLanguageForMediaSelectionGroup:@
selectedMediaPresentationLanguageForMediaSelectionGroupSelector :: Selector
selectedMediaPresentationLanguageForMediaSelectionGroupSelector = mkSelector "selectedMediaPresentationLanguageForMediaSelectionGroup:"

-- | @Selector@ for @selectMediaPresentationSetting:forMediaSelectionGroup:@
selectMediaPresentationSetting_forMediaSelectionGroupSelector :: Selector
selectMediaPresentationSetting_forMediaSelectionGroupSelector = mkSelector "selectMediaPresentationSetting:forMediaSelectionGroup:"

-- | @Selector@ for @selectedMediaPresentationSettingsForMediaSelectionGroup:@
selectedMediaPresentationSettingsForMediaSelectionGroupSelector :: Selector
selectedMediaPresentationSettingsForMediaSelectionGroupSelector = mkSelector "selectedMediaPresentationSettingsForMediaSelectionGroup:"

-- | @Selector@ for @effectiveMediaPresentationSettingsForMediaSelectionGroup:@
effectiveMediaPresentationSettingsForMediaSelectionGroupSelector :: Selector
effectiveMediaPresentationSettingsForMediaSelectionGroupSelector = mkSelector "effectiveMediaPresentationSettingsForMediaSelectionGroup:"

-- | @Selector@ for @selectMediaOption:inMediaSelectionGroup:@
selectMediaOption_inMediaSelectionGroupSelector :: Selector
selectMediaOption_inMediaSelectionGroupSelector = mkSelector "selectMediaOption:inMediaSelectionGroup:"

-- | @Selector@ for @selectMediaOptionAutomaticallyInMediaSelectionGroup:@
selectMediaOptionAutomaticallyInMediaSelectionGroupSelector :: Selector
selectMediaOptionAutomaticallyInMediaSelectionGroupSelector = mkSelector "selectMediaOptionAutomaticallyInMediaSelectionGroup:"

-- | @Selector@ for @cancelPendingSeeks@
cancelPendingSeeksSelector :: Selector
cancelPendingSeeksSelector = mkSelector "cancelPendingSeeks"

-- | @Selector@ for @currentDate@
currentDateSelector :: Selector
currentDateSelector = mkSelector "currentDate"

-- | @Selector@ for @seekToDate:completionHandler:@
seekToDate_completionHandlerSelector :: Selector
seekToDate_completionHandlerSelector = mkSelector "seekToDate:completionHandler:"

-- | @Selector@ for @stepByCount:@
stepByCountSelector :: Selector
stepByCountSelector = mkSelector "stepByCount:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @integratedTimeline@
integratedTimelineSelector :: Selector
integratedTimelineSelector = mkSelector "integratedTimeline"

-- | @Selector@ for @automaticallyHandlesInterstitialEvents@
automaticallyHandlesInterstitialEventsSelector :: Selector
automaticallyHandlesInterstitialEventsSelector = mkSelector "automaticallyHandlesInterstitialEvents"

-- | @Selector@ for @setAutomaticallyHandlesInterstitialEvents:@
setAutomaticallyHandlesInterstitialEventsSelector :: Selector
setAutomaticallyHandlesInterstitialEventsSelector = mkSelector "setAutomaticallyHandlesInterstitialEvents:"

-- | @Selector@ for @templatePlayerItem@
templatePlayerItemSelector :: Selector
templatePlayerItemSelector = mkSelector "templatePlayerItem"

-- | @Selector@ for @authorizationRequiredForPlayback@
authorizationRequiredForPlaybackSelector :: Selector
authorizationRequiredForPlaybackSelector = mkSelector "authorizationRequiredForPlayback"

-- | @Selector@ for @applicationAuthorizedForPlayback@
applicationAuthorizedForPlaybackSelector :: Selector
applicationAuthorizedForPlaybackSelector = mkSelector "applicationAuthorizedForPlayback"

-- | @Selector@ for @contentAuthorizedForPlayback@
contentAuthorizedForPlaybackSelector :: Selector
contentAuthorizedForPlaybackSelector = mkSelector "contentAuthorizedForPlayback"

-- | @Selector@ for @contentAuthorizationRequestStatus@
contentAuthorizationRequestStatusSelector :: Selector
contentAuthorizationRequestStatusSelector = mkSelector "contentAuthorizationRequestStatus"

-- | @Selector@ for @mediaDataCollectors@
mediaDataCollectorsSelector :: Selector
mediaDataCollectorsSelector = mkSelector "mediaDataCollectors"

-- | @Selector@ for @outputs@
outputsSelector :: Selector
outputsSelector = mkSelector "outputs"

-- | @Selector@ for @preferredCustomMediaSelectionSchemes@
preferredCustomMediaSelectionSchemesSelector :: Selector
preferredCustomMediaSelectionSchemesSelector = mkSelector "preferredCustomMediaSelectionSchemes"

-- | @Selector@ for @setPreferredCustomMediaSelectionSchemes:@
setPreferredCustomMediaSelectionSchemesSelector :: Selector
setPreferredCustomMediaSelectionSchemesSelector = mkSelector "setPreferredCustomMediaSelectionSchemes:"

-- | @Selector@ for @currentMediaSelection@
currentMediaSelectionSelector :: Selector
currentMediaSelectionSelector = mkSelector "currentMediaSelection"

-- | @Selector@ for @preferredPeakBitRate@
preferredPeakBitRateSelector :: Selector
preferredPeakBitRateSelector = mkSelector "preferredPeakBitRate"

-- | @Selector@ for @setPreferredPeakBitRate:@
setPreferredPeakBitRateSelector :: Selector
setPreferredPeakBitRateSelector = mkSelector "setPreferredPeakBitRate:"

-- | @Selector@ for @preferredPeakBitRateForExpensiveNetworks@
preferredPeakBitRateForExpensiveNetworksSelector :: Selector
preferredPeakBitRateForExpensiveNetworksSelector = mkSelector "preferredPeakBitRateForExpensiveNetworks"

-- | @Selector@ for @setPreferredPeakBitRateForExpensiveNetworks:@
setPreferredPeakBitRateForExpensiveNetworksSelector :: Selector
setPreferredPeakBitRateForExpensiveNetworksSelector = mkSelector "setPreferredPeakBitRateForExpensiveNetworks:"

-- | @Selector@ for @startsOnFirstEligibleVariant@
startsOnFirstEligibleVariantSelector :: Selector
startsOnFirstEligibleVariantSelector = mkSelector "startsOnFirstEligibleVariant"

-- | @Selector@ for @setStartsOnFirstEligibleVariant:@
setStartsOnFirstEligibleVariantSelector :: Selector
setStartsOnFirstEligibleVariantSelector = mkSelector "setStartsOnFirstEligibleVariant:"

-- | @Selector@ for @variantPreferences@
variantPreferencesSelector :: Selector
variantPreferencesSelector = mkSelector "variantPreferences"

-- | @Selector@ for @setVariantPreferences:@
setVariantPreferencesSelector :: Selector
setVariantPreferencesSelector = mkSelector "setVariantPreferences:"

-- | @Selector@ for @loadedTimeRanges@
loadedTimeRangesSelector :: Selector
loadedTimeRangesSelector = mkSelector "loadedTimeRanges"

-- | @Selector@ for @playbackLikelyToKeepUp@
playbackLikelyToKeepUpSelector :: Selector
playbackLikelyToKeepUpSelector = mkSelector "playbackLikelyToKeepUp"

-- | @Selector@ for @playbackBufferFull@
playbackBufferFullSelector :: Selector
playbackBufferFullSelector = mkSelector "playbackBufferFull"

-- | @Selector@ for @playbackBufferEmpty@
playbackBufferEmptySelector :: Selector
playbackBufferEmptySelector = mkSelector "playbackBufferEmpty"

-- | @Selector@ for @canUseNetworkResourcesForLiveStreamingWhilePaused@
canUseNetworkResourcesForLiveStreamingWhilePausedSelector :: Selector
canUseNetworkResourcesForLiveStreamingWhilePausedSelector = mkSelector "canUseNetworkResourcesForLiveStreamingWhilePaused"

-- | @Selector@ for @setCanUseNetworkResourcesForLiveStreamingWhilePaused:@
setCanUseNetworkResourcesForLiveStreamingWhilePausedSelector :: Selector
setCanUseNetworkResourcesForLiveStreamingWhilePausedSelector = mkSelector "setCanUseNetworkResourcesForLiveStreamingWhilePaused:"

-- | @Selector@ for @preferredForwardBufferDuration@
preferredForwardBufferDurationSelector :: Selector
preferredForwardBufferDurationSelector = mkSelector "preferredForwardBufferDuration"

-- | @Selector@ for @setPreferredForwardBufferDuration:@
setPreferredForwardBufferDurationSelector :: Selector
setPreferredForwardBufferDurationSelector = mkSelector "setPreferredForwardBufferDuration:"

-- | @Selector@ for @audioTimePitchAlgorithm@
audioTimePitchAlgorithmSelector :: Selector
audioTimePitchAlgorithmSelector = mkSelector "audioTimePitchAlgorithm"

-- | @Selector@ for @setAudioTimePitchAlgorithm:@
setAudioTimePitchAlgorithmSelector :: Selector
setAudioTimePitchAlgorithmSelector = mkSelector "setAudioTimePitchAlgorithm:"

-- | @Selector@ for @audioSpatializationAllowed@
audioSpatializationAllowedSelector :: Selector
audioSpatializationAllowedSelector = mkSelector "audioSpatializationAllowed"

-- | @Selector@ for @setAudioSpatializationAllowed:@
setAudioSpatializationAllowedSelector :: Selector
setAudioSpatializationAllowedSelector = mkSelector "setAudioSpatializationAllowed:"

-- | @Selector@ for @allowedAudioSpatializationFormats@
allowedAudioSpatializationFormatsSelector :: Selector
allowedAudioSpatializationFormatsSelector = mkSelector "allowedAudioSpatializationFormats"

-- | @Selector@ for @setAllowedAudioSpatializationFormats:@
setAllowedAudioSpatializationFormatsSelector :: Selector
setAllowedAudioSpatializationFormatsSelector = mkSelector "setAllowedAudioSpatializationFormats:"

-- | @Selector@ for @audioMix@
audioMixSelector :: Selector
audioMixSelector = mkSelector "audioMix"

-- | @Selector@ for @setAudioMix:@
setAudioMixSelector :: Selector
setAudioMixSelector = mkSelector "setAudioMix:"

-- | @Selector@ for @seekingWaitsForVideoCompositionRendering@
seekingWaitsForVideoCompositionRenderingSelector :: Selector
seekingWaitsForVideoCompositionRenderingSelector = mkSelector "seekingWaitsForVideoCompositionRendering"

-- | @Selector@ for @setSeekingWaitsForVideoCompositionRendering:@
setSeekingWaitsForVideoCompositionRenderingSelector :: Selector
setSeekingWaitsForVideoCompositionRenderingSelector = mkSelector "setSeekingWaitsForVideoCompositionRendering:"

-- | @Selector@ for @videoApertureMode@
videoApertureModeSelector :: Selector
videoApertureModeSelector = mkSelector "videoApertureMode"

-- | @Selector@ for @setVideoApertureMode:@
setVideoApertureModeSelector :: Selector
setVideoApertureModeSelector = mkSelector "setVideoApertureMode:"

-- | @Selector@ for @appliesPerFrameHDRDisplayMetadata@
appliesPerFrameHDRDisplayMetadataSelector :: Selector
appliesPerFrameHDRDisplayMetadataSelector = mkSelector "appliesPerFrameHDRDisplayMetadata"

-- | @Selector@ for @setAppliesPerFrameHDRDisplayMetadata:@
setAppliesPerFrameHDRDisplayMetadataSelector :: Selector
setAppliesPerFrameHDRDisplayMetadataSelector = mkSelector "setAppliesPerFrameHDRDisplayMetadata:"

-- | @Selector@ for @seekableTimeRanges@
seekableTimeRangesSelector :: Selector
seekableTimeRangesSelector = mkSelector "seekableTimeRanges"

-- | @Selector@ for @timebase@
timebaseSelector :: Selector
timebaseSelector = mkSelector "timebase"

-- | @Selector@ for @canPlayFastForward@
canPlayFastForwardSelector :: Selector
canPlayFastForwardSelector = mkSelector "canPlayFastForward"

-- | @Selector@ for @canPlaySlowForward@
canPlaySlowForwardSelector :: Selector
canPlaySlowForwardSelector = mkSelector "canPlaySlowForward"

-- | @Selector@ for @canPlayReverse@
canPlayReverseSelector :: Selector
canPlayReverseSelector = mkSelector "canPlayReverse"

-- | @Selector@ for @canPlaySlowReverse@
canPlaySlowReverseSelector :: Selector
canPlaySlowReverseSelector = mkSelector "canPlaySlowReverse"

-- | @Selector@ for @canPlayFastReverse@
canPlayFastReverseSelector :: Selector
canPlayFastReverseSelector = mkSelector "canPlayFastReverse"

-- | @Selector@ for @canStepForward@
canStepForwardSelector :: Selector
canStepForwardSelector = mkSelector "canStepForward"

-- | @Selector@ for @canStepBackward@
canStepBackwardSelector :: Selector
canStepBackwardSelector = mkSelector "canStepBackward"

-- | @Selector@ for @automaticallyPreservesTimeOffsetFromLive@
automaticallyPreservesTimeOffsetFromLiveSelector :: Selector
automaticallyPreservesTimeOffsetFromLiveSelector = mkSelector "automaticallyPreservesTimeOffsetFromLive"

-- | @Selector@ for @setAutomaticallyPreservesTimeOffsetFromLive:@
setAutomaticallyPreservesTimeOffsetFromLiveSelector :: Selector
setAutomaticallyPreservesTimeOffsetFromLiveSelector = mkSelector "setAutomaticallyPreservesTimeOffsetFromLive:"

-- | @Selector@ for @asset@
assetSelector :: Selector
assetSelector = mkSelector "asset"

-- | @Selector@ for @tracks@
tracksSelector :: Selector
tracksSelector = mkSelector "tracks"

-- | @Selector@ for @automaticallyLoadedAssetKeys@
automaticallyLoadedAssetKeysSelector :: Selector
automaticallyLoadedAssetKeysSelector = mkSelector "automaticallyLoadedAssetKeys"

