{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerInterstitialEvent provides instructions for temporarily suspending the playback of primary content in order to play alternative interstitial content instead, resuming playback of the primary content when playback of the interstitial content is complete or is canceled.
--
-- The primary content is specified as an instance of AVPlayerItem, designated as the primary item of the interstitial event.
--
-- The timing of interstitial playback is specified as a date within the date range of the primary item. Interstitial events are currently possible only for items with an intrinsic mapping from their timeline to real-time dates.
--
-- The alternative interstitial content is specified as an array of one or more AVPlayerItems that will be used as templates for the creation of items for interstitial playback. In other words, these template items are not the actual items that will be played during interstitial playback; instead they are used to generate the items that are to be played, with property values that match the configuration of your template items.
--
-- If you wish to observe the scheduling and progress of interstitial events, use an AVPlayerInterstitialEventMonitor. If you wish to specify your own schedule of interstitial events, use an AVPlayerInterstitialEventController.
--
-- Note that while previously AVPlayerInterstitialEvent was an immutable object, it is now mutable. This allows it to be created and customized before being set on an AVPlayerInterstitialEventController.
--
-- Generated bindings for @AVPlayerInterstitialEvent@.
module ObjC.AVFoundation.AVPlayerInterstitialEvent
  ( AVPlayerInterstitialEvent
  , IsAVPlayerInterstitialEvent(..)
  , init_
  , new
  , interstitialEventWithPrimaryItem_date
  , primaryItem
  , identifier
  , date
  , templateItems
  , restrictions
  , alignsStartWithPrimarySegmentBoundary
  , alignsResumptionWithPrimarySegmentBoundary
  , cue
  , willPlayOnce
  , userDefinedAttributes
  , assetListResponse
  , timelineOccupancy
  , supplementsPrimaryContent
  , contentMayVary
  , skipControlLocalizedLabelBundleKey
  , setPrimaryItem
  , setIdentifier
  , setDate
  , setTemplateItems
  , setRestrictions
  , setAlignsStartWithPrimarySegmentBoundary
  , setAlignsResumptionWithPrimarySegmentBoundary
  , setCue
  , setWillPlayOnce
  , setUserDefinedAttributes
  , setTimelineOccupancy
  , setSupplementsPrimaryContent
  , setContentMayVary
  , setSkipControlLocalizedLabelBundleKey
  , alignsResumptionWithPrimarySegmentBoundarySelector
  , alignsStartWithPrimarySegmentBoundarySelector
  , assetListResponseSelector
  , contentMayVarySelector
  , cueSelector
  , dateSelector
  , identifierSelector
  , initSelector
  , interstitialEventWithPrimaryItem_dateSelector
  , newSelector
  , primaryItemSelector
  , restrictionsSelector
  , setAlignsResumptionWithPrimarySegmentBoundarySelector
  , setAlignsStartWithPrimarySegmentBoundarySelector
  , setContentMayVarySelector
  , setCueSelector
  , setDateSelector
  , setIdentifierSelector
  , setPrimaryItemSelector
  , setRestrictionsSelector
  , setSkipControlLocalizedLabelBundleKeySelector
  , setSupplementsPrimaryContentSelector
  , setTemplateItemsSelector
  , setTimelineOccupancySelector
  , setUserDefinedAttributesSelector
  , setWillPlayOnceSelector
  , skipControlLocalizedLabelBundleKeySelector
  , supplementsPrimaryContentSelector
  , templateItemsSelector
  , timelineOccupancySelector
  , userDefinedAttributesSelector
  , willPlayOnceSelector

  -- * Enum types
  , AVPlayerInterstitialEventRestrictions(AVPlayerInterstitialEventRestrictions)
  , pattern AVPlayerInterstitialEventRestrictionNone
  , pattern AVPlayerInterstitialEventRestrictionConstrainsSeekingForwardInPrimaryContent
  , pattern AVPlayerInterstitialEventRestrictionRequiresPlaybackAtPreferredRateForAdvancement
  , pattern AVPlayerInterstitialEventRestrictionDefaultPolicy
  , AVPlayerInterstitialEventTimelineOccupancy(AVPlayerInterstitialEventTimelineOccupancy)
  , pattern AVPlayerInterstitialEventTimelineOccupancySinglePoint
  , pattern AVPlayerInterstitialEventTimelineOccupancyFill

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id AVPlayerInterstitialEvent)
init_ avPlayerInterstitialEvent =
  sendOwnedMessage avPlayerInterstitialEvent initSelector

-- | @+ new@
new :: IO (Id AVPlayerInterstitialEvent)
new  =
  do
    cls' <- getRequiredClass "AVPlayerInterstitialEvent"
    sendOwnedClassMessage cls' newSelector

-- | Returns an instance of AVPlayerInterstitialEvent for use in scheduling interstitial playback.
--
-- - Parameter primaryItem: An AVPlayerItem representing the primary content during the playback of which the interstitial event should occur. The primaryItem must have an AVAsset that provides an intrinsic mapping from its timeline to real-time dates. - Parameter date: The date within the date range of the primary item at which playback of the primary content should be temporarily suspended and the interstitial items played.
--
-- - Returns: An instance of AVPlayerInterstitialEvent.
--
-- ObjC selector: @+ interstitialEventWithPrimaryItem:date:@
interstitialEventWithPrimaryItem_date :: (IsAVPlayerItem primaryItem, IsNSDate date) => primaryItem -> date -> IO (Id AVPlayerInterstitialEvent)
interstitialEventWithPrimaryItem_date primaryItem date =
  do
    cls' <- getRequiredClass "AVPlayerInterstitialEvent"
    sendClassMessage cls' interstitialEventWithPrimaryItem_dateSelector (toAVPlayerItem primaryItem) (toNSDate date)

-- | An AVPlayerItem representing the primary content during the playback of which the interstitial event should occur. The primaryItem must have an AVAsset that provides an intrinsic mapping from its timeline to real-time dates.
--
-- ObjC selector: @- primaryItem@
primaryItem :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id AVPlayerItem)
primaryItem avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent primaryItemSelector

-- | An external identifier for the event.
--
-- If an event is set on an AVPlayerInterstitialEventController that already has an event with the same identifier, the old event will be replaced by the new one.
--
-- ObjC selector: @- identifier@
identifier :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id NSString)
identifier avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent identifierSelector

-- | The date within the date range of the primary item at which playback of the primary content should be temporarily suspended and the interstitial items played.
--
-- Will have a value of nil if the event was initialized with a time instead of a date.
--
-- ObjC selector: @- date@
date :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id NSDate)
date avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent dateSelector

-- | An array of AVPlayerItems with configurations that will be reproduced for the playback of interstitial content.
--
-- If you want the instances of AVURLAsset used during interstitial playback to be identical to the ones you specify for templateItems in AVPlayerInterstitialEvents that you set on an AVPlayerInterstitialEventController, rather than equivalent AVURLAssets with the same URL, you must create them with a value for the key AVURLAssetPrimarySessionIdentifierKey that's equal to the httpSessionIdentifier of the primary AVPlayerItem's asset. See AVAsset.h. This is especially useful if you require the use of a custom AVAssetResourceLoader delegate for interstitial assets.
--
-- An NSInvalidArgumentException will be raised if any of the template items employs an AVAsset that lacks a URL, such as an AVComposition.
--
-- ObjC selector: @- templateItems@
templateItems :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id NSArray)
templateItems avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent templateItemsSelector

-- | Indicates restrictions on the use of end user playback controls that are imposed by the event.
--
-- ObjC selector: @- restrictions@
restrictions :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO AVPlayerInterstitialEventRestrictions
restrictions avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent restrictionsSelector

-- | Specifies that the start time of interstitial playback should be snapped to a segment boundary of the primary asset
--
-- If true, the start time or date of the interstitial will be adjusted to the nearest segment boundary when the primary player is playing an HTTP Live Streaming asset.
--
-- ObjC selector: @- alignsStartWithPrimarySegmentBoundary@
alignsStartWithPrimarySegmentBoundary :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO Bool
alignsStartWithPrimarySegmentBoundary avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent alignsStartWithPrimarySegmentBoundarySelector

-- | Specifies that the resumption time of primary playback should be snapped to a segment boundary of the primary asset
--
-- If true, the resumption time of primary playback following an interstitial will be adjusted to the nearest segment boundary when the primary player is playing an HTTP Live Streaming asset.
--
-- ObjC selector: @- alignsResumptionWithPrimarySegmentBoundary@
alignsResumptionWithPrimarySegmentBoundary :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO Bool
alignsResumptionWithPrimarySegmentBoundary avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent alignsResumptionWithPrimarySegmentBoundarySelector

-- | The cue property is used to schedule event playback at a predefined position of primary playback.
--
-- ObjC selector: @- cue@
cue :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id NSString)
cue avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent cueSelector

-- | Specifies that the interstitial should be scheduled for playback once only, and suppressed for subsequent replay.
--
-- The "once" provision takes effect at the start of interstitial playback. The interstitial will not be scheduled again even if the first playback is canceled before completion.
--
-- ObjC selector: @- willPlayOnce@
willPlayOnce :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO Bool
willPlayOnce avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent willPlayOnceSelector

-- | Attributes of the event defined by the content vendor or the client.
--
-- Dictionary keys are attribute names. Dictionary values are attribute values.
--
-- ObjC selector: @- userDefinedAttributes@
userDefinedAttributes :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id NSDictionary)
userDefinedAttributes avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent userDefinedAttributesSelector

-- | The asset list JSON response as a dictionary, or nil if no asset list response has been loaded for the event.
--
-- If the AVPlayerInterstitialEvent's templateItems is empty and the assetListResponse is nil, then an asset list read is expected. If the AVPlayerInterstitialEvent's templateItems is not empty and the assetListResponse is nil, then an asset list read is not expected.
--
-- ObjC selector: @- assetListResponse@
assetListResponse :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id NSDictionary)
assetListResponse avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent assetListResponseSelector

-- | Indicates this event's occupancy on AVPlayerItemIntegratedTimeline. The default value is AVPlayerInterstitialEventTimelineSinglePointOccupancy.
--
-- ObjC selector: @- timelineOccupancy@
timelineOccupancy :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO AVPlayerInterstitialEventTimelineOccupancy
timelineOccupancy avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent timelineOccupancySelector

-- | Indicates this event will supplement the primary content and should be presented unified with the primary item. The default value is NO.
--
-- ObjC selector: @- supplementsPrimaryContent@
supplementsPrimaryContent :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO Bool
supplementsPrimaryContent avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent supplementsPrimaryContentSelector

-- | Indicates this event's content is dynamic and server may respond with different interstitial assets for other particpants in coordinated playback.
--
-- Indicates this event's content is dynamic and server may respond with different interstitial assets for other particpants in coordinated playback. If this value is set to NO and the primary asset is particpating in coordinated playback, this event will participate in coordinated playback as well. The default value is YES.
--
-- ObjC selector: @- contentMayVary@
contentMayVary :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO Bool
contentMayVary avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent contentMayVarySelector

-- | The key defined in the AVPlayerInterstitialEventController's localizedStringsBundle that points to the localized label for the skip button.
--
-- If the value of the property is nil, the skip button may contain a generic label depending on the implementation of the UI that's in use. To ensure the best available user experience in various playback configurations, including external playback, set a value for this property that provides localized translations of skip control labels.
--
-- ObjC selector: @- skipControlLocalizedLabelBundleKey@
skipControlLocalizedLabelBundleKey :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> IO (Id NSString)
skipControlLocalizedLabelBundleKey avPlayerInterstitialEvent =
  sendMessage avPlayerInterstitialEvent skipControlLocalizedLabelBundleKeySelector

-- | @- setPrimaryItem:@
setPrimaryItem :: (IsAVPlayerInterstitialEvent avPlayerInterstitialEvent, IsAVPlayerItem value) => avPlayerInterstitialEvent -> value -> IO ()
setPrimaryItem avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setPrimaryItemSelector (toAVPlayerItem value)

-- | @- setIdentifier:@
setIdentifier :: (IsAVPlayerInterstitialEvent avPlayerInterstitialEvent, IsNSString value) => avPlayerInterstitialEvent -> value -> IO ()
setIdentifier avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setIdentifierSelector (toNSString value)

-- | @- setDate:@
setDate :: (IsAVPlayerInterstitialEvent avPlayerInterstitialEvent, IsNSDate value) => avPlayerInterstitialEvent -> value -> IO ()
setDate avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setDateSelector (toNSDate value)

-- | @- setTemplateItems:@
setTemplateItems :: (IsAVPlayerInterstitialEvent avPlayerInterstitialEvent, IsNSArray value) => avPlayerInterstitialEvent -> value -> IO ()
setTemplateItems avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setTemplateItemsSelector (toNSArray value)

-- | @- setRestrictions:@
setRestrictions :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> AVPlayerInterstitialEventRestrictions -> IO ()
setRestrictions avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setRestrictionsSelector value

-- | @- setAlignsStartWithPrimarySegmentBoundary:@
setAlignsStartWithPrimarySegmentBoundary :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> Bool -> IO ()
setAlignsStartWithPrimarySegmentBoundary avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setAlignsStartWithPrimarySegmentBoundarySelector value

-- | @- setAlignsResumptionWithPrimarySegmentBoundary:@
setAlignsResumptionWithPrimarySegmentBoundary :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> Bool -> IO ()
setAlignsResumptionWithPrimarySegmentBoundary avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setAlignsResumptionWithPrimarySegmentBoundarySelector value

-- | @- setCue:@
setCue :: (IsAVPlayerInterstitialEvent avPlayerInterstitialEvent, IsNSString value) => avPlayerInterstitialEvent -> value -> IO ()
setCue avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setCueSelector (toNSString value)

-- | @- setWillPlayOnce:@
setWillPlayOnce :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> Bool -> IO ()
setWillPlayOnce avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setWillPlayOnceSelector value

-- | @- setUserDefinedAttributes:@
setUserDefinedAttributes :: (IsAVPlayerInterstitialEvent avPlayerInterstitialEvent, IsNSDictionary value) => avPlayerInterstitialEvent -> value -> IO ()
setUserDefinedAttributes avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setUserDefinedAttributesSelector (toNSDictionary value)

-- | @- setTimelineOccupancy:@
setTimelineOccupancy :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> AVPlayerInterstitialEventTimelineOccupancy -> IO ()
setTimelineOccupancy avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setTimelineOccupancySelector value

-- | @- setSupplementsPrimaryContent:@
setSupplementsPrimaryContent :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> Bool -> IO ()
setSupplementsPrimaryContent avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setSupplementsPrimaryContentSelector value

-- | @- setContentMayVary:@
setContentMayVary :: IsAVPlayerInterstitialEvent avPlayerInterstitialEvent => avPlayerInterstitialEvent -> Bool -> IO ()
setContentMayVary avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setContentMayVarySelector value

-- | @- setSkipControlLocalizedLabelBundleKey:@
setSkipControlLocalizedLabelBundleKey :: (IsAVPlayerInterstitialEvent avPlayerInterstitialEvent, IsNSString value) => avPlayerInterstitialEvent -> value -> IO ()
setSkipControlLocalizedLabelBundleKey avPlayerInterstitialEvent value =
  sendMessage avPlayerInterstitialEvent setSkipControlLocalizedLabelBundleKeySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerInterstitialEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerInterstitialEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @interstitialEventWithPrimaryItem:date:@
interstitialEventWithPrimaryItem_dateSelector :: Selector '[Id AVPlayerItem, Id NSDate] (Id AVPlayerInterstitialEvent)
interstitialEventWithPrimaryItem_dateSelector = mkSelector "interstitialEventWithPrimaryItem:date:"

-- | @Selector@ for @primaryItem@
primaryItemSelector :: Selector '[] (Id AVPlayerItem)
primaryItemSelector = mkSelector "primaryItem"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @templateItems@
templateItemsSelector :: Selector '[] (Id NSArray)
templateItemsSelector = mkSelector "templateItems"

-- | @Selector@ for @restrictions@
restrictionsSelector :: Selector '[] AVPlayerInterstitialEventRestrictions
restrictionsSelector = mkSelector "restrictions"

-- | @Selector@ for @alignsStartWithPrimarySegmentBoundary@
alignsStartWithPrimarySegmentBoundarySelector :: Selector '[] Bool
alignsStartWithPrimarySegmentBoundarySelector = mkSelector "alignsStartWithPrimarySegmentBoundary"

-- | @Selector@ for @alignsResumptionWithPrimarySegmentBoundary@
alignsResumptionWithPrimarySegmentBoundarySelector :: Selector '[] Bool
alignsResumptionWithPrimarySegmentBoundarySelector = mkSelector "alignsResumptionWithPrimarySegmentBoundary"

-- | @Selector@ for @cue@
cueSelector :: Selector '[] (Id NSString)
cueSelector = mkSelector "cue"

-- | @Selector@ for @willPlayOnce@
willPlayOnceSelector :: Selector '[] Bool
willPlayOnceSelector = mkSelector "willPlayOnce"

-- | @Selector@ for @userDefinedAttributes@
userDefinedAttributesSelector :: Selector '[] (Id NSDictionary)
userDefinedAttributesSelector = mkSelector "userDefinedAttributes"

-- | @Selector@ for @assetListResponse@
assetListResponseSelector :: Selector '[] (Id NSDictionary)
assetListResponseSelector = mkSelector "assetListResponse"

-- | @Selector@ for @timelineOccupancy@
timelineOccupancySelector :: Selector '[] AVPlayerInterstitialEventTimelineOccupancy
timelineOccupancySelector = mkSelector "timelineOccupancy"

-- | @Selector@ for @supplementsPrimaryContent@
supplementsPrimaryContentSelector :: Selector '[] Bool
supplementsPrimaryContentSelector = mkSelector "supplementsPrimaryContent"

-- | @Selector@ for @contentMayVary@
contentMayVarySelector :: Selector '[] Bool
contentMayVarySelector = mkSelector "contentMayVary"

-- | @Selector@ for @skipControlLocalizedLabelBundleKey@
skipControlLocalizedLabelBundleKeySelector :: Selector '[] (Id NSString)
skipControlLocalizedLabelBundleKeySelector = mkSelector "skipControlLocalizedLabelBundleKey"

-- | @Selector@ for @setPrimaryItem:@
setPrimaryItemSelector :: Selector '[Id AVPlayerItem] ()
setPrimaryItemSelector = mkSelector "setPrimaryItem:"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @setDate:@
setDateSelector :: Selector '[Id NSDate] ()
setDateSelector = mkSelector "setDate:"

-- | @Selector@ for @setTemplateItems:@
setTemplateItemsSelector :: Selector '[Id NSArray] ()
setTemplateItemsSelector = mkSelector "setTemplateItems:"

-- | @Selector@ for @setRestrictions:@
setRestrictionsSelector :: Selector '[AVPlayerInterstitialEventRestrictions] ()
setRestrictionsSelector = mkSelector "setRestrictions:"

-- | @Selector@ for @setAlignsStartWithPrimarySegmentBoundary:@
setAlignsStartWithPrimarySegmentBoundarySelector :: Selector '[Bool] ()
setAlignsStartWithPrimarySegmentBoundarySelector = mkSelector "setAlignsStartWithPrimarySegmentBoundary:"

-- | @Selector@ for @setAlignsResumptionWithPrimarySegmentBoundary:@
setAlignsResumptionWithPrimarySegmentBoundarySelector :: Selector '[Bool] ()
setAlignsResumptionWithPrimarySegmentBoundarySelector = mkSelector "setAlignsResumptionWithPrimarySegmentBoundary:"

-- | @Selector@ for @setCue:@
setCueSelector :: Selector '[Id NSString] ()
setCueSelector = mkSelector "setCue:"

-- | @Selector@ for @setWillPlayOnce:@
setWillPlayOnceSelector :: Selector '[Bool] ()
setWillPlayOnceSelector = mkSelector "setWillPlayOnce:"

-- | @Selector@ for @setUserDefinedAttributes:@
setUserDefinedAttributesSelector :: Selector '[Id NSDictionary] ()
setUserDefinedAttributesSelector = mkSelector "setUserDefinedAttributes:"

-- | @Selector@ for @setTimelineOccupancy:@
setTimelineOccupancySelector :: Selector '[AVPlayerInterstitialEventTimelineOccupancy] ()
setTimelineOccupancySelector = mkSelector "setTimelineOccupancy:"

-- | @Selector@ for @setSupplementsPrimaryContent:@
setSupplementsPrimaryContentSelector :: Selector '[Bool] ()
setSupplementsPrimaryContentSelector = mkSelector "setSupplementsPrimaryContent:"

-- | @Selector@ for @setContentMayVary:@
setContentMayVarySelector :: Selector '[Bool] ()
setContentMayVarySelector = mkSelector "setContentMayVary:"

-- | @Selector@ for @setSkipControlLocalizedLabelBundleKey:@
setSkipControlLocalizedLabelBundleKeySelector :: Selector '[Id NSString] ()
setSkipControlLocalizedLabelBundleKeySelector = mkSelector "setSkipControlLocalizedLabelBundleKey:"

