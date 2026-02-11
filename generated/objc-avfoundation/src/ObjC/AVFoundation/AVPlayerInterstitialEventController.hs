{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerInterstitialEventController allows you to specify a schedule of interstitial events for items played by a primary player. By creating an instance of AVPlayerInterstitialEventController and setting a schedule of interstitial events, you pre-empt directives the are intrinsic to the items played by the primary player, if any exist, causing them to be ignored.
--
-- The schedule of interstitial events is specified as an array of AVPlayerInterstitialEvents. For each AVPlayerInterstitialEvent, when the primary player's current item is the primary item of the interstitial event and its currentDate reaches the date of the event, playback of the primary item by the primary player is temporarily suspended, i.e. its timeControlStatus changes to AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate and its reasonForWaitingToPlay will change to AVPlayerWaitingDuringInterstitialEventReason. During this suspension, playback of items that replicate the interstitial template items of the event are played by the interstitial player, which temporarily assumes the output configuration of the primary player; for example, its visual content will be routed to AVPlayerLayers that reference the primary player. Once the interstitial player has advanced through playback of the interstitial items specified by the event or its current item otherwise becomes nil, playback of the primary content will resume, at an offset from the time at which it was suspended as specified by the event.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerInterstitialEventController@.
module ObjC.AVFoundation.AVPlayerInterstitialEventController
  ( AVPlayerInterstitialEventController
  , IsAVPlayerInterstitialEventController(..)
  , interstitialEventControllerWithPrimaryPlayer
  , initWithPrimaryPlayer
  , skipCurrentEvent
  , events
  , setEvents
  , localizedStringsBundle
  , setLocalizedStringsBundle
  , localizedStringsTableName
  , setLocalizedStringsTableName
  , interstitialEventControllerWithPrimaryPlayerSelector
  , initWithPrimaryPlayerSelector
  , skipCurrentEventSelector
  , eventsSelector
  , setEventsSelector
  , localizedStringsBundleSelector
  , setLocalizedStringsBundleSelector
  , localizedStringsTableNameSelector
  , setLocalizedStringsTableNameSelector


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

-- | Returns an instance of AVPlayerInterstitialEvent for use in observing and scheduling interstitial playback.
--
-- This method throws an exception if the primary player is an interstitial player.
--
-- - Parameter primaryPlayer: The AVPlayer that will play the primaryItems of the receiver's interstitial events.
--
-- - Returns: An instance of AVPlayerInterstitialEventController.
--
-- ObjC selector: @+ interstitialEventControllerWithPrimaryPlayer:@
interstitialEventControllerWithPrimaryPlayer :: IsAVPlayer primaryPlayer => primaryPlayer -> IO (Id AVPlayerInterstitialEventController)
interstitialEventControllerWithPrimaryPlayer primaryPlayer =
  do
    cls' <- getRequiredClass "AVPlayerInterstitialEventController"
    withObjCPtr primaryPlayer $ \raw_primaryPlayer ->
      sendClassMsg cls' (mkSelector "interstitialEventControllerWithPrimaryPlayer:") (retPtr retVoid) [argPtr (castPtr raw_primaryPlayer :: Ptr ())] >>= retainedObject . castPtr

-- | This method throws an exception if the primary player is an interstitial player.
--
-- ObjC selector: @- initWithPrimaryPlayer:@
initWithPrimaryPlayer :: (IsAVPlayerInterstitialEventController avPlayerInterstitialEventController, IsAVPlayer primaryPlayer) => avPlayerInterstitialEventController -> primaryPlayer -> IO (Id AVPlayerInterstitialEventController)
initWithPrimaryPlayer avPlayerInterstitialEventController  primaryPlayer =
withObjCPtr primaryPlayer $ \raw_primaryPlayer ->
    sendMsg avPlayerInterstitialEventController (mkSelector "initWithPrimaryPlayer:") (retPtr retVoid) [argPtr (castPtr raw_primaryPlayer :: Ptr ())] >>= ownedObject . castPtr

-- | Causes the playback of the currently playing interstital event to be abandoned.
--
-- Note that coinciding events will NOT be skipped. This results in AVPlayerInterstitialEventMonitorCurrentEventSkippedNotification being posted. Has no effect while the currentEvent is nil.
--
-- ObjC selector: @- skipCurrentEvent@
skipCurrentEvent :: IsAVPlayerInterstitialEventController avPlayerInterstitialEventController => avPlayerInterstitialEventController -> IO ()
skipCurrentEvent avPlayerInterstitialEventController  =
  sendMsg avPlayerInterstitialEventController (mkSelector "skipCurrentEvent") retVoid []

-- | Specifies the current schedule of interstitial events.
--
-- Setting this property to a non-nil value cancels and overrides all previously scheduled future interstitial events, including those that are intrinsically specified by the content of primary items, such as directives carried by HLS media playlists. Setting it to nil causes its value to be reset in accordance with the content of the current primary item.
--
-- If you change the value of events during an interstitial event and the current event is not included in the new value of events, the current event is nevertheless allowed to continue until completion. If you wish to cancel the current event, use -cancelCurrentEventWithResumptionOffset:.
--
-- If interstitial events are scheduled with dates that coincide either with the date of another scheduled interstitial event or with the date range of the primary content that's omitted according to the resumption offset of another scheduled interstitial event, the primary content will remain suspended until all coinciding interstitial events have been completed. The effective resumption offset will be the sum of the resumption offsets of the coinciding interstitial events. (Note that the sum of a numeric CMTime and kCMTimeIndefinite is kCMTimeIndefinite.)
--
-- If interstitial events are scheduled for the same date, they are ordered according to their position in the events array.
--
-- The receiver will make a copy of the events that are set on it. Subsequent mutations on the original events will have no effect on the copy.
--
-- An NSInvalidArgumentException will be raised if an under-specified AVPlayerInterstitialEvent is set, such as one with a nil primaryItem, or with neither a time nor a date.
--
-- ObjC selector: @- events@
events :: IsAVPlayerInterstitialEventController avPlayerInterstitialEventController => avPlayerInterstitialEventController -> IO (Id NSArray)
events avPlayerInterstitialEventController  =
  sendMsg avPlayerInterstitialEventController (mkSelector "events") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies the current schedule of interstitial events.
--
-- Setting this property to a non-nil value cancels and overrides all previously scheduled future interstitial events, including those that are intrinsically specified by the content of primary items, such as directives carried by HLS media playlists. Setting it to nil causes its value to be reset in accordance with the content of the current primary item.
--
-- If you change the value of events during an interstitial event and the current event is not included in the new value of events, the current event is nevertheless allowed to continue until completion. If you wish to cancel the current event, use -cancelCurrentEventWithResumptionOffset:.
--
-- If interstitial events are scheduled with dates that coincide either with the date of another scheduled interstitial event or with the date range of the primary content that's omitted according to the resumption offset of another scheduled interstitial event, the primary content will remain suspended until all coinciding interstitial events have been completed. The effective resumption offset will be the sum of the resumption offsets of the coinciding interstitial events. (Note that the sum of a numeric CMTime and kCMTimeIndefinite is kCMTimeIndefinite.)
--
-- If interstitial events are scheduled for the same date, they are ordered according to their position in the events array.
--
-- The receiver will make a copy of the events that are set on it. Subsequent mutations on the original events will have no effect on the copy.
--
-- An NSInvalidArgumentException will be raised if an under-specified AVPlayerInterstitialEvent is set, such as one with a nil primaryItem, or with neither a time nor a date.
--
-- ObjC selector: @- setEvents:@
setEvents :: (IsAVPlayerInterstitialEventController avPlayerInterstitialEventController, IsNSArray value) => avPlayerInterstitialEventController -> value -> IO ()
setEvents avPlayerInterstitialEventController  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerInterstitialEventController (mkSelector "setEvents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The bundle that contains the localized strings to be used by the AVPlayerInterstitialEventController.
--
-- If the value of the property is nil, any UI elements triggered by the AVPlayerInterstitialEventController, such as the skip button, may contain a generic label based on the implementation of the UI that's in use. To ensure the best available user experience in various playback configurations, including external playback, set a value for this property that provides localized translations of skip control labels.
--
-- ObjC selector: @- localizedStringsBundle@
localizedStringsBundle :: IsAVPlayerInterstitialEventController avPlayerInterstitialEventController => avPlayerInterstitialEventController -> IO (Id NSBundle)
localizedStringsBundle avPlayerInterstitialEventController  =
  sendMsg avPlayerInterstitialEventController (mkSelector "localizedStringsBundle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The bundle that contains the localized strings to be used by the AVPlayerInterstitialEventController.
--
-- If the value of the property is nil, any UI elements triggered by the AVPlayerInterstitialEventController, such as the skip button, may contain a generic label based on the implementation of the UI that's in use. To ensure the best available user experience in various playback configurations, including external playback, set a value for this property that provides localized translations of skip control labels.
--
-- ObjC selector: @- setLocalizedStringsBundle:@
setLocalizedStringsBundle :: (IsAVPlayerInterstitialEventController avPlayerInterstitialEventController, IsNSBundle value) => avPlayerInterstitialEventController -> value -> IO ()
setLocalizedStringsBundle avPlayerInterstitialEventController  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerInterstitialEventController (mkSelector "setLocalizedStringsBundle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The name of the table in the bundle that contains the localized strings to be used by the AVPlayerInterstitialEventController.
--
-- If the value of the property is nil, it will default to "Localizable"
--
-- ObjC selector: @- localizedStringsTableName@
localizedStringsTableName :: IsAVPlayerInterstitialEventController avPlayerInterstitialEventController => avPlayerInterstitialEventController -> IO (Id NSString)
localizedStringsTableName avPlayerInterstitialEventController  =
  sendMsg avPlayerInterstitialEventController (mkSelector "localizedStringsTableName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the table in the bundle that contains the localized strings to be used by the AVPlayerInterstitialEventController.
--
-- If the value of the property is nil, it will default to "Localizable"
--
-- ObjC selector: @- setLocalizedStringsTableName:@
setLocalizedStringsTableName :: (IsAVPlayerInterstitialEventController avPlayerInterstitialEventController, IsNSString value) => avPlayerInterstitialEventController -> value -> IO ()
setLocalizedStringsTableName avPlayerInterstitialEventController  value =
withObjCPtr value $ \raw_value ->
    sendMsg avPlayerInterstitialEventController (mkSelector "setLocalizedStringsTableName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interstitialEventControllerWithPrimaryPlayer:@
interstitialEventControllerWithPrimaryPlayerSelector :: Selector
interstitialEventControllerWithPrimaryPlayerSelector = mkSelector "interstitialEventControllerWithPrimaryPlayer:"

-- | @Selector@ for @initWithPrimaryPlayer:@
initWithPrimaryPlayerSelector :: Selector
initWithPrimaryPlayerSelector = mkSelector "initWithPrimaryPlayer:"

-- | @Selector@ for @skipCurrentEvent@
skipCurrentEventSelector :: Selector
skipCurrentEventSelector = mkSelector "skipCurrentEvent"

-- | @Selector@ for @events@
eventsSelector :: Selector
eventsSelector = mkSelector "events"

-- | @Selector@ for @setEvents:@
setEventsSelector :: Selector
setEventsSelector = mkSelector "setEvents:"

-- | @Selector@ for @localizedStringsBundle@
localizedStringsBundleSelector :: Selector
localizedStringsBundleSelector = mkSelector "localizedStringsBundle"

-- | @Selector@ for @setLocalizedStringsBundle:@
setLocalizedStringsBundleSelector :: Selector
setLocalizedStringsBundleSelector = mkSelector "setLocalizedStringsBundle:"

-- | @Selector@ for @localizedStringsTableName@
localizedStringsTableNameSelector :: Selector
localizedStringsTableNameSelector = mkSelector "localizedStringsTableName"

-- | @Selector@ for @setLocalizedStringsTableName:@
setLocalizedStringsTableNameSelector :: Selector
setLocalizedStringsTableNameSelector = mkSelector "setLocalizedStringsTableName:"

