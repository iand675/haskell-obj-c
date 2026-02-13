{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVPlayerInterstitialEventMonitor allows you to observe the scheduling and progress of interstitial events, specified either intrinsically within the content of primary items, such as via use of directives carried by HLS media playlists, or via use of an AVPlayerInterstitialEventController.
--
-- The schedule of interstitial events is provided as an array of AVPlayerInterstitialEvents. For each AVPlayerInterstitialEvent, when the primary player's current item is the primary item of the interstitial event and its currentDate reaches the date of the event, playback of the primary item by the primary player is temporarily suspended, i.e. its timeControlStatus changes to AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate and its reasonForWaitingToPlay will change to AVPlayerWaitingDuringInterstitialEventReason. During this suspension, playback of items that replicate the interstitial template items of the event are played by the interstitial player, which temporarily assumes the output configuration of the primary player; for example, its visual content will be routed to AVPlayerLayers that reference the primary player. Once the interstitial player has advanced through playback of the interstitial items specified by the event or its current item otherwise becomes nil, playback of the primary content will resume, at an offset from the time at which it was suspended as specified by the event.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerInterstitialEventMonitor@.
module ObjC.AVFoundation.AVPlayerInterstitialEventMonitor
  ( AVPlayerInterstitialEventMonitor
  , IsAVPlayerInterstitialEventMonitor(..)
  , interstitialEventMonitorWithPrimaryPlayer
  , initWithPrimaryPlayer
  , primaryPlayer
  , interstitialPlayer
  , events
  , currentEvent
  , currentEventSkippableState
  , currentEventSkipControlLabel
  , currentEventSelector
  , currentEventSkipControlLabelSelector
  , currentEventSkippableStateSelector
  , eventsSelector
  , initWithPrimaryPlayerSelector
  , interstitialEventMonitorWithPrimaryPlayerSelector
  , interstitialPlayerSelector
  , primaryPlayerSelector

  -- * Enum types
  , AVPlayerInterstitialEventSkippableEventState(AVPlayerInterstitialEventSkippableEventState)
  , pattern AVPlayerInterstitialEventSkippableEventStateNotSkippable
  , pattern AVPlayerInterstitialEventSkippableEventStateNotYetEligible
  , pattern AVPlayerInterstitialEventSkippableEventStateEligible
  , pattern AVPlayerInterstitialEventSkippableEventStateNoLongerEligible

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

-- | Returns an instance of AVPlayerInterstitialEvent for use in observing and scheduling interstitial playback.
--
-- - Parameter primaryPlayer: The AVPlayer that will play the primaryItems of the receiver's interstitial events.
--
-- - Returns: An instance of AVPlayerInterstitialEventMonitor.
--
-- ObjC selector: @+ interstitialEventMonitorWithPrimaryPlayer:@
interstitialEventMonitorWithPrimaryPlayer :: IsAVPlayer primaryPlayer => primaryPlayer -> IO (Id AVPlayerInterstitialEventMonitor)
interstitialEventMonitorWithPrimaryPlayer primaryPlayer =
  do
    cls' <- getRequiredClass "AVPlayerInterstitialEventMonitor"
    sendClassMessage cls' interstitialEventMonitorWithPrimaryPlayerSelector (toAVPlayer primaryPlayer)

-- | @- initWithPrimaryPlayer:@
initWithPrimaryPlayer :: (IsAVPlayerInterstitialEventMonitor avPlayerInterstitialEventMonitor, IsAVPlayer primaryPlayer) => avPlayerInterstitialEventMonitor -> primaryPlayer -> IO (Id AVPlayerInterstitialEventMonitor)
initWithPrimaryPlayer avPlayerInterstitialEventMonitor primaryPlayer =
  sendOwnedMessage avPlayerInterstitialEventMonitor initWithPrimaryPlayerSelector (toAVPlayer primaryPlayer)

-- | The AVPlayer that will play the primaryItems of the receiver's interstitial events.
--
-- ObjC selector: @- primaryPlayer@
primaryPlayer :: IsAVPlayerInterstitialEventMonitor avPlayerInterstitialEventMonitor => avPlayerInterstitialEventMonitor -> IO (Id AVPlayer)
primaryPlayer avPlayerInterstitialEventMonitor =
  sendMessage avPlayerInterstitialEventMonitor primaryPlayerSelector

-- | The AVQueuePlayer that will play interstitial items during suspension of playback of primary items.
--
-- ObjC selector: @- interstitialPlayer@
interstitialPlayer :: IsAVPlayerInterstitialEventMonitor avPlayerInterstitialEventMonitor => avPlayerInterstitialEventMonitor -> IO (Id AVQueuePlayer)
interstitialPlayer avPlayerInterstitialEventMonitor =
  sendMessage avPlayerInterstitialEventMonitor interstitialPlayerSelector

-- | Provides the current schedule of interstitial events, specified either intrinsically within the content of primary items, such as via use of directives carried by HLS media playlists, or via use of an AVPlayerInterstitialEventController.
--
-- When interstitial events follow a schedule specified intrinsically within the content of primary items, the value of this property will typically change whenever the currentItem of the primaryPlayer changes. For HLS content that specifies interstitials via the use of DATERANGE tags, the value of this property may also change whenever the set of DATERANGE tags in the currentItem's media playlist changes. When interstitial events follow a schedule specified via use of an AVPlayerInterstitialEventController, the value of this property changes only when a new schedule is set on the AVPlayerInterstitialEventController. The events returned in this array are immutable. Attempting to mutate them will trigger an exception. To alter an event, make a copy and mutate the copy.
--
-- ObjC selector: @- events@
events :: IsAVPlayerInterstitialEventMonitor avPlayerInterstitialEventMonitor => avPlayerInterstitialEventMonitor -> IO (Id NSArray)
events avPlayerInterstitialEventMonitor =
  sendMessage avPlayerInterstitialEventMonitor eventsSelector

-- | The current interstitial event. Has a value of nil during playback of primary content by the primary player.
--
-- ObjC selector: @- currentEvent@
currentEvent :: IsAVPlayerInterstitialEventMonitor avPlayerInterstitialEventMonitor => avPlayerInterstitialEventMonitor -> IO (Id AVPlayerInterstitialEvent)
currentEvent avPlayerInterstitialEventMonitor =
  sendMessage avPlayerInterstitialEventMonitor currentEventSelector

-- | The skippable event state for the currentEvent.
--
-- If currentEvent is nil, then the value will be AVPlayerInterstitialEventSkippableEventStateNotSkippable.
--
-- ObjC selector: @- currentEventSkippableState@
currentEventSkippableState :: IsAVPlayerInterstitialEventMonitor avPlayerInterstitialEventMonitor => avPlayerInterstitialEventMonitor -> IO AVPlayerInterstitialEventSkippableEventState
currentEventSkippableState avPlayerInterstitialEventMonitor =
  sendMessage avPlayerInterstitialEventMonitor currentEventSkippableStateSelector

-- | The skip control label for the currentEvent.
--
-- If a localizedStringsBundle has been set on the AVPlayerInterstitialEventController, and a skipControlLocalizedLabelBundleKey is set on the currentEvent, then this value will be the localized string that was matched to the event's skipControlLocalizedLabelBundleKey for the corresponding system language in the supplied Bundle, if any. If currentEvent is nil, then the value will be nil.
--
-- ObjC selector: @- currentEventSkipControlLabel@
currentEventSkipControlLabel :: IsAVPlayerInterstitialEventMonitor avPlayerInterstitialEventMonitor => avPlayerInterstitialEventMonitor -> IO (Id NSString)
currentEventSkipControlLabel avPlayerInterstitialEventMonitor =
  sendMessage avPlayerInterstitialEventMonitor currentEventSkipControlLabelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interstitialEventMonitorWithPrimaryPlayer:@
interstitialEventMonitorWithPrimaryPlayerSelector :: Selector '[Id AVPlayer] (Id AVPlayerInterstitialEventMonitor)
interstitialEventMonitorWithPrimaryPlayerSelector = mkSelector "interstitialEventMonitorWithPrimaryPlayer:"

-- | @Selector@ for @initWithPrimaryPlayer:@
initWithPrimaryPlayerSelector :: Selector '[Id AVPlayer] (Id AVPlayerInterstitialEventMonitor)
initWithPrimaryPlayerSelector = mkSelector "initWithPrimaryPlayer:"

-- | @Selector@ for @primaryPlayer@
primaryPlayerSelector :: Selector '[] (Id AVPlayer)
primaryPlayerSelector = mkSelector "primaryPlayer"

-- | @Selector@ for @interstitialPlayer@
interstitialPlayerSelector :: Selector '[] (Id AVQueuePlayer)
interstitialPlayerSelector = mkSelector "interstitialPlayer"

-- | @Selector@ for @events@
eventsSelector :: Selector '[] (Id NSArray)
eventsSelector = mkSelector "events"

-- | @Selector@ for @currentEvent@
currentEventSelector :: Selector '[] (Id AVPlayerInterstitialEvent)
currentEventSelector = mkSelector "currentEvent"

-- | @Selector@ for @currentEventSkippableState@
currentEventSkippableStateSelector :: Selector '[] AVPlayerInterstitialEventSkippableEventState
currentEventSkippableStateSelector = mkSelector "currentEventSkippableState"

-- | @Selector@ for @currentEventSkipControlLabel@
currentEventSkipControlLabelSelector :: Selector '[] (Id NSString)
currentEventSkipControlLabelSelector = mkSelector "currentEventSkipControlLabel"

