{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MAMusicHapticsManager@.
module ObjC.MediaAccessibility.MAMusicHapticsManager
  ( MAMusicHapticsManager
  , IsMAMusicHapticsManager(..)
  , checkHapticTrackAvailabilityForMediaMatchingCode_completionHandler
  , addStatusObserver
  , removeStatusObserver
  , init_
  , new
  , sharedManager
  , isActive
  , addStatusObserverSelector
  , checkHapticTrackAvailabilityForMediaMatchingCode_completionHandlerSelector
  , initSelector
  , isActiveSelector
  , newSelector
  , removeStatusObserverSelector
  , sharedManagerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaAccessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | In an asynchronous completion handler, returns whether a specific media track with the supplied ISRC has an available haptic track.
--
-- ObjC selector: @- checkHapticTrackAvailabilityForMediaMatchingCode:completionHandler:@
checkHapticTrackAvailabilityForMediaMatchingCode_completionHandler :: (IsMAMusicHapticsManager maMusicHapticsManager, IsNSString internationalStandardRecordingCode) => maMusicHapticsManager -> internationalStandardRecordingCode -> Ptr () -> IO ()
checkHapticTrackAvailabilityForMediaMatchingCode_completionHandler maMusicHapticsManager internationalStandardRecordingCode completionHandler =
  sendMessage maMusicHapticsManager checkHapticTrackAvailabilityForMediaMatchingCode_completionHandlerSelector (toNSString internationalStandardRecordingCode) completionHandler

-- | Determine the status of haptic playback for the now playing track asynchronously. This will only be delivered for the app that is the active Now Playing app.
--
-- ObjC selector: @- addStatusObserver:@
addStatusObserver :: IsMAMusicHapticsManager maMusicHapticsManager => maMusicHapticsManager -> Ptr () -> IO RawId
addStatusObserver maMusicHapticsManager statusHandler =
  sendMessage maMusicHapticsManager addStatusObserverSelector statusHandler

-- | @- removeStatusObserver:@
removeStatusObserver :: IsMAMusicHapticsManager maMusicHapticsManager => maMusicHapticsManager -> RawId -> IO ()
removeStatusObserver maMusicHapticsManager registrationToken =
  sendMessage maMusicHapticsManager removeStatusObserverSelector registrationToken

-- | @- init@
init_ :: IsMAMusicHapticsManager maMusicHapticsManager => maMusicHapticsManager -> IO (Id MAMusicHapticsManager)
init_ maMusicHapticsManager =
  sendOwnedMessage maMusicHapticsManager initSelector

-- | @+ new@
new :: IO (Id MAMusicHapticsManager)
new  =
  do
    cls' <- getRequiredClass "MAMusicHapticsManager"
    sendOwnedClassMessage cls' newSelector

-- | @+ sharedManager@
sharedManager :: IO (Id MAMusicHapticsManager)
sharedManager  =
  do
    cls' <- getRequiredClass "MAMusicHapticsManager"
    sendClassMessage cls' sharedManagerSelector

-- | Whether the user setting to indicate Music Haptics are currently active.
--
-- Returns: A boolean result.
--
-- ObjC selector: @- isActive@
isActive :: IsMAMusicHapticsManager maMusicHapticsManager => maMusicHapticsManager -> IO Bool
isActive maMusicHapticsManager =
  sendMessage maMusicHapticsManager isActiveSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkHapticTrackAvailabilityForMediaMatchingCode:completionHandler:@
checkHapticTrackAvailabilityForMediaMatchingCode_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
checkHapticTrackAvailabilityForMediaMatchingCode_completionHandlerSelector = mkSelector "checkHapticTrackAvailabilityForMediaMatchingCode:completionHandler:"

-- | @Selector@ for @addStatusObserver:@
addStatusObserverSelector :: Selector '[Ptr ()] RawId
addStatusObserverSelector = mkSelector "addStatusObserver:"

-- | @Selector@ for @removeStatusObserver:@
removeStatusObserverSelector :: Selector '[RawId] ()
removeStatusObserverSelector = mkSelector "removeStatusObserver:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MAMusicHapticsManager)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MAMusicHapticsManager)
newSelector = mkSelector "new"

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id MAMusicHapticsManager)
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @isActive@
isActiveSelector :: Selector '[] Bool
isActiveSelector = mkSelector "isActive"

