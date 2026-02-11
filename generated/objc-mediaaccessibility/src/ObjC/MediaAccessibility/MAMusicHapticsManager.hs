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
  , checkHapticTrackAvailabilityForMediaMatchingCode_completionHandlerSelector
  , addStatusObserverSelector
  , removeStatusObserverSelector
  , initSelector
  , newSelector
  , sharedManagerSelector
  , isActiveSelector


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

import ObjC.MediaAccessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | In an asynchronous completion handler, returns whether a specific media track with the supplied ISRC has an available haptic track.
--
-- ObjC selector: @- checkHapticTrackAvailabilityForMediaMatchingCode:completionHandler:@
checkHapticTrackAvailabilityForMediaMatchingCode_completionHandler :: (IsMAMusicHapticsManager maMusicHapticsManager, IsNSString internationalStandardRecordingCode) => maMusicHapticsManager -> internationalStandardRecordingCode -> Ptr () -> IO ()
checkHapticTrackAvailabilityForMediaMatchingCode_completionHandler maMusicHapticsManager  internationalStandardRecordingCode completionHandler =
withObjCPtr internationalStandardRecordingCode $ \raw_internationalStandardRecordingCode ->
    sendMsg maMusicHapticsManager (mkSelector "checkHapticTrackAvailabilityForMediaMatchingCode:completionHandler:") retVoid [argPtr (castPtr raw_internationalStandardRecordingCode :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Determine the status of haptic playback for the now playing track asynchronously. This will only be delivered for the app that is the active Now Playing app.
--
-- ObjC selector: @- addStatusObserver:@
addStatusObserver :: IsMAMusicHapticsManager maMusicHapticsManager => maMusicHapticsManager -> Ptr () -> IO RawId
addStatusObserver maMusicHapticsManager  statusHandler =
  fmap (RawId . castPtr) $ sendMsg maMusicHapticsManager (mkSelector "addStatusObserver:") (retPtr retVoid) [argPtr (castPtr statusHandler :: Ptr ())]

-- | @- removeStatusObserver:@
removeStatusObserver :: IsMAMusicHapticsManager maMusicHapticsManager => maMusicHapticsManager -> RawId -> IO ()
removeStatusObserver maMusicHapticsManager  registrationToken =
  sendMsg maMusicHapticsManager (mkSelector "removeStatusObserver:") retVoid [argPtr (castPtr (unRawId registrationToken) :: Ptr ())]

-- | @- init@
init_ :: IsMAMusicHapticsManager maMusicHapticsManager => maMusicHapticsManager -> IO (Id MAMusicHapticsManager)
init_ maMusicHapticsManager  =
  sendMsg maMusicHapticsManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MAMusicHapticsManager)
new  =
  do
    cls' <- getRequiredClass "MAMusicHapticsManager"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ sharedManager@
sharedManager :: IO (Id MAMusicHapticsManager)
sharedManager  =
  do
    cls' <- getRequiredClass "MAMusicHapticsManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether the user setting to indicate Music Haptics are currently active.
--
-- Returns: A boolean result.
--
-- ObjC selector: @- isActive@
isActive :: IsMAMusicHapticsManager maMusicHapticsManager => maMusicHapticsManager -> IO Bool
isActive maMusicHapticsManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg maMusicHapticsManager (mkSelector "isActive") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkHapticTrackAvailabilityForMediaMatchingCode:completionHandler:@
checkHapticTrackAvailabilityForMediaMatchingCode_completionHandlerSelector :: Selector
checkHapticTrackAvailabilityForMediaMatchingCode_completionHandlerSelector = mkSelector "checkHapticTrackAvailabilityForMediaMatchingCode:completionHandler:"

-- | @Selector@ for @addStatusObserver:@
addStatusObserverSelector :: Selector
addStatusObserverSelector = mkSelector "addStatusObserver:"

-- | @Selector@ for @removeStatusObserver:@
removeStatusObserverSelector :: Selector
removeStatusObserverSelector = mkSelector "removeStatusObserver:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @isActive@
isActiveSelector :: Selector
isActiveSelector = mkSelector "isActive"

