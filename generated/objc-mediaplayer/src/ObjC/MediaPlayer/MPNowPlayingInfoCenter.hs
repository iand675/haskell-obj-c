{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPNowPlayingInfoCenter@.
module ObjC.MediaPlayer.MPNowPlayingInfoCenter
  ( MPNowPlayingInfoCenter
  , IsMPNowPlayingInfoCenter(..)
  , defaultCenter
  , new
  , init_
  , nowPlayingInfo
  , setNowPlayingInfo
  , playbackState
  , setPlaybackState
  , supportedAnimatedArtworkKeys
  , defaultCenterSelector
  , newSelector
  , initSelector
  , nowPlayingInfoSelector
  , setNowPlayingInfoSelector
  , playbackStateSelector
  , setPlaybackStateSelector
  , supportedAnimatedArtworkKeysSelector

  -- * Enum types
  , MPNowPlayingPlaybackState(MPNowPlayingPlaybackState)
  , pattern MPNowPlayingPlaybackStateUnknown
  , pattern MPNowPlayingPlaybackStatePlaying
  , pattern MPNowPlayingPlaybackStatePaused
  , pattern MPNowPlayingPlaybackStateStopped
  , pattern MPNowPlayingPlaybackStateInterrupted

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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns the default now playing info center. The default center holds now playing info about the current application.
--
-- ObjC selector: @+ defaultCenter@
defaultCenter :: IO (Id MPNowPlayingInfoCenter)
defaultCenter  =
  do
    cls' <- getRequiredClass "MPNowPlayingInfoCenter"
    sendClassMsg cls' (mkSelector "defaultCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id MPNowPlayingInfoCenter)
new  =
  do
    cls' <- getRequiredClass "MPNowPlayingInfoCenter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter => mpNowPlayingInfoCenter -> IO (Id MPNowPlayingInfoCenter)
init_ mpNowPlayingInfoCenter  =
  sendMsg mpNowPlayingInfoCenter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The current now playing info for the center. Setting the info to nil will clear it.
--
-- ObjC selector: @- nowPlayingInfo@
nowPlayingInfo :: IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter => mpNowPlayingInfoCenter -> IO (Id NSDictionary)
nowPlayingInfo mpNowPlayingInfoCenter  =
  sendMsg mpNowPlayingInfoCenter (mkSelector "nowPlayingInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The current now playing info for the center. Setting the info to nil will clear it.
--
-- ObjC selector: @- setNowPlayingInfo:@
setNowPlayingInfo :: (IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter, IsNSDictionary value) => mpNowPlayingInfoCenter -> value -> IO ()
setNowPlayingInfo mpNowPlayingInfoCenter  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpNowPlayingInfoCenter (mkSelector "setNowPlayingInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The current playback state of the app. This only applies on macOS, where playback state cannot be determined by the application's audio session. This property must be set every time the app begins or halts playback, otherwise remote control functionality may not work as expected.
--
-- ObjC selector: @- playbackState@
playbackState :: IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter => mpNowPlayingInfoCenter -> IO MPNowPlayingPlaybackState
playbackState mpNowPlayingInfoCenter  =
  fmap (coerce :: CULong -> MPNowPlayingPlaybackState) $ sendMsg mpNowPlayingInfoCenter (mkSelector "playbackState") retCULong []

-- | The current playback state of the app. This only applies on macOS, where playback state cannot be determined by the application's audio session. This property must be set every time the app begins or halts playback, otherwise remote control functionality may not work as expected.
--
-- ObjC selector: @- setPlaybackState:@
setPlaybackState :: IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter => mpNowPlayingInfoCenter -> MPNowPlayingPlaybackState -> IO ()
setPlaybackState mpNowPlayingInfoCenter  value =
  sendMsg mpNowPlayingInfoCenter (mkSelector "setPlaybackState:") retVoid [argCULong (coerce value)]

-- | Keys related to animated artwork that are supported by the current platform.
--
-- If you specify an instance of animated artwork (an @MPMediaItemAnimatedArtwork@) to @nowPlayingInfo@ using any key not in this collection it will be ignored.
--
-- ObjC selector: @+ supportedAnimatedArtworkKeys@
supportedAnimatedArtworkKeys :: IO (Id NSArray)
supportedAnimatedArtworkKeys  =
  do
    cls' <- getRequiredClass "MPNowPlayingInfoCenter"
    sendClassMsg cls' (mkSelector "supportedAnimatedArtworkKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultCenter@
defaultCenterSelector :: Selector
defaultCenterSelector = mkSelector "defaultCenter"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @nowPlayingInfo@
nowPlayingInfoSelector :: Selector
nowPlayingInfoSelector = mkSelector "nowPlayingInfo"

-- | @Selector@ for @setNowPlayingInfo:@
setNowPlayingInfoSelector :: Selector
setNowPlayingInfoSelector = mkSelector "setNowPlayingInfo:"

-- | @Selector@ for @playbackState@
playbackStateSelector :: Selector
playbackStateSelector = mkSelector "playbackState"

-- | @Selector@ for @setPlaybackState:@
setPlaybackStateSelector :: Selector
setPlaybackStateSelector = mkSelector "setPlaybackState:"

-- | @Selector@ for @supportedAnimatedArtworkKeys@
supportedAnimatedArtworkKeysSelector :: Selector
supportedAnimatedArtworkKeysSelector = mkSelector "supportedAnimatedArtworkKeys"

