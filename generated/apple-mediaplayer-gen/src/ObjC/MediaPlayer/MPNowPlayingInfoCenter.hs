{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector
  , nowPlayingInfoSelector
  , playbackStateSelector
  , setNowPlayingInfoSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' defaultCenterSelector

-- | @+ new@
new :: IO (Id MPNowPlayingInfoCenter)
new  =
  do
    cls' <- getRequiredClass "MPNowPlayingInfoCenter"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter => mpNowPlayingInfoCenter -> IO (Id MPNowPlayingInfoCenter)
init_ mpNowPlayingInfoCenter =
  sendOwnedMessage mpNowPlayingInfoCenter initSelector

-- | The current now playing info for the center. Setting the info to nil will clear it.
--
-- ObjC selector: @- nowPlayingInfo@
nowPlayingInfo :: IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter => mpNowPlayingInfoCenter -> IO (Id NSDictionary)
nowPlayingInfo mpNowPlayingInfoCenter =
  sendMessage mpNowPlayingInfoCenter nowPlayingInfoSelector

-- | The current now playing info for the center. Setting the info to nil will clear it.
--
-- ObjC selector: @- setNowPlayingInfo:@
setNowPlayingInfo :: (IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter, IsNSDictionary value) => mpNowPlayingInfoCenter -> value -> IO ()
setNowPlayingInfo mpNowPlayingInfoCenter value =
  sendMessage mpNowPlayingInfoCenter setNowPlayingInfoSelector (toNSDictionary value)

-- | The current playback state of the app. This only applies on macOS, where playback state cannot be determined by the application's audio session. This property must be set every time the app begins or halts playback, otherwise remote control functionality may not work as expected.
--
-- ObjC selector: @- playbackState@
playbackState :: IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter => mpNowPlayingInfoCenter -> IO MPNowPlayingPlaybackState
playbackState mpNowPlayingInfoCenter =
  sendMessage mpNowPlayingInfoCenter playbackStateSelector

-- | The current playback state of the app. This only applies on macOS, where playback state cannot be determined by the application's audio session. This property must be set every time the app begins or halts playback, otherwise remote control functionality may not work as expected.
--
-- ObjC selector: @- setPlaybackState:@
setPlaybackState :: IsMPNowPlayingInfoCenter mpNowPlayingInfoCenter => mpNowPlayingInfoCenter -> MPNowPlayingPlaybackState -> IO ()
setPlaybackState mpNowPlayingInfoCenter value =
  sendMessage mpNowPlayingInfoCenter setPlaybackStateSelector value

-- | Keys related to animated artwork that are supported by the current platform.
--
-- If you specify an instance of animated artwork (an @MPMediaItemAnimatedArtwork@) to @nowPlayingInfo@ using any key not in this collection it will be ignored.
--
-- ObjC selector: @+ supportedAnimatedArtworkKeys@
supportedAnimatedArtworkKeys :: IO (Id NSArray)
supportedAnimatedArtworkKeys  =
  do
    cls' <- getRequiredClass "MPNowPlayingInfoCenter"
    sendClassMessage cls' supportedAnimatedArtworkKeysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultCenter@
defaultCenterSelector :: Selector '[] (Id MPNowPlayingInfoCenter)
defaultCenterSelector = mkSelector "defaultCenter"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPNowPlayingInfoCenter)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPNowPlayingInfoCenter)
initSelector = mkSelector "init"

-- | @Selector@ for @nowPlayingInfo@
nowPlayingInfoSelector :: Selector '[] (Id NSDictionary)
nowPlayingInfoSelector = mkSelector "nowPlayingInfo"

-- | @Selector@ for @setNowPlayingInfo:@
setNowPlayingInfoSelector :: Selector '[Id NSDictionary] ()
setNowPlayingInfoSelector = mkSelector "setNowPlayingInfo:"

-- | @Selector@ for @playbackState@
playbackStateSelector :: Selector '[] MPNowPlayingPlaybackState
playbackStateSelector = mkSelector "playbackState"

-- | @Selector@ for @setPlaybackState:@
setPlaybackStateSelector :: Selector '[MPNowPlayingPlaybackState] ()
setPlaybackStateSelector = mkSelector "setPlaybackState:"

-- | @Selector@ for @supportedAnimatedArtworkKeys@
supportedAnimatedArtworkKeysSelector :: Selector '[] (Id NSArray)
supportedAnimatedArtworkKeysSelector = mkSelector "supportedAnimatedArtworkKeys"

