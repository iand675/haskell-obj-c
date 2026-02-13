{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterPlaybackPreferencesStruct@.
module ObjC.Matter.MTRContentLauncherClusterPlaybackPreferencesStruct
  ( MTRContentLauncherClusterPlaybackPreferencesStruct
  , IsMTRContentLauncherClusterPlaybackPreferencesStruct(..)
  , playbackPosition
  , setPlaybackPosition
  , textTrack
  , setTextTrack
  , audioTracks
  , setAudioTracks
  , audioTracksSelector
  , playbackPositionSelector
  , setAudioTracksSelector
  , setPlaybackPositionSelector
  , setTextTrackSelector
  , textTrackSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- playbackPosition@
playbackPosition :: IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct => mtrContentLauncherClusterPlaybackPreferencesStruct -> IO (Id NSNumber)
playbackPosition mtrContentLauncherClusterPlaybackPreferencesStruct =
  sendMessage mtrContentLauncherClusterPlaybackPreferencesStruct playbackPositionSelector

-- | @- setPlaybackPosition:@
setPlaybackPosition :: (IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct, IsNSNumber value) => mtrContentLauncherClusterPlaybackPreferencesStruct -> value -> IO ()
setPlaybackPosition mtrContentLauncherClusterPlaybackPreferencesStruct value =
  sendMessage mtrContentLauncherClusterPlaybackPreferencesStruct setPlaybackPositionSelector (toNSNumber value)

-- | @- textTrack@
textTrack :: IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct => mtrContentLauncherClusterPlaybackPreferencesStruct -> IO (Id MTRContentLauncherClusterTrackPreferenceStruct)
textTrack mtrContentLauncherClusterPlaybackPreferencesStruct =
  sendMessage mtrContentLauncherClusterPlaybackPreferencesStruct textTrackSelector

-- | @- setTextTrack:@
setTextTrack :: (IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct, IsMTRContentLauncherClusterTrackPreferenceStruct value) => mtrContentLauncherClusterPlaybackPreferencesStruct -> value -> IO ()
setTextTrack mtrContentLauncherClusterPlaybackPreferencesStruct value =
  sendMessage mtrContentLauncherClusterPlaybackPreferencesStruct setTextTrackSelector (toMTRContentLauncherClusterTrackPreferenceStruct value)

-- | @- audioTracks@
audioTracks :: IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct => mtrContentLauncherClusterPlaybackPreferencesStruct -> IO (Id NSArray)
audioTracks mtrContentLauncherClusterPlaybackPreferencesStruct =
  sendMessage mtrContentLauncherClusterPlaybackPreferencesStruct audioTracksSelector

-- | @- setAudioTracks:@
setAudioTracks :: (IsMTRContentLauncherClusterPlaybackPreferencesStruct mtrContentLauncherClusterPlaybackPreferencesStruct, IsNSArray value) => mtrContentLauncherClusterPlaybackPreferencesStruct -> value -> IO ()
setAudioTracks mtrContentLauncherClusterPlaybackPreferencesStruct value =
  sendMessage mtrContentLauncherClusterPlaybackPreferencesStruct setAudioTracksSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playbackPosition@
playbackPositionSelector :: Selector '[] (Id NSNumber)
playbackPositionSelector = mkSelector "playbackPosition"

-- | @Selector@ for @setPlaybackPosition:@
setPlaybackPositionSelector :: Selector '[Id NSNumber] ()
setPlaybackPositionSelector = mkSelector "setPlaybackPosition:"

-- | @Selector@ for @textTrack@
textTrackSelector :: Selector '[] (Id MTRContentLauncherClusterTrackPreferenceStruct)
textTrackSelector = mkSelector "textTrack"

-- | @Selector@ for @setTextTrack:@
setTextTrackSelector :: Selector '[Id MTRContentLauncherClusterTrackPreferenceStruct] ()
setTextTrackSelector = mkSelector "setTextTrack:"

-- | @Selector@ for @audioTracks@
audioTracksSelector :: Selector '[] (Id NSArray)
audioTracksSelector = mkSelector "audioTracks"

-- | @Selector@ for @setAudioTracks:@
setAudioTracksSelector :: Selector '[Id NSArray] ()
setAudioTracksSelector = mkSelector "setAudioTracks:"

