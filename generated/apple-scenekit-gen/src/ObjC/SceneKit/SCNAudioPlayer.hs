{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCNAudioPlayer@.
module ObjC.SceneKit.SCNAudioPlayer
  ( SCNAudioPlayer
  , IsSCNAudioPlayer(..)
  , init_
  , initWithSource
  , initWithAVAudioNode
  , audioPlayerWithSource
  , audioPlayerWithAVAudioNode
  , willStartPlayback
  , setWillStartPlayback
  , didFinishPlayback
  , setDidFinishPlayback
  , audioNode
  , audioSource
  , audioNodeSelector
  , audioPlayerWithAVAudioNodeSelector
  , audioPlayerWithSourceSelector
  , audioSourceSelector
  , didFinishPlaybackSelector
  , initSelector
  , initWithAVAudioNodeSelector
  , initWithSourceSelector
  , setDidFinishPlaybackSelector
  , setWillStartPlaybackSelector
  , willStartPlaybackSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Id SCNAudioPlayer)
init_ scnAudioPlayer =
  sendOwnedMessage scnAudioPlayer initSelector

-- | initWithSource:
--
-- Init an audio player with a source. Most people should use audioPlayerWithSource as it permits to recycle previous players instead of creating new ones for each instance.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsSCNAudioPlayer scnAudioPlayer, IsSCNAudioSource source) => scnAudioPlayer -> source -> IO (Id SCNAudioPlayer)
initWithSource scnAudioPlayer source =
  sendOwnedMessage scnAudioPlayer initWithSourceSelector (toSCNAudioSource source)

-- | initWithAVAudioNode:
--
-- Init an audio player with an AVAudioNode. Most people should use audioPlayerWithAVAudioNode as it permits to recycle previous players instead of creating new ones for each instance.
--
-- ObjC selector: @- initWithAVAudioNode:@
initWithAVAudioNode :: (IsSCNAudioPlayer scnAudioPlayer, IsAVAudioNode audioNode) => scnAudioPlayer -> audioNode -> IO (Id SCNAudioPlayer)
initWithAVAudioNode scnAudioPlayer audioNode =
  sendOwnedMessage scnAudioPlayer initWithAVAudioNodeSelector (toAVAudioNode audioNode)

-- | audioPlayerWithSource:
--
-- Create an audio player with a source.
--
-- ObjC selector: @+ audioPlayerWithSource:@
audioPlayerWithSource :: IsSCNAudioSource source => source -> IO (Id SCNAudioPlayer)
audioPlayerWithSource source =
  do
    cls' <- getRequiredClass "SCNAudioPlayer"
    sendClassMessage cls' audioPlayerWithSourceSelector (toSCNAudioSource source)

-- | audioPlayerWithAVAudioNode:
--
-- Create an audio player with a custom AVAudioNode instance.
--
-- ObjC selector: @+ audioPlayerWithAVAudioNode:@
audioPlayerWithAVAudioNode :: IsAVAudioNode audioNode => audioNode -> IO (Id SCNAudioPlayer)
audioPlayerWithAVAudioNode audioNode =
  do
    cls' <- getRequiredClass "SCNAudioPlayer"
    sendClassMessage cls' audioPlayerWithAVAudioNodeSelector (toAVAudioNode audioNode)

-- | playbackStarted
--
-- This block is called when the playback starts in case a valid audio source is present.
--
-- ObjC selector: @- willStartPlayback@
willStartPlayback :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Ptr ())
willStartPlayback scnAudioPlayer =
  sendMessage scnAudioPlayer willStartPlaybackSelector

-- | playbackStarted
--
-- This block is called when the playback starts in case a valid audio source is present.
--
-- ObjC selector: @- setWillStartPlayback:@
setWillStartPlayback :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> Ptr () -> IO ()
setWillStartPlayback scnAudioPlayer value =
  sendMessage scnAudioPlayer setWillStartPlaybackSelector value

-- | playbackFinished
--
-- This block is called when the playback stops in case a valid audio source is present.
--
-- ObjC selector: @- didFinishPlayback@
didFinishPlayback :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Ptr ())
didFinishPlayback scnAudioPlayer =
  sendMessage scnAudioPlayer didFinishPlaybackSelector

-- | playbackFinished
--
-- This block is called when the playback stops in case a valid audio source is present.
--
-- ObjC selector: @- setDidFinishPlayback:@
setDidFinishPlayback :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> Ptr () -> IO ()
setDidFinishPlayback scnAudioPlayer value =
  sendMessage scnAudioPlayer setDidFinishPlaybackSelector value

-- | audioNode
--
-- The audioNode. If this player was not initialised with a custom AVAudioNode this contains the internal audio player node used by scene kit internally.
--
-- ObjC selector: @- audioNode@
audioNode :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Id AVAudioNode)
audioNode scnAudioPlayer =
  sendMessage scnAudioPlayer audioNodeSelector

-- | audioSource
--
-- The audioSource if there is one.
--
-- ObjC selector: @- audioSource@
audioSource :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Id SCNAudioSource)
audioSource scnAudioPlayer =
  sendMessage scnAudioPlayer audioSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCNAudioPlayer)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id SCNAudioSource] (Id SCNAudioPlayer)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithAVAudioNode:@
initWithAVAudioNodeSelector :: Selector '[Id AVAudioNode] (Id SCNAudioPlayer)
initWithAVAudioNodeSelector = mkSelector "initWithAVAudioNode:"

-- | @Selector@ for @audioPlayerWithSource:@
audioPlayerWithSourceSelector :: Selector '[Id SCNAudioSource] (Id SCNAudioPlayer)
audioPlayerWithSourceSelector = mkSelector "audioPlayerWithSource:"

-- | @Selector@ for @audioPlayerWithAVAudioNode:@
audioPlayerWithAVAudioNodeSelector :: Selector '[Id AVAudioNode] (Id SCNAudioPlayer)
audioPlayerWithAVAudioNodeSelector = mkSelector "audioPlayerWithAVAudioNode:"

-- | @Selector@ for @willStartPlayback@
willStartPlaybackSelector :: Selector '[] (Ptr ())
willStartPlaybackSelector = mkSelector "willStartPlayback"

-- | @Selector@ for @setWillStartPlayback:@
setWillStartPlaybackSelector :: Selector '[Ptr ()] ()
setWillStartPlaybackSelector = mkSelector "setWillStartPlayback:"

-- | @Selector@ for @didFinishPlayback@
didFinishPlaybackSelector :: Selector '[] (Ptr ())
didFinishPlaybackSelector = mkSelector "didFinishPlayback"

-- | @Selector@ for @setDidFinishPlayback:@
setDidFinishPlaybackSelector :: Selector '[Ptr ()] ()
setDidFinishPlaybackSelector = mkSelector "setDidFinishPlayback:"

-- | @Selector@ for @audioNode@
audioNodeSelector :: Selector '[] (Id AVAudioNode)
audioNodeSelector = mkSelector "audioNode"

-- | @Selector@ for @audioSource@
audioSourceSelector :: Selector '[] (Id SCNAudioSource)
audioSourceSelector = mkSelector "audioSource"

