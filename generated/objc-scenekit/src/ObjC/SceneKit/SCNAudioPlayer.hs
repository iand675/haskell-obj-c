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
  , initSelector
  , initWithSourceSelector
  , initWithAVAudioNodeSelector
  , audioPlayerWithSourceSelector
  , audioPlayerWithAVAudioNodeSelector
  , willStartPlaybackSelector
  , setWillStartPlaybackSelector
  , didFinishPlaybackSelector
  , setDidFinishPlaybackSelector
  , audioNodeSelector
  , audioSourceSelector


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

import ObjC.SceneKit.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Id SCNAudioPlayer)
init_ scnAudioPlayer  =
  sendMsg scnAudioPlayer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithSource:
--
-- Init an audio player with a source. Most people should use audioPlayerWithSource as it permits to recycle previous players instead of creating new ones for each instance.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsSCNAudioPlayer scnAudioPlayer, IsSCNAudioSource source) => scnAudioPlayer -> source -> IO (Id SCNAudioPlayer)
initWithSource scnAudioPlayer  source =
withObjCPtr source $ \raw_source ->
    sendMsg scnAudioPlayer (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= ownedObject . castPtr

-- | initWithAVAudioNode:
--
-- Init an audio player with an AVAudioNode. Most people should use audioPlayerWithAVAudioNode as it permits to recycle previous players instead of creating new ones for each instance.
--
-- ObjC selector: @- initWithAVAudioNode:@
initWithAVAudioNode :: (IsSCNAudioPlayer scnAudioPlayer, IsAVAudioNode audioNode) => scnAudioPlayer -> audioNode -> IO (Id SCNAudioPlayer)
initWithAVAudioNode scnAudioPlayer  audioNode =
withObjCPtr audioNode $ \raw_audioNode ->
    sendMsg scnAudioPlayer (mkSelector "initWithAVAudioNode:") (retPtr retVoid) [argPtr (castPtr raw_audioNode :: Ptr ())] >>= ownedObject . castPtr

-- | audioPlayerWithSource:
--
-- Create an audio player with a source.
--
-- ObjC selector: @+ audioPlayerWithSource:@
audioPlayerWithSource :: IsSCNAudioSource source => source -> IO (Id SCNAudioPlayer)
audioPlayerWithSource source =
  do
    cls' <- getRequiredClass "SCNAudioPlayer"
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "audioPlayerWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | audioPlayerWithAVAudioNode:
--
-- Create an audio player with a custom AVAudioNode instance.
--
-- ObjC selector: @+ audioPlayerWithAVAudioNode:@
audioPlayerWithAVAudioNode :: IsAVAudioNode audioNode => audioNode -> IO (Id SCNAudioPlayer)
audioPlayerWithAVAudioNode audioNode =
  do
    cls' <- getRequiredClass "SCNAudioPlayer"
    withObjCPtr audioNode $ \raw_audioNode ->
      sendClassMsg cls' (mkSelector "audioPlayerWithAVAudioNode:") (retPtr retVoid) [argPtr (castPtr raw_audioNode :: Ptr ())] >>= retainedObject . castPtr

-- | playbackStarted
--
-- This block is called when the playback starts in case a valid audio source is present.
--
-- ObjC selector: @- willStartPlayback@
willStartPlayback :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Ptr ())
willStartPlayback scnAudioPlayer  =
  fmap castPtr $ sendMsg scnAudioPlayer (mkSelector "willStartPlayback") (retPtr retVoid) []

-- | playbackStarted
--
-- This block is called when the playback starts in case a valid audio source is present.
--
-- ObjC selector: @- setWillStartPlayback:@
setWillStartPlayback :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> Ptr () -> IO ()
setWillStartPlayback scnAudioPlayer  value =
  sendMsg scnAudioPlayer (mkSelector "setWillStartPlayback:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | playbackFinished
--
-- This block is called when the playback stops in case a valid audio source is present.
--
-- ObjC selector: @- didFinishPlayback@
didFinishPlayback :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Ptr ())
didFinishPlayback scnAudioPlayer  =
  fmap castPtr $ sendMsg scnAudioPlayer (mkSelector "didFinishPlayback") (retPtr retVoid) []

-- | playbackFinished
--
-- This block is called when the playback stops in case a valid audio source is present.
--
-- ObjC selector: @- setDidFinishPlayback:@
setDidFinishPlayback :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> Ptr () -> IO ()
setDidFinishPlayback scnAudioPlayer  value =
  sendMsg scnAudioPlayer (mkSelector "setDidFinishPlayback:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | audioNode
--
-- The audioNode. If this player was not initialised with a custom AVAudioNode this contains the internal audio player node used by scene kit internally.
--
-- ObjC selector: @- audioNode@
audioNode :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Id AVAudioNode)
audioNode scnAudioPlayer  =
  sendMsg scnAudioPlayer (mkSelector "audioNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | audioSource
--
-- The audioSource if there is one.
--
-- ObjC selector: @- audioSource@
audioSource :: IsSCNAudioPlayer scnAudioPlayer => scnAudioPlayer -> IO (Id SCNAudioSource)
audioSource scnAudioPlayer  =
  sendMsg scnAudioPlayer (mkSelector "audioSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithAVAudioNode:@
initWithAVAudioNodeSelector :: Selector
initWithAVAudioNodeSelector = mkSelector "initWithAVAudioNode:"

-- | @Selector@ for @audioPlayerWithSource:@
audioPlayerWithSourceSelector :: Selector
audioPlayerWithSourceSelector = mkSelector "audioPlayerWithSource:"

-- | @Selector@ for @audioPlayerWithAVAudioNode:@
audioPlayerWithAVAudioNodeSelector :: Selector
audioPlayerWithAVAudioNodeSelector = mkSelector "audioPlayerWithAVAudioNode:"

-- | @Selector@ for @willStartPlayback@
willStartPlaybackSelector :: Selector
willStartPlaybackSelector = mkSelector "willStartPlayback"

-- | @Selector@ for @setWillStartPlayback:@
setWillStartPlaybackSelector :: Selector
setWillStartPlaybackSelector = mkSelector "setWillStartPlayback:"

-- | @Selector@ for @didFinishPlayback@
didFinishPlaybackSelector :: Selector
didFinishPlaybackSelector = mkSelector "didFinishPlayback"

-- | @Selector@ for @setDidFinishPlayback:@
setDidFinishPlaybackSelector :: Selector
setDidFinishPlaybackSelector = mkSelector "setDidFinishPlayback:"

-- | @Selector@ for @audioNode@
audioNodeSelector :: Selector
audioNodeSelector = mkSelector "audioNode"

-- | @Selector@ for @audioSource@
audioSourceSelector :: Selector
audioSourceSelector = mkSelector "audioSource"

