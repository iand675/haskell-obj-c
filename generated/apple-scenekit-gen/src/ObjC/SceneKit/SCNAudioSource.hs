{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNAudioSource
--
-- The SCNAudioSource class represents an audio source that can be added to a SCNNode.
--
-- Generated bindings for @SCNAudioSource@.
module ObjC.SceneKit.SCNAudioSource
  ( SCNAudioSource
  , IsSCNAudioSource(..)
  , initWithFileNamed
  , initWithURL
  , audioSourceNamed
  , load
  , positional
  , setPositional
  , volume
  , setVolume
  , rate
  , setRate
  , reverbBlend
  , setReverbBlend
  , loops
  , setLoops
  , shouldStream
  , setShouldStream
  , audioSourceNamedSelector
  , initWithFileNamedSelector
  , initWithURLSelector
  , loadSelector
  , loopsSelector
  , positionalSelector
  , rateSelector
  , reverbBlendSelector
  , setLoopsSelector
  , setPositionalSelector
  , setRateSelector
  , setReverbBlendSelector
  , setShouldStreamSelector
  , setVolumeSelector
  , shouldStreamSelector
  , volumeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithFileNamed:
--
-- Convenience initializer that creates an AVAudioNode from the named audio asset in the main bundle.
--
-- ObjC selector: @- initWithFileNamed:@
initWithFileNamed :: (IsSCNAudioSource scnAudioSource, IsNSString name) => scnAudioSource -> name -> IO (Id SCNAudioSource)
initWithFileNamed scnAudioSource name =
  sendOwnedMessage scnAudioSource initWithFileNamedSelector (toNSString name)

-- | initWithURL:
--
-- Convenience initializer that creates an AVAudioNode from the URL that contain a audio asset.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSCNAudioSource scnAudioSource, IsNSURL url) => scnAudioSource -> url -> IO (Id SCNAudioSource)
initWithURL scnAudioSource url =
  sendOwnedMessage scnAudioSource initWithURLSelector (toNSURL url)

-- | audioSourceNamed:
--
-- Convenience class initializer that caches audioSources.
--
-- ObjC selector: @+ audioSourceNamed:@
audioSourceNamed :: IsNSString fileName => fileName -> IO (Id SCNAudioSource)
audioSourceNamed fileName =
  do
    cls' <- getRequiredClass "SCNAudioSource"
    sendClassMessage cls' audioSourceNamedSelector (toNSString fileName)

-- | load
--
-- Load and uncompress the audio source in memory. This method has no effect if "shouldStream" is set to YES or if the audio source is already loaded.
--
-- This method let you preload your audio sources. If an audio source is not preloaded, it will be loaded anyway when playing it.
--
-- ObjC selector: @- load@
load :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO ()
load scnAudioSource =
  sendMessage scnAudioSource loadSelector

-- | positional
--
-- Marks the audio source as positional so that the audio mix considers relative position and velocity with regards to the SCNSceneRenderer's current listener node. Defaults to YES.
--
-- shouldStream must be set to false in order to get positional audio (see shouldStream).
--
-- See: SCNSceneRenderer audioListener.
--
-- ObjC selector: @- positional@
positional :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO Bool
positional scnAudioSource =
  sendMessage scnAudioSource positionalSelector

-- | positional
--
-- Marks the audio source as positional so that the audio mix considers relative position and velocity with regards to the SCNSceneRenderer's current listener node. Defaults to YES.
--
-- shouldStream must be set to false in order to get positional audio (see shouldStream).
--
-- See: SCNSceneRenderer audioListener.
--
-- ObjC selector: @- setPositional:@
setPositional :: IsSCNAudioSource scnAudioSource => scnAudioSource -> Bool -> IO ()
setPositional scnAudioSource value =
  sendMessage scnAudioSource setPositionalSelector value

-- | volume
--
-- The default volume for this audio buffer. Default is 1.0 (full volume).
--
-- ObjC selector: @- volume@
volume :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO CFloat
volume scnAudioSource =
  sendMessage scnAudioSource volumeSelector

-- | volume
--
-- The default volume for this audio buffer. Default is 1.0 (full volume).
--
-- ObjC selector: @- setVolume:@
setVolume :: IsSCNAudioSource scnAudioSource => scnAudioSource -> CFloat -> IO ()
setVolume scnAudioSource value =
  sendMessage scnAudioSource setVolumeSelector value

-- | rate
--
-- The default rate for this audio buffer. Default is 1.0 (original rate of the audio source).
--
-- ObjC selector: @- rate@
rate :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO CFloat
rate scnAudioSource =
  sendMessage scnAudioSource rateSelector

-- | rate
--
-- The default rate for this audio buffer. Default is 1.0 (original rate of the audio source).
--
-- ObjC selector: @- setRate:@
setRate :: IsSCNAudioSource scnAudioSource => scnAudioSource -> CFloat -> IO ()
setRate scnAudioSource value =
  sendMessage scnAudioSource setRateSelector value

-- | reverbBlend
--
-- The default reverbBlend for this audio buffer. Default is 0.0 (no sound is sent to the reverb).
--
-- ObjC selector: @- reverbBlend@
reverbBlend :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO CFloat
reverbBlend scnAudioSource =
  sendMessage scnAudioSource reverbBlendSelector

-- | reverbBlend
--
-- The default reverbBlend for this audio buffer. Default is 0.0 (no sound is sent to the reverb).
--
-- ObjC selector: @- setReverbBlend:@
setReverbBlend :: IsSCNAudioSource scnAudioSource => scnAudioSource -> CFloat -> IO ()
setReverbBlend scnAudioSource value =
  sendMessage scnAudioSource setReverbBlendSelector value

-- | loops
--
-- Specifies whether the audio source should loop or not. Defaults to NO.
--
-- ObjC selector: @- loops@
loops :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO Bool
loops scnAudioSource =
  sendMessage scnAudioSource loopsSelector

-- | loops
--
-- Specifies whether the audio source should loop or not. Defaults to NO.
--
-- ObjC selector: @- setLoops:@
setLoops :: IsSCNAudioSource scnAudioSource => scnAudioSource -> Bool -> IO ()
setLoops scnAudioSource value =
  sendMessage scnAudioSource setLoopsSelector value

-- | shouldStream
--
-- Specifies whether the audio source should be streamed or not. Defaults to NO.
--
-- ObjC selector: @- shouldStream@
shouldStream :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO Bool
shouldStream scnAudioSource =
  sendMessage scnAudioSource shouldStreamSelector

-- | shouldStream
--
-- Specifies whether the audio source should be streamed or not. Defaults to NO.
--
-- ObjC selector: @- setShouldStream:@
setShouldStream :: IsSCNAudioSource scnAudioSource => scnAudioSource -> Bool -> IO ()
setShouldStream scnAudioSource value =
  sendMessage scnAudioSource setShouldStreamSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileNamed:@
initWithFileNamedSelector :: Selector '[Id NSString] (Id SCNAudioSource)
initWithFileNamedSelector = mkSelector "initWithFileNamed:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id SCNAudioSource)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @audioSourceNamed:@
audioSourceNamedSelector :: Selector '[Id NSString] (Id SCNAudioSource)
audioSourceNamedSelector = mkSelector "audioSourceNamed:"

-- | @Selector@ for @load@
loadSelector :: Selector '[] ()
loadSelector = mkSelector "load"

-- | @Selector@ for @positional@
positionalSelector :: Selector '[] Bool
positionalSelector = mkSelector "positional"

-- | @Selector@ for @setPositional:@
setPositionalSelector :: Selector '[Bool] ()
setPositionalSelector = mkSelector "setPositional:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @reverbBlend@
reverbBlendSelector :: Selector '[] CFloat
reverbBlendSelector = mkSelector "reverbBlend"

-- | @Selector@ for @setReverbBlend:@
setReverbBlendSelector :: Selector '[CFloat] ()
setReverbBlendSelector = mkSelector "setReverbBlend:"

-- | @Selector@ for @loops@
loopsSelector :: Selector '[] Bool
loopsSelector = mkSelector "loops"

-- | @Selector@ for @setLoops:@
setLoopsSelector :: Selector '[Bool] ()
setLoopsSelector = mkSelector "setLoops:"

-- | @Selector@ for @shouldStream@
shouldStreamSelector :: Selector '[] Bool
shouldStreamSelector = mkSelector "shouldStream"

-- | @Selector@ for @setShouldStream:@
setShouldStreamSelector :: Selector '[Bool] ()
setShouldStreamSelector = mkSelector "setShouldStream:"

