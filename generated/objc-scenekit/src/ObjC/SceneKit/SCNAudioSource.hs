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
  , initWithFileNamedSelector
  , initWithURLSelector
  , audioSourceNamedSelector
  , loadSelector
  , positionalSelector
  , setPositionalSelector
  , volumeSelector
  , setVolumeSelector
  , rateSelector
  , setRateSelector
  , reverbBlendSelector
  , setReverbBlendSelector
  , loopsSelector
  , setLoopsSelector
  , shouldStreamSelector
  , setShouldStreamSelector


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
import ObjC.Foundation.Internal.Classes

-- | initWithFileNamed:
--
-- Convenience initializer that creates an AVAudioNode from the named audio asset in the main bundle.
--
-- ObjC selector: @- initWithFileNamed:@
initWithFileNamed :: (IsSCNAudioSource scnAudioSource, IsNSString name) => scnAudioSource -> name -> IO (Id SCNAudioSource)
initWithFileNamed scnAudioSource  name =
withObjCPtr name $ \raw_name ->
    sendMsg scnAudioSource (mkSelector "initWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | initWithURL:
--
-- Convenience initializer that creates an AVAudioNode from the URL that contain a audio asset.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSCNAudioSource scnAudioSource, IsNSURL url) => scnAudioSource -> url -> IO (Id SCNAudioSource)
initWithURL scnAudioSource  url =
withObjCPtr url $ \raw_url ->
    sendMsg scnAudioSource (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | audioSourceNamed:
--
-- Convenience class initializer that caches audioSources.
--
-- ObjC selector: @+ audioSourceNamed:@
audioSourceNamed :: IsNSString fileName => fileName -> IO (Id SCNAudioSource)
audioSourceNamed fileName =
  do
    cls' <- getRequiredClass "SCNAudioSource"
    withObjCPtr fileName $ \raw_fileName ->
      sendClassMsg cls' (mkSelector "audioSourceNamed:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ())] >>= retainedObject . castPtr

-- | load
--
-- Load and uncompress the audio source in memory. This method has no effect if "shouldStream" is set to YES or if the audio source is already loaded.
--
-- This method let you preload your audio sources. If an audio source is not preloaded, it will be loaded anyway when playing it.
--
-- ObjC selector: @- load@
load :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO ()
load scnAudioSource  =
  sendMsg scnAudioSource (mkSelector "load") retVoid []

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
positional scnAudioSource  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAudioSource (mkSelector "positional") retCULong []

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
setPositional scnAudioSource  value =
  sendMsg scnAudioSource (mkSelector "setPositional:") retVoid [argCULong (if value then 1 else 0)]

-- | volume
--
-- The default volume for this audio buffer. Default is 1.0 (full volume).
--
-- ObjC selector: @- volume@
volume :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO CFloat
volume scnAudioSource  =
  sendMsg scnAudioSource (mkSelector "volume") retCFloat []

-- | volume
--
-- The default volume for this audio buffer. Default is 1.0 (full volume).
--
-- ObjC selector: @- setVolume:@
setVolume :: IsSCNAudioSource scnAudioSource => scnAudioSource -> CFloat -> IO ()
setVolume scnAudioSource  value =
  sendMsg scnAudioSource (mkSelector "setVolume:") retVoid [argCFloat (fromIntegral value)]

-- | rate
--
-- The default rate for this audio buffer. Default is 1.0 (original rate of the audio source).
--
-- ObjC selector: @- rate@
rate :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO CFloat
rate scnAudioSource  =
  sendMsg scnAudioSource (mkSelector "rate") retCFloat []

-- | rate
--
-- The default rate for this audio buffer. Default is 1.0 (original rate of the audio source).
--
-- ObjC selector: @- setRate:@
setRate :: IsSCNAudioSource scnAudioSource => scnAudioSource -> CFloat -> IO ()
setRate scnAudioSource  value =
  sendMsg scnAudioSource (mkSelector "setRate:") retVoid [argCFloat (fromIntegral value)]

-- | reverbBlend
--
-- The default reverbBlend for this audio buffer. Default is 0.0 (no sound is sent to the reverb).
--
-- ObjC selector: @- reverbBlend@
reverbBlend :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO CFloat
reverbBlend scnAudioSource  =
  sendMsg scnAudioSource (mkSelector "reverbBlend") retCFloat []

-- | reverbBlend
--
-- The default reverbBlend for this audio buffer. Default is 0.0 (no sound is sent to the reverb).
--
-- ObjC selector: @- setReverbBlend:@
setReverbBlend :: IsSCNAudioSource scnAudioSource => scnAudioSource -> CFloat -> IO ()
setReverbBlend scnAudioSource  value =
  sendMsg scnAudioSource (mkSelector "setReverbBlend:") retVoid [argCFloat (fromIntegral value)]

-- | loops
--
-- Specifies whether the audio source should loop or not. Defaults to NO.
--
-- ObjC selector: @- loops@
loops :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO Bool
loops scnAudioSource  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAudioSource (mkSelector "loops") retCULong []

-- | loops
--
-- Specifies whether the audio source should loop or not. Defaults to NO.
--
-- ObjC selector: @- setLoops:@
setLoops :: IsSCNAudioSource scnAudioSource => scnAudioSource -> Bool -> IO ()
setLoops scnAudioSource  value =
  sendMsg scnAudioSource (mkSelector "setLoops:") retVoid [argCULong (if value then 1 else 0)]

-- | shouldStream
--
-- Specifies whether the audio source should be streamed or not. Defaults to NO.
--
-- ObjC selector: @- shouldStream@
shouldStream :: IsSCNAudioSource scnAudioSource => scnAudioSource -> IO Bool
shouldStream scnAudioSource  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnAudioSource (mkSelector "shouldStream") retCULong []

-- | shouldStream
--
-- Specifies whether the audio source should be streamed or not. Defaults to NO.
--
-- ObjC selector: @- setShouldStream:@
setShouldStream :: IsSCNAudioSource scnAudioSource => scnAudioSource -> Bool -> IO ()
setShouldStream scnAudioSource  value =
  sendMsg scnAudioSource (mkSelector "setShouldStream:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileNamed:@
initWithFileNamedSelector :: Selector
initWithFileNamedSelector = mkSelector "initWithFileNamed:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @audioSourceNamed:@
audioSourceNamedSelector :: Selector
audioSourceNamedSelector = mkSelector "audioSourceNamed:"

-- | @Selector@ for @load@
loadSelector :: Selector
loadSelector = mkSelector "load"

-- | @Selector@ for @positional@
positionalSelector :: Selector
positionalSelector = mkSelector "positional"

-- | @Selector@ for @setPositional:@
setPositionalSelector :: Selector
setPositionalSelector = mkSelector "setPositional:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @reverbBlend@
reverbBlendSelector :: Selector
reverbBlendSelector = mkSelector "reverbBlend"

-- | @Selector@ for @setReverbBlend:@
setReverbBlendSelector :: Selector
setReverbBlendSelector = mkSelector "setReverbBlend:"

-- | @Selector@ for @loops@
loopsSelector :: Selector
loopsSelector = mkSelector "loops"

-- | @Selector@ for @setLoops:@
setLoopsSelector :: Selector
setLoopsSelector = mkSelector "setLoops:"

-- | @Selector@ for @shouldStream@
shouldStreamSelector :: Selector
shouldStreamSelector = mkSelector "shouldStream"

-- | @Selector@ for @setShouldStream:@
setShouldStreamSelector :: Selector
setShouldStreamSelector = mkSelector "setShouldStream:"

