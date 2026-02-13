{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnit
--
-- An AVAudioNode implemented by an audio unit.
--
-- An AVAudioUnit is an AVAudioNode implemented by an audio unit. Depending on the type of        the audio unit, audio is processed either in real-time or non real-time.
--
-- Generated bindings for @AVAudioUnit@.
module ObjC.AVFAudio.AVAudioUnit
  ( AVAudioUnit
  , IsAVAudioUnit(..)
  , instantiateWithComponentDescription_options_completionHandler
  , loadAudioUnitPresetAtURL_error
  , audioComponentDescription
  , audioUnit
  , auAudioUnit
  , name
  , manufacturerName
  , version
  , auAudioUnitSelector
  , audioComponentDescriptionSelector
  , audioUnitSelector
  , instantiateWithComponentDescription_options_completionHandlerSelector
  , loadAudioUnitPresetAtURL_errorSelector
  , manufacturerNameSelector
  , nameSelector
  , versionSelector

  -- * Enum types
  , AudioComponentInstantiationOptions(AudioComponentInstantiationOptions)
  , pattern KAudioComponentInstantiation_LoadOutOfProcess
  , pattern KAudioComponentInstantiation_LoadInProcess
  , pattern KAudioComponentInstantiation_LoadedRemotely

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AudioToolbox.Internal.Structs
import ObjC.AudioToolbox.Internal.Enums
import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | instantiateWithComponentDescription:options:completionHandler:
--
-- Asynchronously create an instance of an audio unit component, wrapped in an AVAudioUnit.
--
-- @audioComponentDescription@ — The component to instantiate.
--
-- @options@ — Instantiation options.
--
-- @completionHandler@ — Called in an arbitrary thread/queue context when instantiation is complete. The client		should retain the provided AVAudioUnit.
--
-- Components whose flags include kAudioComponentFlag_RequiresAsyncInstantiation must be 		instantiated asynchronously, via this method if they are to be used with AVAudioEngine.		See the discussion of this flag in AudioToolbox/AudioComponent.h.
--
-- The returned AVAudioUnit instance normally will be of a subclass (AVAudioUnitEffect,		AVAudioUnitGenerator, AVAudioUnitMIDIInstrument, or AVAudioUnitTimeEffect), selected		according to the component's type.
--
-- ObjC selector: @+ instantiateWithComponentDescription:options:completionHandler:@
instantiateWithComponentDescription_options_completionHandler :: AudioComponentDescription -> AudioComponentInstantiationOptions -> Ptr () -> IO ()
instantiateWithComponentDescription_options_completionHandler audioComponentDescription options completionHandler =
  do
    cls' <- getRequiredClass "AVAudioUnit"
    sendClassMessage cls' instantiateWithComponentDescription_options_completionHandlerSelector audioComponentDescription options completionHandler

-- | loadAudioUnitPresetAtURL:error:
--
-- Load an audio unit preset.
--
-- @url@ — NSURL of the .aupreset file.
--
-- @outError@ — A pointer to a NSError object
--
-- If the .aupreset file cannot be successfully loaded, an error is returned.
--
-- ObjC selector: @- loadAudioUnitPresetAtURL:error:@
loadAudioUnitPresetAtURL_error :: (IsAVAudioUnit avAudioUnit, IsNSURL url, IsNSError outError) => avAudioUnit -> url -> outError -> IO Bool
loadAudioUnitPresetAtURL_error avAudioUnit url outError =
  sendMessage avAudioUnit loadAudioUnitPresetAtURL_errorSelector (toNSURL url) (toNSError outError)

-- | audioComponentDescription
--
-- AudioComponentDescription of the underlying audio unit.
--
-- ObjC selector: @- audioComponentDescription@
audioComponentDescription :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO AudioComponentDescription
audioComponentDescription avAudioUnit =
  sendMessage avAudioUnit audioComponentDescriptionSelector

-- | audioUnit
--
-- Reference to the underlying audio unit.
--
-- A reference to the underlying audio unit is provided so that parameters that are not        exposed by AVAudioUnit subclasses can be modified using the AudioUnit C API.
--
-- No operations that may conflict with state maintained by the engine should be performed        directly on the audio unit. These include changing initialization state, stream formats,        channel layouts or connections to other audio units.
--
-- ObjC selector: @- audioUnit@
audioUnit :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO (Ptr ())
audioUnit avAudioUnit =
  sendMessage avAudioUnit audioUnitSelector

-- | AUAudioUnit
--
-- An AUAudioUnit wrapping or underlying the implementation's AudioUnit.
--
-- This provides an AUAudioUnit which either wraps or underlies the implementation's        AudioUnit, depending on how that audio unit is packaged. Applications can interact with this        AUAudioUnit to control custom properties, select presets, change parameters, etc.
--
-- As with the audioUnit property, no operations that may conflict with state maintained by the        engine should be performed directly on the audio unit. These include changing initialization        state, stream formats, channel layouts or connections to other audio units.
--
-- ObjC selector: @- AUAudioUnit@
auAudioUnit :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO (Id AUAudioUnit)
auAudioUnit avAudioUnit =
  sendMessage avAudioUnit auAudioUnitSelector

-- | name
--
-- Name of the audio unit.
--
-- ObjC selector: @- name@
name :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO (Id NSString)
name avAudioUnit =
  sendMessage avAudioUnit nameSelector

-- | manufacturerName
--
-- Manufacturer name of the audio unit.
--
-- ObjC selector: @- manufacturerName@
manufacturerName :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO (Id NSString)
manufacturerName avAudioUnit =
  sendMessage avAudioUnit manufacturerNameSelector

-- | version
--
-- Version number of the audio unit.
--
-- ObjC selector: @- version@
version :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO CULong
version avAudioUnit =
  sendMessage avAudioUnit versionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instantiateWithComponentDescription:options:completionHandler:@
instantiateWithComponentDescription_options_completionHandlerSelector :: Selector '[AudioComponentDescription, AudioComponentInstantiationOptions, Ptr ()] ()
instantiateWithComponentDescription_options_completionHandlerSelector = mkSelector "instantiateWithComponentDescription:options:completionHandler:"

-- | @Selector@ for @loadAudioUnitPresetAtURL:error:@
loadAudioUnitPresetAtURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
loadAudioUnitPresetAtURL_errorSelector = mkSelector "loadAudioUnitPresetAtURL:error:"

-- | @Selector@ for @audioComponentDescription@
audioComponentDescriptionSelector :: Selector '[] AudioComponentDescription
audioComponentDescriptionSelector = mkSelector "audioComponentDescription"

-- | @Selector@ for @audioUnit@
audioUnitSelector :: Selector '[] (Ptr ())
audioUnitSelector = mkSelector "audioUnit"

-- | @Selector@ for @AUAudioUnit@
auAudioUnitSelector :: Selector '[] (Id AUAudioUnit)
auAudioUnitSelector = mkSelector "AUAudioUnit"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @manufacturerName@
manufacturerNameSelector :: Selector '[] (Id NSString)
manufacturerNameSelector = mkSelector "manufacturerName"

-- | @Selector@ for @version@
versionSelector :: Selector '[] CULong
versionSelector = mkSelector "version"

