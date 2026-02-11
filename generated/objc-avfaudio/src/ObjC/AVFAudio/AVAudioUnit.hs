{-# LANGUAGE PatternSynonyms #-}
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
  , instantiateWithComponentDescription_options_completionHandlerSelector
  , loadAudioUnitPresetAtURL_errorSelector
  , audioComponentDescriptionSelector
  , audioUnitSelector
  , auAudioUnitSelector
  , nameSelector
  , manufacturerNameSelector
  , versionSelector

  -- * Enum types
  , AudioComponentInstantiationOptions(AudioComponentInstantiationOptions)
  , pattern KAudioComponentInstantiation_LoadOutOfProcess
  , pattern KAudioComponentInstantiation_LoadInProcess
  , pattern KAudioComponentInstantiation_LoadedRemotely

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    sendClassMsg cls' (mkSelector "instantiateWithComponentDescription:options:completionHandler:") retVoid [argAudioComponentDescription audioComponentDescription, argCUInt (coerce options), argPtr (castPtr completionHandler :: Ptr ())]

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
loadAudioUnitPresetAtURL_error avAudioUnit  url outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnit (mkSelector "loadAudioUnitPresetAtURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | audioComponentDescription
--
-- AudioComponentDescription of the underlying audio unit.
--
-- ObjC selector: @- audioComponentDescription@
audioComponentDescription :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO AudioComponentDescription
audioComponentDescription avAudioUnit  =
  sendMsgStret avAudioUnit (mkSelector "audioComponentDescription") retAudioComponentDescription []

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
audioUnit avAudioUnit  =
  fmap castPtr $ sendMsg avAudioUnit (mkSelector "audioUnit") (retPtr retVoid) []

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
auAudioUnit avAudioUnit  =
  sendMsg avAudioUnit (mkSelector "AUAudioUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Name of the audio unit.
--
-- ObjC selector: @- name@
name :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO (Id NSString)
name avAudioUnit  =
  sendMsg avAudioUnit (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | manufacturerName
--
-- Manufacturer name of the audio unit.
--
-- ObjC selector: @- manufacturerName@
manufacturerName :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO (Id NSString)
manufacturerName avAudioUnit  =
  sendMsg avAudioUnit (mkSelector "manufacturerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | version
--
-- Version number of the audio unit.
--
-- ObjC selector: @- version@
version :: IsAVAudioUnit avAudioUnit => avAudioUnit -> IO CULong
version avAudioUnit  =
  sendMsg avAudioUnit (mkSelector "version") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instantiateWithComponentDescription:options:completionHandler:@
instantiateWithComponentDescription_options_completionHandlerSelector :: Selector
instantiateWithComponentDescription_options_completionHandlerSelector = mkSelector "instantiateWithComponentDescription:options:completionHandler:"

-- | @Selector@ for @loadAudioUnitPresetAtURL:error:@
loadAudioUnitPresetAtURL_errorSelector :: Selector
loadAudioUnitPresetAtURL_errorSelector = mkSelector "loadAudioUnitPresetAtURL:error:"

-- | @Selector@ for @audioComponentDescription@
audioComponentDescriptionSelector :: Selector
audioComponentDescriptionSelector = mkSelector "audioComponentDescription"

-- | @Selector@ for @audioUnit@
audioUnitSelector :: Selector
audioUnitSelector = mkSelector "audioUnit"

-- | @Selector@ for @AUAudioUnit@
auAudioUnitSelector :: Selector
auAudioUnitSelector = mkSelector "AUAudioUnit"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @manufacturerName@
manufacturerNameSelector :: Selector
manufacturerNameSelector = mkSelector "manufacturerName"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

