{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEPullStreamNodeDefinition
--
-- An object for defining a pull stream sound event node when building a sound event.
--
-- Generated bindings for @PHASEPullStreamNodeDefinition@.
module ObjC.PHASE.PHASEPullStreamNodeDefinition
  ( PHASEPullStreamNodeDefinition
  , IsPHASEPullStreamNodeDefinition(..)
  , init_
  , new
  , initWithMixerDefinition_format_identifier
  , initWithMixerDefinition_format
  , format
  , normalize
  , setNormalize
  , initSelector
  , newSelector
  , initWithMixerDefinition_format_identifierSelector
  , initWithMixerDefinition_formatSelector
  , formatSelector
  , normalizeSelector
  , setNormalizeSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition => phasePullStreamNodeDefinition -> IO (Id PHASEPullStreamNodeDefinition)
init_ phasePullStreamNodeDefinition  =
  sendMsg phasePullStreamNodeDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEPullStreamNodeDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEPullStreamNodeDefinition"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithMixerDefinition:format:identifier
--
-- Create a pull stream node definition
--
-- @mixerDefinition@ — The mixer definition this stream will be assigned to
--
-- @format@ — The AVAudioFormat object that will define the attributes of the audio this node will accept.        Only Core Audio's standard deinterleaved 32-bit floating-point formats are supported.
--
-- @identifier@ — An optional custom identifier to give to this object
--
-- Returns: A new PHASEPullStreamNodeDefinition object
--
-- ObjC selector: @- initWithMixerDefinition:format:identifier:@
initWithMixerDefinition_format_identifier :: (IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition, IsPHASEMixerDefinition mixerDefinition, IsAVAudioFormat format, IsNSString identifier) => phasePullStreamNodeDefinition -> mixerDefinition -> format -> identifier -> IO (Id PHASEPullStreamNodeDefinition)
initWithMixerDefinition_format_identifier phasePullStreamNodeDefinition  mixerDefinition format identifier =
withObjCPtr mixerDefinition $ \raw_mixerDefinition ->
  withObjCPtr format $ \raw_format ->
    withObjCPtr identifier $ \raw_identifier ->
        sendMsg phasePullStreamNodeDefinition (mkSelector "initWithMixerDefinition:format:identifier:") (retPtr retVoid) [argPtr (castPtr raw_mixerDefinition :: Ptr ()), argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | initWithMixerDefinition:format
--
-- Create a pull stream node definition
--
-- @mixerDefinition@ — The mixer definition this stream will be assigned to
--
-- @format@ — The AVAudioFormat object that will define the attributes of the audio this node will accept.        Only Core Audio's standard deinterleaved 32-bit floating-point formats are supported.
--
-- Returns: A new PHASEPullStreamNodeDefinition object
--
-- ObjC selector: @- initWithMixerDefinition:format:@
initWithMixerDefinition_format :: (IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition, IsPHASEMixerDefinition mixerDefinition, IsAVAudioFormat format) => phasePullStreamNodeDefinition -> mixerDefinition -> format -> IO (Id PHASEPullStreamNodeDefinition)
initWithMixerDefinition_format phasePullStreamNodeDefinition  mixerDefinition format =
withObjCPtr mixerDefinition $ \raw_mixerDefinition ->
  withObjCPtr format $ \raw_format ->
      sendMsg phasePullStreamNodeDefinition (mkSelector "initWithMixerDefinition:format:") (retPtr retVoid) [argPtr (castPtr raw_mixerDefinition :: Ptr ()), argPtr (castPtr raw_format :: Ptr ())] >>= ownedObject . castPtr

-- | format
--
-- The readonly property that returns the AVAudioFormat that this stream was initialized with
--
-- ObjC selector: @- format@
format :: IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition => phasePullStreamNodeDefinition -> IO (Id AVAudioFormat)
format phasePullStreamNodeDefinition  =
  sendMsg phasePullStreamNodeDefinition (mkSelector "format") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | normalize
--
-- Determines whether or not the engine should normalize the stream. The default value is NO.
--
-- In general, clients are advised to normalize the input. Normalization is required to properly calibrate the output level.        If you set this value to NO, it's advised that you do custom normalization of the audio data prior to passing the buffers to PHASE.
--
-- ObjC selector: @- normalize@
normalize :: IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition => phasePullStreamNodeDefinition -> IO Bool
normalize phasePullStreamNodeDefinition  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phasePullStreamNodeDefinition (mkSelector "normalize") retCULong []

-- | normalize
--
-- Determines whether or not the engine should normalize the stream. The default value is NO.
--
-- In general, clients are advised to normalize the input. Normalization is required to properly calibrate the output level.        If you set this value to NO, it's advised that you do custom normalization of the audio data prior to passing the buffers to PHASE.
--
-- ObjC selector: @- setNormalize:@
setNormalize :: IsPHASEPullStreamNodeDefinition phasePullStreamNodeDefinition => phasePullStreamNodeDefinition -> Bool -> IO ()
setNormalize phasePullStreamNodeDefinition  value =
  sendMsg phasePullStreamNodeDefinition (mkSelector "setNormalize:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithMixerDefinition:format:identifier:@
initWithMixerDefinition_format_identifierSelector :: Selector
initWithMixerDefinition_format_identifierSelector = mkSelector "initWithMixerDefinition:format:identifier:"

-- | @Selector@ for @initWithMixerDefinition:format:@
initWithMixerDefinition_formatSelector :: Selector
initWithMixerDefinition_formatSelector = mkSelector "initWithMixerDefinition:format:"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @normalize@
normalizeSelector :: Selector
normalizeSelector = mkSelector "normalize"

-- | @Selector@ for @setNormalize:@
setNormalizeSelector :: Selector
setNormalizeSelector = mkSelector "setNormalize:"

