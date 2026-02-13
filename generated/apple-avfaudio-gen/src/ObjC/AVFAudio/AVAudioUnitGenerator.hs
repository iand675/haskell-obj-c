{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitGenerator
--
-- an AVAudioUnit that generates audio output
--
-- An AVAudioUnitGenerator represents an audio unit of type kAudioUnitType_Generator or	kAudioUnitType_RemoteGenerator.    A generator will have no audio input, but will just produce audio output.    A tone generator is an example of this.
--
-- Generated bindings for @AVAudioUnitGenerator@.
module ObjC.AVFAudio.AVAudioUnitGenerator
  ( AVAudioUnitGenerator
  , IsAVAudioUnitGenerator(..)
  , initWithAudioComponentDescription
  , bypass
  , setBypass
  , bypassSelector
  , initWithAudioComponentDescriptionSelector
  , setBypassSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AudioToolbox.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | initWithAudioComponentDescription:
--
-- Create an AVAudioUnitGenerator object.
--
-- @audioComponentDescription@ — AudioComponentDescription of the audio unit to be instantiated.
--
-- The componentType must be kAudioUnitType_Generator or kAudioUnitType_RemoteGenerator
--
-- ObjC selector: @- initWithAudioComponentDescription:@
initWithAudioComponentDescription :: IsAVAudioUnitGenerator avAudioUnitGenerator => avAudioUnitGenerator -> AudioComponentDescription -> IO (Id AVAudioUnitGenerator)
initWithAudioComponentDescription avAudioUnitGenerator audioComponentDescription =
  sendOwnedMessage avAudioUnitGenerator initWithAudioComponentDescriptionSelector audioComponentDescription

-- | bypass
--
-- Bypass state of the audio unit.
--
-- ObjC selector: @- bypass@
bypass :: IsAVAudioUnitGenerator avAudioUnitGenerator => avAudioUnitGenerator -> IO Bool
bypass avAudioUnitGenerator =
  sendMessage avAudioUnitGenerator bypassSelector

-- | bypass
--
-- Bypass state of the audio unit.
--
-- ObjC selector: @- setBypass:@
setBypass :: IsAVAudioUnitGenerator avAudioUnitGenerator => avAudioUnitGenerator -> Bool -> IO ()
setBypass avAudioUnitGenerator value =
  sendMessage avAudioUnitGenerator setBypassSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioComponentDescription:@
initWithAudioComponentDescriptionSelector :: Selector '[AudioComponentDescription] (Id AVAudioUnitGenerator)
initWithAudioComponentDescriptionSelector = mkSelector "initWithAudioComponentDescription:"

-- | @Selector@ for @bypass@
bypassSelector :: Selector '[] Bool
bypassSelector = mkSelector "bypass"

-- | @Selector@ for @setBypass:@
setBypassSelector :: Selector '[Bool] ()
setBypassSelector = mkSelector "setBypass:"

