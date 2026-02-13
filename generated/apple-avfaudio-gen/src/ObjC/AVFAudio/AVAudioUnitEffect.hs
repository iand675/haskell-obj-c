{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitEffect
--
-- an AVAudioUnit that processes audio in real-time
--
-- An AVAudioUnitEffect represents an audio unit of type kAudioUnitType_Effect,    kAudioUnitType_MusicEffect, kAudioUnitType_Panner, kAudioUnitType_RemoteEffect or     kAudioUnitType_RemoteMusicEffect.
--
-- These effects run in real-time and process some x number of audio input     samples to produce x number of audio output samples. A delay unit is an     example of an effect unit.
--
-- Generated bindings for @AVAudioUnitEffect@.
module ObjC.AVFAudio.AVAudioUnitEffect
  ( AVAudioUnitEffect
  , IsAVAudioUnitEffect(..)
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
-- Create an AVAudioUnitEffect object.
--
-- @audioComponentDescription@ — AudioComponentDescription of the audio unit to be instantiated.
--
-- The componentType must be one of these types    kAudioUnitType_Effect    kAudioUnitType_MusicEffect    kAudioUnitType_Panner    kAudioUnitType_RemoteEffect    kAudioUnitType_RemoteMusicEffect
--
-- ObjC selector: @- initWithAudioComponentDescription:@
initWithAudioComponentDescription :: IsAVAudioUnitEffect avAudioUnitEffect => avAudioUnitEffect -> AudioComponentDescription -> IO (Id AVAudioUnitEffect)
initWithAudioComponentDescription avAudioUnitEffect audioComponentDescription =
  sendOwnedMessage avAudioUnitEffect initWithAudioComponentDescriptionSelector audioComponentDescription

-- | bypass
--
-- Bypass state of the audio unit.
--
-- ObjC selector: @- bypass@
bypass :: IsAVAudioUnitEffect avAudioUnitEffect => avAudioUnitEffect -> IO Bool
bypass avAudioUnitEffect =
  sendMessage avAudioUnitEffect bypassSelector

-- | bypass
--
-- Bypass state of the audio unit.
--
-- ObjC selector: @- setBypass:@
setBypass :: IsAVAudioUnitEffect avAudioUnitEffect => avAudioUnitEffect -> Bool -> IO ()
setBypass avAudioUnitEffect value =
  sendMessage avAudioUnitEffect setBypassSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioComponentDescription:@
initWithAudioComponentDescriptionSelector :: Selector '[AudioComponentDescription] (Id AVAudioUnitEffect)
initWithAudioComponentDescriptionSelector = mkSelector "initWithAudioComponentDescription:"

-- | @Selector@ for @bypass@
bypassSelector :: Selector '[] Bool
bypassSelector = mkSelector "bypass"

-- | @Selector@ for @setBypass:@
setBypassSelector :: Selector '[Bool] ()
setBypassSelector = mkSelector "setBypass:"

