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
  , initWithAudioComponentDescriptionSelector
  , bypassSelector
  , setBypassSelector


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
initWithAudioComponentDescription avAudioUnitEffect  audioComponentDescription =
  sendMsg avAudioUnitEffect (mkSelector "initWithAudioComponentDescription:") (retPtr retVoid) [argAudioComponentDescription audioComponentDescription] >>= ownedObject . castPtr

-- | bypass
--
-- Bypass state of the audio unit.
--
-- ObjC selector: @- bypass@
bypass :: IsAVAudioUnitEffect avAudioUnitEffect => avAudioUnitEffect -> IO Bool
bypass avAudioUnitEffect  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitEffect (mkSelector "bypass") retCULong []

-- | bypass
--
-- Bypass state of the audio unit.
--
-- ObjC selector: @- setBypass:@
setBypass :: IsAVAudioUnitEffect avAudioUnitEffect => avAudioUnitEffect -> Bool -> IO ()
setBypass avAudioUnitEffect  value =
  sendMsg avAudioUnitEffect (mkSelector "setBypass:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioComponentDescription:@
initWithAudioComponentDescriptionSelector :: Selector
initWithAudioComponentDescriptionSelector = mkSelector "initWithAudioComponentDescription:"

-- | @Selector@ for @bypass@
bypassSelector :: Selector
bypassSelector = mkSelector "bypass"

-- | @Selector@ for @setBypass:@
setBypassSelector :: Selector
setBypassSelector = mkSelector "setBypass:"

