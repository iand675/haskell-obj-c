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
-- Create an AVAudioUnitGenerator object.
--
-- @audioComponentDescription@ — AudioComponentDescription of the audio unit to be instantiated.
--
-- The componentType must be kAudioUnitType_Generator or kAudioUnitType_RemoteGenerator
--
-- ObjC selector: @- initWithAudioComponentDescription:@
initWithAudioComponentDescription :: IsAVAudioUnitGenerator avAudioUnitGenerator => avAudioUnitGenerator -> AudioComponentDescription -> IO (Id AVAudioUnitGenerator)
initWithAudioComponentDescription avAudioUnitGenerator  audioComponentDescription =
  sendMsg avAudioUnitGenerator (mkSelector "initWithAudioComponentDescription:") (retPtr retVoid) [argAudioComponentDescription audioComponentDescription] >>= ownedObject . castPtr

-- | bypass
--
-- Bypass state of the audio unit.
--
-- ObjC selector: @- bypass@
bypass :: IsAVAudioUnitGenerator avAudioUnitGenerator => avAudioUnitGenerator -> IO Bool
bypass avAudioUnitGenerator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitGenerator (mkSelector "bypass") retCULong []

-- | bypass
--
-- Bypass state of the audio unit.
--
-- ObjC selector: @- setBypass:@
setBypass :: IsAVAudioUnitGenerator avAudioUnitGenerator => avAudioUnitGenerator -> Bool -> IO ()
setBypass avAudioUnitGenerator  value =
  sendMsg avAudioUnitGenerator (mkSelector "setBypass:") retVoid [argCULong (if value then 1 else 0)]

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

