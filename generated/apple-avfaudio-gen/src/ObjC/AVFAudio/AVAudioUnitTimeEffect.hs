{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitTimeEffect
--
-- an AVAudioUnit that processes audio in non real-time
--
-- An AVAudioUnitTimeEffect represents an audio unit of type aufc.    These effects do not process audio in real-time. The varispeed    unit is an example of a time effect unit.
--
-- Generated bindings for @AVAudioUnitTimeEffect@.
module ObjC.AVFAudio.AVAudioUnitTimeEffect
  ( AVAudioUnitTimeEffect
  , IsAVAudioUnitTimeEffect(..)
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
-- create an AVAudioUnitTimeEffect object
--
-- @audioComponentDescription@ — AudioComponentDescription of the audio unit to be initialized
--
-- The componentType must be kAudioUnitType_FormatConverter
--
-- ObjC selector: @- initWithAudioComponentDescription:@
initWithAudioComponentDescription :: IsAVAudioUnitTimeEffect avAudioUnitTimeEffect => avAudioUnitTimeEffect -> AudioComponentDescription -> IO (Id AVAudioUnitTimeEffect)
initWithAudioComponentDescription avAudioUnitTimeEffect audioComponentDescription =
  sendOwnedMessage avAudioUnitTimeEffect initWithAudioComponentDescriptionSelector audioComponentDescription

-- | bypass
--
-- bypass state of the audio unit
--
-- ObjC selector: @- bypass@
bypass :: IsAVAudioUnitTimeEffect avAudioUnitTimeEffect => avAudioUnitTimeEffect -> IO Bool
bypass avAudioUnitTimeEffect =
  sendMessage avAudioUnitTimeEffect bypassSelector

-- | bypass
--
-- bypass state of the audio unit
--
-- ObjC selector: @- setBypass:@
setBypass :: IsAVAudioUnitTimeEffect avAudioUnitTimeEffect => avAudioUnitTimeEffect -> Bool -> IO ()
setBypass avAudioUnitTimeEffect value =
  sendMessage avAudioUnitTimeEffect setBypassSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioComponentDescription:@
initWithAudioComponentDescriptionSelector :: Selector '[AudioComponentDescription] (Id AVAudioUnitTimeEffect)
initWithAudioComponentDescriptionSelector = mkSelector "initWithAudioComponentDescription:"

-- | @Selector@ for @bypass@
bypassSelector :: Selector '[] Bool
bypassSelector = mkSelector "bypass"

-- | @Selector@ for @setBypass:@
setBypassSelector :: Selector '[Bool] ()
setBypassSelector = mkSelector "setBypass:"

