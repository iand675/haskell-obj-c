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
-- create an AVAudioUnitTimeEffect object
--
-- @audioComponentDescription@ — AudioComponentDescription of the audio unit to be initialized
--
-- The componentType must be kAudioUnitType_FormatConverter
--
-- ObjC selector: @- initWithAudioComponentDescription:@
initWithAudioComponentDescription :: IsAVAudioUnitTimeEffect avAudioUnitTimeEffect => avAudioUnitTimeEffect -> AudioComponentDescription -> IO (Id AVAudioUnitTimeEffect)
initWithAudioComponentDescription avAudioUnitTimeEffect  audioComponentDescription =
  sendMsg avAudioUnitTimeEffect (mkSelector "initWithAudioComponentDescription:") (retPtr retVoid) [argAudioComponentDescription audioComponentDescription] >>= ownedObject . castPtr

-- | bypass
--
-- bypass state of the audio unit
--
-- ObjC selector: @- bypass@
bypass :: IsAVAudioUnitTimeEffect avAudioUnitTimeEffect => avAudioUnitTimeEffect -> IO Bool
bypass avAudioUnitTimeEffect  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitTimeEffect (mkSelector "bypass") retCULong []

-- | bypass
--
-- bypass state of the audio unit
--
-- ObjC selector: @- setBypass:@
setBypass :: IsAVAudioUnitTimeEffect avAudioUnitTimeEffect => avAudioUnitTimeEffect -> Bool -> IO ()
setBypass avAudioUnitTimeEffect  value =
  sendMsg avAudioUnitTimeEffect (mkSelector "setBypass:") retVoid [argCULong (if value then 1 else 0)]

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

