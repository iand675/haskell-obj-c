{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitEQ
--
-- An AVAudioUnitEffect that implements a Multi-Band Equalizer.
--
-- Generated bindings for @AVAudioUnitEQ@.
module ObjC.AVFAudio.AVAudioUnitEQ
  ( AVAudioUnitEQ
  , IsAVAudioUnitEQ(..)
  , initWithNumberOfBands
  , bands
  , globalGain
  , setGlobalGain
  , initWithNumberOfBandsSelector
  , bandsSelector
  , globalGainSelector
  , setGlobalGainSelector


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
import ObjC.Foundation.Internal.Classes

-- | initWithNumberOfBands:
--
-- Initialize the EQ with number of bands.
--
-- @numberOfBands@ — The number of bands created by the EQ.
--
-- ObjC selector: @- initWithNumberOfBands:@
initWithNumberOfBands :: IsAVAudioUnitEQ avAudioUnitEQ => avAudioUnitEQ -> CULong -> IO (Id AVAudioUnitEQ)
initWithNumberOfBands avAudioUnitEQ  numberOfBands =
  sendMsg avAudioUnitEQ (mkSelector "initWithNumberOfBands:") (retPtr retVoid) [argCULong (fromIntegral numberOfBands)] >>= ownedObject . castPtr

-- | bands
--
-- Array of AVAudioUnitEQFilterParameters objects.
--
-- The number of elements in the array is equal to the number of bands.
--
-- ObjC selector: @- bands@
bands :: IsAVAudioUnitEQ avAudioUnitEQ => avAudioUnitEQ -> IO (Id NSArray)
bands avAudioUnitEQ  =
  sendMsg avAudioUnitEQ (mkSelector "bands") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | globalGain
--
-- Overall gain adjustment applied to the signal.
--
-- Range:     -96 -> 24        Default:   0        Unit:      dB
--
-- ObjC selector: @- globalGain@
globalGain :: IsAVAudioUnitEQ avAudioUnitEQ => avAudioUnitEQ -> IO CFloat
globalGain avAudioUnitEQ  =
  sendMsg avAudioUnitEQ (mkSelector "globalGain") retCFloat []

-- | globalGain
--
-- Overall gain adjustment applied to the signal.
--
-- Range:     -96 -> 24        Default:   0        Unit:      dB
--
-- ObjC selector: @- setGlobalGain:@
setGlobalGain :: IsAVAudioUnitEQ avAudioUnitEQ => avAudioUnitEQ -> CFloat -> IO ()
setGlobalGain avAudioUnitEQ  value =
  sendMsg avAudioUnitEQ (mkSelector "setGlobalGain:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNumberOfBands:@
initWithNumberOfBandsSelector :: Selector
initWithNumberOfBandsSelector = mkSelector "initWithNumberOfBands:"

-- | @Selector@ for @bands@
bandsSelector :: Selector
bandsSelector = mkSelector "bands"

-- | @Selector@ for @globalGain@
globalGainSelector :: Selector
globalGainSelector = mkSelector "globalGain"

-- | @Selector@ for @setGlobalGain:@
setGlobalGainSelector :: Selector
setGlobalGainSelector = mkSelector "setGlobalGain:"

