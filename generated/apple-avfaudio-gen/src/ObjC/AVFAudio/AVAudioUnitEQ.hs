{-# LANGUAGE DataKinds #-}
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
  , bandsSelector
  , globalGainSelector
  , initWithNumberOfBandsSelector
  , setGlobalGainSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithNumberOfBands avAudioUnitEQ numberOfBands =
  sendOwnedMessage avAudioUnitEQ initWithNumberOfBandsSelector numberOfBands

-- | bands
--
-- Array of AVAudioUnitEQFilterParameters objects.
--
-- The number of elements in the array is equal to the number of bands.
--
-- ObjC selector: @- bands@
bands :: IsAVAudioUnitEQ avAudioUnitEQ => avAudioUnitEQ -> IO (Id NSArray)
bands avAudioUnitEQ =
  sendMessage avAudioUnitEQ bandsSelector

-- | globalGain
--
-- Overall gain adjustment applied to the signal.
--
-- Range:     -96 -> 24        Default:   0        Unit:      dB
--
-- ObjC selector: @- globalGain@
globalGain :: IsAVAudioUnitEQ avAudioUnitEQ => avAudioUnitEQ -> IO CFloat
globalGain avAudioUnitEQ =
  sendMessage avAudioUnitEQ globalGainSelector

-- | globalGain
--
-- Overall gain adjustment applied to the signal.
--
-- Range:     -96 -> 24        Default:   0        Unit:      dB
--
-- ObjC selector: @- setGlobalGain:@
setGlobalGain :: IsAVAudioUnitEQ avAudioUnitEQ => avAudioUnitEQ -> CFloat -> IO ()
setGlobalGain avAudioUnitEQ value =
  sendMessage avAudioUnitEQ setGlobalGainSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNumberOfBands:@
initWithNumberOfBandsSelector :: Selector '[CULong] (Id AVAudioUnitEQ)
initWithNumberOfBandsSelector = mkSelector "initWithNumberOfBands:"

-- | @Selector@ for @bands@
bandsSelector :: Selector '[] (Id NSArray)
bandsSelector = mkSelector "bands"

-- | @Selector@ for @globalGain@
globalGainSelector :: Selector '[] CFloat
globalGainSelector = mkSelector "globalGain"

-- | @Selector@ for @setGlobalGain:@
setGlobalGainSelector :: Selector '[CFloat] ()
setGlobalGainSelector = mkSelector "setGlobalGain:"

