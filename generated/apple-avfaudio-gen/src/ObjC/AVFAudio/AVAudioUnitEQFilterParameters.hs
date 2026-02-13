{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitEQFilterParameters
--
-- Filter parameters used by AVAudioUnitEQ.
--
-- A standalone instance of AVAudioUnitEQFilterParameters cannot be created. Only an instance        vended out by a source object (e.g. AVAudioUnitEQ) can be used.
--
-- Generated bindings for @AVAudioUnitEQFilterParameters@.
module ObjC.AVFAudio.AVAudioUnitEQFilterParameters
  ( AVAudioUnitEQFilterParameters
  , IsAVAudioUnitEQFilterParameters(..)
  , init_
  , filterType
  , setFilterType
  , frequency
  , setFrequency
  , bandwidth
  , setBandwidth
  , gain
  , setGain
  , bypass
  , setBypass
  , bandwidthSelector
  , bypassSelector
  , filterTypeSelector
  , frequencySelector
  , gainSelector
  , initSelector
  , setBandwidthSelector
  , setBypassSelector
  , setFilterTypeSelector
  , setFrequencySelector
  , setGainSelector

  -- * Enum types
  , AVAudioUnitEQFilterType(AVAudioUnitEQFilterType)
  , pattern AVAudioUnitEQFilterTypeParametric
  , pattern AVAudioUnitEQFilterTypeLowPass
  , pattern AVAudioUnitEQFilterTypeHighPass
  , pattern AVAudioUnitEQFilterTypeResonantLowPass
  , pattern AVAudioUnitEQFilterTypeResonantHighPass
  , pattern AVAudioUnitEQFilterTypeBandPass
  , pattern AVAudioUnitEQFilterTypeBandStop
  , pattern AVAudioUnitEQFilterTypeLowShelf
  , pattern AVAudioUnitEQFilterTypeHighShelf
  , pattern AVAudioUnitEQFilterTypeResonantLowShelf
  , pattern AVAudioUnitEQFilterTypeResonantHighShelf

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO (Id AVAudioUnitEQFilterParameters)
init_ avAudioUnitEQFilterParameters =
  sendOwnedMessage avAudioUnitEQFilterParameters initSelector

-- | filterType
--
-- AVAudioUnitEQFilterType
--
-- Default:    AVAudioUnitEQFilterTypeParametric
--
-- ObjC selector: @- filterType@
filterType :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO AVAudioUnitEQFilterType
filterType avAudioUnitEQFilterParameters =
  sendMessage avAudioUnitEQFilterParameters filterTypeSelector

-- | filterType
--
-- AVAudioUnitEQFilterType
--
-- Default:    AVAudioUnitEQFilterTypeParametric
--
-- ObjC selector: @- setFilterType:@
setFilterType :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> AVAudioUnitEQFilterType -> IO ()
setFilterType avAudioUnitEQFilterParameters value =
  sendMessage avAudioUnitEQFilterParameters setFilterTypeSelector value

-- | frequency
--
-- Frequency in Hertz.
--
-- Range:      20 -> (SampleRate/2)    Unit:       Hertz
--
-- ObjC selector: @- frequency@
frequency :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO CFloat
frequency avAudioUnitEQFilterParameters =
  sendMessage avAudioUnitEQFilterParameters frequencySelector

-- | frequency
--
-- Frequency in Hertz.
--
-- Range:      20 -> (SampleRate/2)    Unit:       Hertz
--
-- ObjC selector: @- setFrequency:@
setFrequency :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> CFloat -> IO ()
setFrequency avAudioUnitEQFilterParameters value =
  sendMessage avAudioUnitEQFilterParameters setFrequencySelector value

-- | bandwidth
--
-- Bandwidth in octaves.
--
-- Range:      0.05 -> 5.0    Unit:       Octaves
--
-- ObjC selector: @- bandwidth@
bandwidth :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO CFloat
bandwidth avAudioUnitEQFilterParameters =
  sendMessage avAudioUnitEQFilterParameters bandwidthSelector

-- | bandwidth
--
-- Bandwidth in octaves.
--
-- Range:      0.05 -> 5.0    Unit:       Octaves
--
-- ObjC selector: @- setBandwidth:@
setBandwidth :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> CFloat -> IO ()
setBandwidth avAudioUnitEQFilterParameters value =
  sendMessage avAudioUnitEQFilterParameters setBandwidthSelector value

-- | gain
--
-- Gain in dB.
--
-- Range:      -96 -> 24    Default:    0    Unit:       dB
--
-- ObjC selector: @- gain@
gain :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO CFloat
gain avAudioUnitEQFilterParameters =
  sendMessage avAudioUnitEQFilterParameters gainSelector

-- | gain
--
-- Gain in dB.
--
-- Range:      -96 -> 24    Default:    0    Unit:       dB
--
-- ObjC selector: @- setGain:@
setGain :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> CFloat -> IO ()
setGain avAudioUnitEQFilterParameters value =
  sendMessage avAudioUnitEQFilterParameters setGainSelector value

-- | bypass
--
-- bypass state of band.
--
-- Default:    YES
--
-- ObjC selector: @- bypass@
bypass :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO Bool
bypass avAudioUnitEQFilterParameters =
  sendMessage avAudioUnitEQFilterParameters bypassSelector

-- | bypass
--
-- bypass state of band.
--
-- Default:    YES
--
-- ObjC selector: @- setBypass:@
setBypass :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> Bool -> IO ()
setBypass avAudioUnitEQFilterParameters value =
  sendMessage avAudioUnitEQFilterParameters setBypassSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioUnitEQFilterParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @filterType@
filterTypeSelector :: Selector '[] AVAudioUnitEQFilterType
filterTypeSelector = mkSelector "filterType"

-- | @Selector@ for @setFilterType:@
setFilterTypeSelector :: Selector '[AVAudioUnitEQFilterType] ()
setFilterTypeSelector = mkSelector "setFilterType:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] CFloat
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector '[CFloat] ()
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @bandwidth@
bandwidthSelector :: Selector '[] CFloat
bandwidthSelector = mkSelector "bandwidth"

-- | @Selector@ for @setBandwidth:@
setBandwidthSelector :: Selector '[CFloat] ()
setBandwidthSelector = mkSelector "setBandwidth:"

-- | @Selector@ for @gain@
gainSelector :: Selector '[] CFloat
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector '[CFloat] ()
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @bypass@
bypassSelector :: Selector '[] Bool
bypassSelector = mkSelector "bypass"

-- | @Selector@ for @setBypass:@
setBypassSelector :: Selector '[Bool] ()
setBypassSelector = mkSelector "setBypass:"

