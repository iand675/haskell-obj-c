{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , filterTypeSelector
  , setFilterTypeSelector
  , frequencySelector
  , setFrequencySelector
  , bandwidthSelector
  , setBandwidthSelector
  , gainSelector
  , setGainSelector
  , bypassSelector
  , setBypassSelector

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
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO (Id AVAudioUnitEQFilterParameters)
init_ avAudioUnitEQFilterParameters  =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | filterType
--
-- AVAudioUnitEQFilterType
--
-- Default:    AVAudioUnitEQFilterTypeParametric
--
-- ObjC selector: @- filterType@
filterType :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO AVAudioUnitEQFilterType
filterType avAudioUnitEQFilterParameters  =
  fmap (coerce :: CLong -> AVAudioUnitEQFilterType) $ sendMsg avAudioUnitEQFilterParameters (mkSelector "filterType") retCLong []

-- | filterType
--
-- AVAudioUnitEQFilterType
--
-- Default:    AVAudioUnitEQFilterTypeParametric
--
-- ObjC selector: @- setFilterType:@
setFilterType :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> AVAudioUnitEQFilterType -> IO ()
setFilterType avAudioUnitEQFilterParameters  value =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "setFilterType:") retVoid [argCLong (coerce value)]

-- | frequency
--
-- Frequency in Hertz.
--
-- Range:      20 -> (SampleRate/2)    Unit:       Hertz
--
-- ObjC selector: @- frequency@
frequency :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO CFloat
frequency avAudioUnitEQFilterParameters  =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "frequency") retCFloat []

-- | frequency
--
-- Frequency in Hertz.
--
-- Range:      20 -> (SampleRate/2)    Unit:       Hertz
--
-- ObjC selector: @- setFrequency:@
setFrequency :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> CFloat -> IO ()
setFrequency avAudioUnitEQFilterParameters  value =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "setFrequency:") retVoid [argCFloat (fromIntegral value)]

-- | bandwidth
--
-- Bandwidth in octaves.
--
-- Range:      0.05 -> 5.0    Unit:       Octaves
--
-- ObjC selector: @- bandwidth@
bandwidth :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO CFloat
bandwidth avAudioUnitEQFilterParameters  =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "bandwidth") retCFloat []

-- | bandwidth
--
-- Bandwidth in octaves.
--
-- Range:      0.05 -> 5.0    Unit:       Octaves
--
-- ObjC selector: @- setBandwidth:@
setBandwidth :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> CFloat -> IO ()
setBandwidth avAudioUnitEQFilterParameters  value =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "setBandwidth:") retVoid [argCFloat (fromIntegral value)]

-- | gain
--
-- Gain in dB.
--
-- Range:      -96 -> 24    Default:    0    Unit:       dB
--
-- ObjC selector: @- gain@
gain :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO CFloat
gain avAudioUnitEQFilterParameters  =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "gain") retCFloat []

-- | gain
--
-- Gain in dB.
--
-- Range:      -96 -> 24    Default:    0    Unit:       dB
--
-- ObjC selector: @- setGain:@
setGain :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> CFloat -> IO ()
setGain avAudioUnitEQFilterParameters  value =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "setGain:") retVoid [argCFloat (fromIntegral value)]

-- | bypass
--
-- bypass state of band.
--
-- Default:    YES
--
-- ObjC selector: @- bypass@
bypass :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> IO Bool
bypass avAudioUnitEQFilterParameters  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioUnitEQFilterParameters (mkSelector "bypass") retCULong []

-- | bypass
--
-- bypass state of band.
--
-- Default:    YES
--
-- ObjC selector: @- setBypass:@
setBypass :: IsAVAudioUnitEQFilterParameters avAudioUnitEQFilterParameters => avAudioUnitEQFilterParameters -> Bool -> IO ()
setBypass avAudioUnitEQFilterParameters  value =
  sendMsg avAudioUnitEQFilterParameters (mkSelector "setBypass:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @filterType@
filterTypeSelector :: Selector
filterTypeSelector = mkSelector "filterType"

-- | @Selector@ for @setFilterType:@
setFilterTypeSelector :: Selector
setFilterTypeSelector = mkSelector "setFilterType:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @bandwidth@
bandwidthSelector :: Selector
bandwidthSelector = mkSelector "bandwidth"

-- | @Selector@ for @setBandwidth:@
setBandwidthSelector :: Selector
setBandwidthSelector = mkSelector "setBandwidth:"

-- | @Selector@ for @gain@
gainSelector :: Selector
gainSelector = mkSelector "gain"

-- | @Selector@ for @setGain:@
setGainSelector :: Selector
setGainSelector = mkSelector "setGain:"

-- | @Selector@ for @bypass@
bypassSelector :: Selector
bypassSelector = mkSelector "bypass"

-- | @Selector@ for @setBypass:@
setBypassSelector :: Selector
setBypassSelector = mkSelector "setBypass:"

