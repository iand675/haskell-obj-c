{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASECardioidDirectivityModelSubbandParameters
--
-- Cardioid directivity model subband parameters.
--
-- Generated bindings for @PHASECardioidDirectivityModelSubbandParameters@.
module ObjC.PHASE.PHASECardioidDirectivityModelSubbandParameters
  ( PHASECardioidDirectivityModelSubbandParameters
  , IsPHASECardioidDirectivityModelSubbandParameters(..)
  , init_
  , frequency
  , setFrequency
  , pattern_
  , setPattern
  , sharpness
  , setSharpness
  , initSelector
  , frequencySelector
  , setFrequencySelector
  , patternSelector
  , setPatternSelector
  , sharpnessSelector
  , setSharpnessSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> IO (Id PHASECardioidDirectivityModelSubbandParameters)
init_ phaseCardioidDirectivityModelSubbandParameters  =
  sendMsg phaseCardioidDirectivityModelSubbandParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | frequency
--
-- The frequency of the subband, in hertz.
--
-- Note: Values are clamped to the range [20.0, 20000.0]. Default value is 1000.0.
--
-- ObjC selector: @- frequency@
frequency :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> IO CDouble
frequency phaseCardioidDirectivityModelSubbandParameters  =
  sendMsg phaseCardioidDirectivityModelSubbandParameters (mkSelector "frequency") retCDouble []

-- | frequency
--
-- The frequency of the subband, in hertz.
--
-- Note: Values are clamped to the range [20.0, 20000.0]. Default value is 1000.0.
--
-- ObjC selector: @- setFrequency:@
setFrequency :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> CDouble -> IO ()
setFrequency phaseCardioidDirectivityModelSubbandParameters  value =
  sendMsg phaseCardioidDirectivityModelSubbandParameters (mkSelector "setFrequency:") retVoid [argCDouble (fromIntegral value)]

-- | pattern
--
-- The directivity pattern.
--
-- Note: Values are clamped to the range [0.0, 1.0]. Default value is 0.0. 0.0 is omnidirectional. 0.5 is cardioid. 1.0 is dipole.
--
-- ObjC selector: @- pattern@
pattern_ :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> IO CDouble
pattern_ phaseCardioidDirectivityModelSubbandParameters  =
  sendMsg phaseCardioidDirectivityModelSubbandParameters (mkSelector "pattern") retCDouble []

-- | pattern
--
-- The directivity pattern.
--
-- Note: Values are clamped to the range [0.0, 1.0]. Default value is 0.0. 0.0 is omnidirectional. 0.5 is cardioid. 1.0 is dipole.
--
-- ObjC selector: @- setPattern:@
setPattern :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> CDouble -> IO ()
setPattern phaseCardioidDirectivityModelSubbandParameters  value =
  sendMsg phaseCardioidDirectivityModelSubbandParameters (mkSelector "setPattern:") retVoid [argCDouble (fromIntegral value)]

-- | sharpness
--
-- The sharpness of the directivity pattern.
--
-- Note: Values are clamped to the range [1.0, DBL_MAX]. Default value is 1.0. Values > 1.0 increase sharpness.
--
-- ObjC selector: @- sharpness@
sharpness :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> IO CDouble
sharpness phaseCardioidDirectivityModelSubbandParameters  =
  sendMsg phaseCardioidDirectivityModelSubbandParameters (mkSelector "sharpness") retCDouble []

-- | sharpness
--
-- The sharpness of the directivity pattern.
--
-- Note: Values are clamped to the range [1.0, DBL_MAX]. Default value is 1.0. Values > 1.0 increase sharpness.
--
-- ObjC selector: @- setSharpness:@
setSharpness :: IsPHASECardioidDirectivityModelSubbandParameters phaseCardioidDirectivityModelSubbandParameters => phaseCardioidDirectivityModelSubbandParameters -> CDouble -> IO ()
setSharpness phaseCardioidDirectivityModelSubbandParameters  value =
  sendMsg phaseCardioidDirectivityModelSubbandParameters (mkSelector "setSharpness:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector
setFrequencySelector = mkSelector "setFrequency:"

-- | @Selector@ for @pattern@
patternSelector :: Selector
patternSelector = mkSelector "pattern"

-- | @Selector@ for @setPattern:@
setPatternSelector :: Selector
setPatternSelector = mkSelector "setPattern:"

-- | @Selector@ for @sharpness@
sharpnessSelector :: Selector
sharpnessSelector = mkSelector "sharpness"

-- | @Selector@ for @setSharpness:@
setSharpnessSelector :: Selector
setSharpnessSelector = mkSelector "setSharpness:"

