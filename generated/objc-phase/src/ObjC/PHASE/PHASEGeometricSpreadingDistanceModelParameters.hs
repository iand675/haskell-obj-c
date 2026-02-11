{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEGeometricSpreadingDistanceModelParameters
--
-- Geometric spreading distance model parameters.
--
-- Standard geometric spreading loss as a function of geometry and distance.
--
-- Generated bindings for @PHASEGeometricSpreadingDistanceModelParameters@.
module ObjC.PHASE.PHASEGeometricSpreadingDistanceModelParameters
  ( PHASEGeometricSpreadingDistanceModelParameters
  , IsPHASEGeometricSpreadingDistanceModelParameters(..)
  , init_
  , rolloffFactor
  , setRolloffFactor
  , initSelector
  , rolloffFactorSelector
  , setRolloffFactorSelector


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
init_ :: IsPHASEGeometricSpreadingDistanceModelParameters phaseGeometricSpreadingDistanceModelParameters => phaseGeometricSpreadingDistanceModelParameters -> IO (Id PHASEGeometricSpreadingDistanceModelParameters)
init_ phaseGeometricSpreadingDistanceModelParameters  =
  sendMsg phaseGeometricSpreadingDistanceModelParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | rolloffFactor
--
-- Rolloff factor.
--
-- Note: Values are clamped to the range [0.0, DBL_MAX].        Default value is 1.0.        0.0 is no effect. 0.5 is half the effect. 1.0 is normal. 2.0 is double the effect.
--
-- ObjC selector: @- rolloffFactor@
rolloffFactor :: IsPHASEGeometricSpreadingDistanceModelParameters phaseGeometricSpreadingDistanceModelParameters => phaseGeometricSpreadingDistanceModelParameters -> IO CDouble
rolloffFactor phaseGeometricSpreadingDistanceModelParameters  =
  sendMsg phaseGeometricSpreadingDistanceModelParameters (mkSelector "rolloffFactor") retCDouble []

-- | rolloffFactor
--
-- Rolloff factor.
--
-- Note: Values are clamped to the range [0.0, DBL_MAX].        Default value is 1.0.        0.0 is no effect. 0.5 is half the effect. 1.0 is normal. 2.0 is double the effect.
--
-- ObjC selector: @- setRolloffFactor:@
setRolloffFactor :: IsPHASEGeometricSpreadingDistanceModelParameters phaseGeometricSpreadingDistanceModelParameters => phaseGeometricSpreadingDistanceModelParameters -> CDouble -> IO ()
setRolloffFactor phaseGeometricSpreadingDistanceModelParameters  value =
  sendMsg phaseGeometricSpreadingDistanceModelParameters (mkSelector "setRolloffFactor:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @rolloffFactor@
rolloffFactorSelector :: Selector
rolloffFactorSelector = mkSelector "rolloffFactor"

-- | @Selector@ for @setRolloffFactor:@
setRolloffFactorSelector :: Selector
setRolloffFactorSelector = mkSelector "setRolloffFactor:"

