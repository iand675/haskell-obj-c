{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGeometricSpreadingDistanceModelParameters phaseGeometricSpreadingDistanceModelParameters => phaseGeometricSpreadingDistanceModelParameters -> IO (Id PHASEGeometricSpreadingDistanceModelParameters)
init_ phaseGeometricSpreadingDistanceModelParameters =
  sendOwnedMessage phaseGeometricSpreadingDistanceModelParameters initSelector

-- | rolloffFactor
--
-- Rolloff factor.
--
-- Note: Values are clamped to the range [0.0, DBL_MAX].        Default value is 1.0.        0.0 is no effect. 0.5 is half the effect. 1.0 is normal. 2.0 is double the effect.
--
-- ObjC selector: @- rolloffFactor@
rolloffFactor :: IsPHASEGeometricSpreadingDistanceModelParameters phaseGeometricSpreadingDistanceModelParameters => phaseGeometricSpreadingDistanceModelParameters -> IO CDouble
rolloffFactor phaseGeometricSpreadingDistanceModelParameters =
  sendMessage phaseGeometricSpreadingDistanceModelParameters rolloffFactorSelector

-- | rolloffFactor
--
-- Rolloff factor.
--
-- Note: Values are clamped to the range [0.0, DBL_MAX].        Default value is 1.0.        0.0 is no effect. 0.5 is half the effect. 1.0 is normal. 2.0 is double the effect.
--
-- ObjC selector: @- setRolloffFactor:@
setRolloffFactor :: IsPHASEGeometricSpreadingDistanceModelParameters phaseGeometricSpreadingDistanceModelParameters => phaseGeometricSpreadingDistanceModelParameters -> CDouble -> IO ()
setRolloffFactor phaseGeometricSpreadingDistanceModelParameters value =
  sendMessage phaseGeometricSpreadingDistanceModelParameters setRolloffFactorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEGeometricSpreadingDistanceModelParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @rolloffFactor@
rolloffFactorSelector :: Selector '[] CDouble
rolloffFactorSelector = mkSelector "rolloffFactor"

-- | @Selector@ for @setRolloffFactor:@
setRolloffFactorSelector :: Selector '[CDouble] ()
setRolloffFactorSelector = mkSelector "setRolloffFactor:"

