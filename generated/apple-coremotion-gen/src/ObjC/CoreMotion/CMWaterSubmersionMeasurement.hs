{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMWaterSubmersionMeasurement@.
module ObjC.CoreMotion.CMWaterSubmersionMeasurement
  ( CMWaterSubmersionMeasurement
  , IsCMWaterSubmersionMeasurement(..)
  , date
  , depth
  , pressure
  , surfacePressure
  , submersionState
  , dateSelector
  , depthSelector
  , pressureSelector
  , submersionStateSelector
  , surfacePressureSelector

  -- * Enum types
  , CMWaterSubmersionDepthState(CMWaterSubmersionDepthState)
  , pattern CMWaterSubmersionDepthStateUnknown
  , pattern CMWaterSubmersionDepthStateNotSubmerged
  , pattern CMWaterSubmersionDepthStateSubmergedShallow
  , pattern CMWaterSubmersionDepthStateSubmergedDeep
  , pattern CMWaterSubmersionDepthStateApproachingMaxDepth
  , pattern CMWaterSubmersionDepthStatePastMaxDepth
  , pattern CMWaterSubmersionDepthStateSensorDepthError

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- date@
date :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO (Id NSDate)
date cmWaterSubmersionMeasurement =
  sendMessage cmWaterSubmersionMeasurement dateSelector

-- | @- depth@
depth :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO (Id NSMeasurement)
depth cmWaterSubmersionMeasurement =
  sendMessage cmWaterSubmersionMeasurement depthSelector

-- | @- pressure@
pressure :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO (Id NSMeasurement)
pressure cmWaterSubmersionMeasurement =
  sendMessage cmWaterSubmersionMeasurement pressureSelector

-- | @- surfacePressure@
surfacePressure :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO (Id NSMeasurement)
surfacePressure cmWaterSubmersionMeasurement =
  sendMessage cmWaterSubmersionMeasurement surfacePressureSelector

-- | @- submersionState@
submersionState :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO CMWaterSubmersionDepthState
submersionState cmWaterSubmersionMeasurement =
  sendMessage cmWaterSubmersionMeasurement submersionStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @depth@
depthSelector :: Selector '[] (Id NSMeasurement)
depthSelector = mkSelector "depth"

-- | @Selector@ for @pressure@
pressureSelector :: Selector '[] (Id NSMeasurement)
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @surfacePressure@
surfacePressureSelector :: Selector '[] (Id NSMeasurement)
surfacePressureSelector = mkSelector "surfacePressure"

-- | @Selector@ for @submersionState@
submersionStateSelector :: Selector '[] CMWaterSubmersionDepthState
submersionStateSelector = mkSelector "submersionState"

