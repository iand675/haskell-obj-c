{-# LANGUAGE PatternSynonyms #-}
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
  , surfacePressureSelector
  , submersionStateSelector

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

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- date@
date :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO (Id NSDate)
date cmWaterSubmersionMeasurement  =
  sendMsg cmWaterSubmersionMeasurement (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- depth@
depth :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO (Id NSMeasurement)
depth cmWaterSubmersionMeasurement  =
  sendMsg cmWaterSubmersionMeasurement (mkSelector "depth") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pressure@
pressure :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO (Id NSMeasurement)
pressure cmWaterSubmersionMeasurement  =
  sendMsg cmWaterSubmersionMeasurement (mkSelector "pressure") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- surfacePressure@
surfacePressure :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO (Id NSMeasurement)
surfacePressure cmWaterSubmersionMeasurement  =
  sendMsg cmWaterSubmersionMeasurement (mkSelector "surfacePressure") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- submersionState@
submersionState :: IsCMWaterSubmersionMeasurement cmWaterSubmersionMeasurement => cmWaterSubmersionMeasurement -> IO CMWaterSubmersionDepthState
submersionState cmWaterSubmersionMeasurement  =
  fmap (coerce :: CLong -> CMWaterSubmersionDepthState) $ sendMsg cmWaterSubmersionMeasurement (mkSelector "submersionState") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @depth@
depthSelector :: Selector
depthSelector = mkSelector "depth"

-- | @Selector@ for @pressure@
pressureSelector :: Selector
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @surfacePressure@
surfacePressureSelector :: Selector
surfacePressureSelector = mkSelector "surfacePressure"

-- | @Selector@ for @submersionState@
submersionStateSelector :: Selector
submersionStateSelector = mkSelector "submersionState"

