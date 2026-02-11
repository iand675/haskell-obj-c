{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMPedometerData@.
module ObjC.CoreMotion.CMPedometerData
  ( CMPedometerData
  , IsCMPedometerData(..)
  , startDate
  , endDate
  , numberOfSteps
  , distance
  , floorsAscended
  , floorsDescended
  , startDateSelector
  , endDateSelector
  , numberOfStepsSelector
  , distanceSelector
  , floorsAscendedSelector
  , floorsDescendedSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- startDate@
startDate :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSDate)
startDate cmPedometerData  =
  sendMsg cmPedometerData (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSDate)
endDate cmPedometerData  =
  sendMsg cmPedometerData (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfSteps@
numberOfSteps :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSNumber)
numberOfSteps cmPedometerData  =
  sendMsg cmPedometerData (mkSelector "numberOfSteps") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- distance@
distance :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSNumber)
distance cmPedometerData  =
  sendMsg cmPedometerData (mkSelector "distance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- floorsAscended@
floorsAscended :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSNumber)
floorsAscended cmPedometerData  =
  sendMsg cmPedometerData (mkSelector "floorsAscended") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- floorsDescended@
floorsDescended :: IsCMPedometerData cmPedometerData => cmPedometerData -> IO (Id NSNumber)
floorsDescended cmPedometerData  =
  sendMsg cmPedometerData (mkSelector "floorsDescended") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @numberOfSteps@
numberOfStepsSelector :: Selector
numberOfStepsSelector = mkSelector "numberOfSteps"

-- | @Selector@ for @distance@
distanceSelector :: Selector
distanceSelector = mkSelector "distance"

-- | @Selector@ for @floorsAscended@
floorsAscendedSelector :: Selector
floorsAscendedSelector = mkSelector "floorsAscended"

-- | @Selector@ for @floorsDescended@
floorsDescendedSelector :: Selector
floorsDescendedSelector = mkSelector "floorsDescended"

