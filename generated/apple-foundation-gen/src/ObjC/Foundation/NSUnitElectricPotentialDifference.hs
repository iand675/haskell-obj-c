{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitElectricPotentialDifference@.
module ObjC.Foundation.NSUnitElectricPotentialDifference
  ( NSUnitElectricPotentialDifference
  , IsNSUnitElectricPotentialDifference(..)
  , megavolts
  , kilovolts
  , volts
  , millivolts
  , microvolts
  , kilovoltsSelector
  , megavoltsSelector
  , microvoltsSelector
  , millivoltsSelector
  , voltsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ megavolts@
megavolts :: IO (Id NSUnitElectricPotentialDifference)
megavolts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMessage cls' megavoltsSelector

-- | @+ kilovolts@
kilovolts :: IO (Id NSUnitElectricPotentialDifference)
kilovolts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMessage cls' kilovoltsSelector

-- | @+ volts@
volts :: IO (Id NSUnitElectricPotentialDifference)
volts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMessage cls' voltsSelector

-- | @+ millivolts@
millivolts :: IO (Id NSUnitElectricPotentialDifference)
millivolts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMessage cls' millivoltsSelector

-- | @+ microvolts@
microvolts :: IO (Id NSUnitElectricPotentialDifference)
microvolts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMessage cls' microvoltsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megavolts@
megavoltsSelector :: Selector '[] (Id NSUnitElectricPotentialDifference)
megavoltsSelector = mkSelector "megavolts"

-- | @Selector@ for @kilovolts@
kilovoltsSelector :: Selector '[] (Id NSUnitElectricPotentialDifference)
kilovoltsSelector = mkSelector "kilovolts"

-- | @Selector@ for @volts@
voltsSelector :: Selector '[] (Id NSUnitElectricPotentialDifference)
voltsSelector = mkSelector "volts"

-- | @Selector@ for @millivolts@
millivoltsSelector :: Selector '[] (Id NSUnitElectricPotentialDifference)
millivoltsSelector = mkSelector "millivolts"

-- | @Selector@ for @microvolts@
microvoltsSelector :: Selector '[] (Id NSUnitElectricPotentialDifference)
microvoltsSelector = mkSelector "microvolts"

