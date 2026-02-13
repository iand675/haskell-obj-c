{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitElectricCharge@.
module ObjC.Foundation.NSUnitElectricCharge
  ( NSUnitElectricCharge
  , IsNSUnitElectricCharge(..)
  , coulombs
  , megaampereHours
  , kiloampereHours
  , ampereHours
  , milliampereHours
  , microampereHours
  , ampereHoursSelector
  , coulombsSelector
  , kiloampereHoursSelector
  , megaampereHoursSelector
  , microampereHoursSelector
  , milliampereHoursSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ coulombs@
coulombs :: IO (Id NSUnitElectricCharge)
coulombs  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMessage cls' coulombsSelector

-- | @+ megaampereHours@
megaampereHours :: IO (Id NSUnitElectricCharge)
megaampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMessage cls' megaampereHoursSelector

-- | @+ kiloampereHours@
kiloampereHours :: IO (Id NSUnitElectricCharge)
kiloampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMessage cls' kiloampereHoursSelector

-- | @+ ampereHours@
ampereHours :: IO (Id NSUnitElectricCharge)
ampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMessage cls' ampereHoursSelector

-- | @+ milliampereHours@
milliampereHours :: IO (Id NSUnitElectricCharge)
milliampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMessage cls' milliampereHoursSelector

-- | @+ microampereHours@
microampereHours :: IO (Id NSUnitElectricCharge)
microampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMessage cls' microampereHoursSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @coulombs@
coulombsSelector :: Selector '[] (Id NSUnitElectricCharge)
coulombsSelector = mkSelector "coulombs"

-- | @Selector@ for @megaampereHours@
megaampereHoursSelector :: Selector '[] (Id NSUnitElectricCharge)
megaampereHoursSelector = mkSelector "megaampereHours"

-- | @Selector@ for @kiloampereHours@
kiloampereHoursSelector :: Selector '[] (Id NSUnitElectricCharge)
kiloampereHoursSelector = mkSelector "kiloampereHours"

-- | @Selector@ for @ampereHours@
ampereHoursSelector :: Selector '[] (Id NSUnitElectricCharge)
ampereHoursSelector = mkSelector "ampereHours"

-- | @Selector@ for @milliampereHours@
milliampereHoursSelector :: Selector '[] (Id NSUnitElectricCharge)
milliampereHoursSelector = mkSelector "milliampereHours"

-- | @Selector@ for @microampereHours@
microampereHoursSelector :: Selector '[] (Id NSUnitElectricCharge)
microampereHoursSelector = mkSelector "microampereHours"

