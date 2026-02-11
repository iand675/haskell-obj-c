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
  , coulombsSelector
  , megaampereHoursSelector
  , kiloampereHoursSelector
  , ampereHoursSelector
  , milliampereHoursSelector
  , microampereHoursSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ coulombs@
coulombs :: IO (Id NSUnitElectricCharge)
coulombs  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMsg cls' (mkSelector "coulombs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ megaampereHours@
megaampereHours :: IO (Id NSUnitElectricCharge)
megaampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMsg cls' (mkSelector "megaampereHours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kiloampereHours@
kiloampereHours :: IO (Id NSUnitElectricCharge)
kiloampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMsg cls' (mkSelector "kiloampereHours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ampereHours@
ampereHours :: IO (Id NSUnitElectricCharge)
ampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMsg cls' (mkSelector "ampereHours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milliampereHours@
milliampereHours :: IO (Id NSUnitElectricCharge)
milliampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMsg cls' (mkSelector "milliampereHours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ microampereHours@
microampereHours :: IO (Id NSUnitElectricCharge)
microampereHours  =
  do
    cls' <- getRequiredClass "NSUnitElectricCharge"
    sendClassMsg cls' (mkSelector "microampereHours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @coulombs@
coulombsSelector :: Selector
coulombsSelector = mkSelector "coulombs"

-- | @Selector@ for @megaampereHours@
megaampereHoursSelector :: Selector
megaampereHoursSelector = mkSelector "megaampereHours"

-- | @Selector@ for @kiloampereHours@
kiloampereHoursSelector :: Selector
kiloampereHoursSelector = mkSelector "kiloampereHours"

-- | @Selector@ for @ampereHours@
ampereHoursSelector :: Selector
ampereHoursSelector = mkSelector "ampereHours"

-- | @Selector@ for @milliampereHours@
milliampereHoursSelector :: Selector
milliampereHoursSelector = mkSelector "milliampereHours"

-- | @Selector@ for @microampereHours@
microampereHoursSelector :: Selector
microampereHoursSelector = mkSelector "microampereHours"

