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
  , megavoltsSelector
  , kilovoltsSelector
  , voltsSelector
  , millivoltsSelector
  , microvoltsSelector


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

-- | @+ megavolts@
megavolts :: IO (Id NSUnitElectricPotentialDifference)
megavolts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMsg cls' (mkSelector "megavolts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilovolts@
kilovolts :: IO (Id NSUnitElectricPotentialDifference)
kilovolts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMsg cls' (mkSelector "kilovolts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ volts@
volts :: IO (Id NSUnitElectricPotentialDifference)
volts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMsg cls' (mkSelector "volts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ millivolts@
millivolts :: IO (Id NSUnitElectricPotentialDifference)
millivolts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMsg cls' (mkSelector "millivolts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ microvolts@
microvolts :: IO (Id NSUnitElectricPotentialDifference)
microvolts  =
  do
    cls' <- getRequiredClass "NSUnitElectricPotentialDifference"
    sendClassMsg cls' (mkSelector "microvolts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megavolts@
megavoltsSelector :: Selector
megavoltsSelector = mkSelector "megavolts"

-- | @Selector@ for @kilovolts@
kilovoltsSelector :: Selector
kilovoltsSelector = mkSelector "kilovolts"

-- | @Selector@ for @volts@
voltsSelector :: Selector
voltsSelector = mkSelector "volts"

-- | @Selector@ for @millivolts@
millivoltsSelector :: Selector
millivoltsSelector = mkSelector "millivolts"

-- | @Selector@ for @microvolts@
microvoltsSelector :: Selector
microvoltsSelector = mkSelector "microvolts"

