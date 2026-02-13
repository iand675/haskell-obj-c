{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitElectricCurrent@.
module ObjC.Foundation.NSUnitElectricCurrent
  ( NSUnitElectricCurrent
  , IsNSUnitElectricCurrent(..)
  , megaamperes
  , kiloamperes
  , amperes
  , milliamperes
  , microamperes
  , amperesSelector
  , kiloamperesSelector
  , megaamperesSelector
  , microamperesSelector
  , milliamperesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ megaamperes@
megaamperes :: IO (Id NSUnitElectricCurrent)
megaamperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMessage cls' megaamperesSelector

-- | @+ kiloamperes@
kiloamperes :: IO (Id NSUnitElectricCurrent)
kiloamperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMessage cls' kiloamperesSelector

-- | @+ amperes@
amperes :: IO (Id NSUnitElectricCurrent)
amperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMessage cls' amperesSelector

-- | @+ milliamperes@
milliamperes :: IO (Id NSUnitElectricCurrent)
milliamperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMessage cls' milliamperesSelector

-- | @+ microamperes@
microamperes :: IO (Id NSUnitElectricCurrent)
microamperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMessage cls' microamperesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megaamperes@
megaamperesSelector :: Selector '[] (Id NSUnitElectricCurrent)
megaamperesSelector = mkSelector "megaamperes"

-- | @Selector@ for @kiloamperes@
kiloamperesSelector :: Selector '[] (Id NSUnitElectricCurrent)
kiloamperesSelector = mkSelector "kiloamperes"

-- | @Selector@ for @amperes@
amperesSelector :: Selector '[] (Id NSUnitElectricCurrent)
amperesSelector = mkSelector "amperes"

-- | @Selector@ for @milliamperes@
milliamperesSelector :: Selector '[] (Id NSUnitElectricCurrent)
milliamperesSelector = mkSelector "milliamperes"

-- | @Selector@ for @microamperes@
microamperesSelector :: Selector '[] (Id NSUnitElectricCurrent)
microamperesSelector = mkSelector "microamperes"

