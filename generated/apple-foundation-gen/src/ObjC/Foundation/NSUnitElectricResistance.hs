{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitElectricResistance@.
module ObjC.Foundation.NSUnitElectricResistance
  ( NSUnitElectricResistance
  , IsNSUnitElectricResistance(..)
  , megaohms
  , kiloohms
  , ohms
  , milliohms
  , microohms
  , kiloohmsSelector
  , megaohmsSelector
  , microohmsSelector
  , milliohmsSelector
  , ohmsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ megaohms@
megaohms :: IO (Id NSUnitElectricResistance)
megaohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMessage cls' megaohmsSelector

-- | @+ kiloohms@
kiloohms :: IO (Id NSUnitElectricResistance)
kiloohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMessage cls' kiloohmsSelector

-- | @+ ohms@
ohms :: IO (Id NSUnitElectricResistance)
ohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMessage cls' ohmsSelector

-- | @+ milliohms@
milliohms :: IO (Id NSUnitElectricResistance)
milliohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMessage cls' milliohmsSelector

-- | @+ microohms@
microohms :: IO (Id NSUnitElectricResistance)
microohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMessage cls' microohmsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megaohms@
megaohmsSelector :: Selector '[] (Id NSUnitElectricResistance)
megaohmsSelector = mkSelector "megaohms"

-- | @Selector@ for @kiloohms@
kiloohmsSelector :: Selector '[] (Id NSUnitElectricResistance)
kiloohmsSelector = mkSelector "kiloohms"

-- | @Selector@ for @ohms@
ohmsSelector :: Selector '[] (Id NSUnitElectricResistance)
ohmsSelector = mkSelector "ohms"

-- | @Selector@ for @milliohms@
milliohmsSelector :: Selector '[] (Id NSUnitElectricResistance)
milliohmsSelector = mkSelector "milliohms"

-- | @Selector@ for @microohms@
microohmsSelector :: Selector '[] (Id NSUnitElectricResistance)
microohmsSelector = mkSelector "microohms"

