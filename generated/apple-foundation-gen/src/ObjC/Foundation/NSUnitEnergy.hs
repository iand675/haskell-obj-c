{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitEnergy@.
module ObjC.Foundation.NSUnitEnergy
  ( NSUnitEnergy
  , IsNSUnitEnergy(..)
  , kilojoules
  , joules
  , kilocalories
  , calories
  , kilowattHours
  , caloriesSelector
  , joulesSelector
  , kilocaloriesSelector
  , kilojoulesSelector
  , kilowattHoursSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ kilojoules@
kilojoules :: IO (Id NSUnitEnergy)
kilojoules  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMessage cls' kilojoulesSelector

-- | @+ joules@
joules :: IO (Id NSUnitEnergy)
joules  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMessage cls' joulesSelector

-- | @+ kilocalories@
kilocalories :: IO (Id NSUnitEnergy)
kilocalories  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMessage cls' kilocaloriesSelector

-- | @+ calories@
calories :: IO (Id NSUnitEnergy)
calories  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMessage cls' caloriesSelector

-- | @+ kilowattHours@
kilowattHours :: IO (Id NSUnitEnergy)
kilowattHours  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMessage cls' kilowattHoursSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kilojoules@
kilojoulesSelector :: Selector '[] (Id NSUnitEnergy)
kilojoulesSelector = mkSelector "kilojoules"

-- | @Selector@ for @joules@
joulesSelector :: Selector '[] (Id NSUnitEnergy)
joulesSelector = mkSelector "joules"

-- | @Selector@ for @kilocalories@
kilocaloriesSelector :: Selector '[] (Id NSUnitEnergy)
kilocaloriesSelector = mkSelector "kilocalories"

-- | @Selector@ for @calories@
caloriesSelector :: Selector '[] (Id NSUnitEnergy)
caloriesSelector = mkSelector "calories"

-- | @Selector@ for @kilowattHours@
kilowattHoursSelector :: Selector '[] (Id NSUnitEnergy)
kilowattHoursSelector = mkSelector "kilowattHours"

