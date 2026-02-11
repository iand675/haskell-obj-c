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
  , kilojoulesSelector
  , joulesSelector
  , kilocaloriesSelector
  , caloriesSelector
  , kilowattHoursSelector


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

-- | @+ kilojoules@
kilojoules :: IO (Id NSUnitEnergy)
kilojoules  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMsg cls' (mkSelector "kilojoules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ joules@
joules :: IO (Id NSUnitEnergy)
joules  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMsg cls' (mkSelector "joules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilocalories@
kilocalories :: IO (Id NSUnitEnergy)
kilocalories  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMsg cls' (mkSelector "kilocalories") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ calories@
calories :: IO (Id NSUnitEnergy)
calories  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMsg cls' (mkSelector "calories") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilowattHours@
kilowattHours :: IO (Id NSUnitEnergy)
kilowattHours  =
  do
    cls' <- getRequiredClass "NSUnitEnergy"
    sendClassMsg cls' (mkSelector "kilowattHours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kilojoules@
kilojoulesSelector :: Selector
kilojoulesSelector = mkSelector "kilojoules"

-- | @Selector@ for @joules@
joulesSelector :: Selector
joulesSelector = mkSelector "joules"

-- | @Selector@ for @kilocalories@
kilocaloriesSelector :: Selector
kilocaloriesSelector = mkSelector "kilocalories"

-- | @Selector@ for @calories@
caloriesSelector :: Selector
caloriesSelector = mkSelector "calories"

-- | @Selector@ for @kilowattHours@
kilowattHoursSelector :: Selector
kilowattHoursSelector = mkSelector "kilowattHours"

