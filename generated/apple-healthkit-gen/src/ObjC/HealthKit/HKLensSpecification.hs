{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKLensSpecification
--
-- An object subclass representing common lens specification
--
-- Generated bindings for @HKLensSpecification@.
module ObjC.HealthKit.HKLensSpecification
  ( HKLensSpecification
  , IsHKLensSpecification(..)
  , init_
  , new
  , sphere
  , cylinder
  , axis
  , addPower
  , addPowerSelector
  , axisSelector
  , cylinderSelector
  , initSelector
  , newSelector
  , sphereSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKLensSpecification)
init_ hkLensSpecification =
  sendOwnedMessage hkLensSpecification initSelector

-- | @+ new@
new :: IO (Id HKLensSpecification)
new  =
  do
    cls' <- getRequiredClass "HKLensSpecification"
    sendOwnedClassMessage cls' newSelector

-- | sphere
--
-- The lens power to correct nearsightedness or farsightedness. (-) means nearsighted while (+) farsighted.
--
-- ObjC selector: @- sphere@
sphere :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKQuantity)
sphere hkLensSpecification =
  sendMessage hkLensSpecification sphereSelector

-- | cylinder
--
-- The lens power required to correct astigmatism. Can be positive or negative.
--
-- ObjC selector: @- cylinder@
cylinder :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKQuantity)
cylinder hkLensSpecification =
  sendMessage hkLensSpecification cylinderSelector

-- | axis
--
-- The angle along which cylindrical power should be positioned to correct astigmatism
--
-- ObjC selector: @- axis@
axis :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKQuantity)
axis hkLensSpecification =
  sendMessage hkLensSpecification axisSelector

-- | addPower
--
-- The power adjustment applied to a multifocal lens to correct presbyopia
--
-- ObjC selector: @- addPower@
addPower :: IsHKLensSpecification hkLensSpecification => hkLensSpecification -> IO (Id HKQuantity)
addPower hkLensSpecification =
  sendMessage hkLensSpecification addPowerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKLensSpecification)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKLensSpecification)
newSelector = mkSelector "new"

-- | @Selector@ for @sphere@
sphereSelector :: Selector '[] (Id HKQuantity)
sphereSelector = mkSelector "sphere"

-- | @Selector@ for @cylinder@
cylinderSelector :: Selector '[] (Id HKQuantity)
cylinderSelector = mkSelector "cylinder"

-- | @Selector@ for @axis@
axisSelector :: Selector '[] (Id HKQuantity)
axisSelector = mkSelector "axis"

-- | @Selector@ for @addPower@
addPowerSelector :: Selector '[] (Id HKQuantity)
addPowerSelector = mkSelector "addPower"

