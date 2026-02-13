{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKContactsLensSpecification
--
-- An object subclass representing lens specification for contacts
--
-- Generated bindings for @HKContactsLensSpecification@.
module ObjC.HealthKit.HKContactsLensSpecification
  ( HKContactsLensSpecification
  , IsHKContactsLensSpecification(..)
  , initWithSphere_cylinder_axis_addPower_baseCurve_diameter
  , init_
  , new
  , baseCurve
  , diameter
  , baseCurveSelector
  , diameterSelector
  , initSelector
  , initWithSphere_cylinder_axis_addPower_baseCurve_diameterSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSphere:cylinder:axis:addPower:baseCurve:diameter
--
-- @sphere@ — The lens power to correct nearsightedness or farsightedness
--
-- @cylinder@ — The lens power required to correct astigmatism
--
-- @axis@ — The angle along which cylindrical power should be positioned to correct astigmatism
--
-- @addPower@ — The power adjustment applied to a multifocal lens to correct presbyopia
--
-- @baseCurve@ — The curvature of the back surface of the lens
--
-- @diameter@ — The width of the lens from edge to edge
--
-- ObjC selector: @- initWithSphere:cylinder:axis:addPower:baseCurve:diameter:@
initWithSphere_cylinder_axis_addPower_baseCurve_diameter :: (IsHKContactsLensSpecification hkContactsLensSpecification, IsHKQuantity sphere, IsHKQuantity cylinder, IsHKQuantity axis, IsHKQuantity addPower, IsHKQuantity baseCurve, IsHKQuantity diameter) => hkContactsLensSpecification -> sphere -> cylinder -> axis -> addPower -> baseCurve -> diameter -> IO (Id HKContactsLensSpecification)
initWithSphere_cylinder_axis_addPower_baseCurve_diameter hkContactsLensSpecification sphere cylinder axis addPower baseCurve diameter =
  sendOwnedMessage hkContactsLensSpecification initWithSphere_cylinder_axis_addPower_baseCurve_diameterSelector (toHKQuantity sphere) (toHKQuantity cylinder) (toHKQuantity axis) (toHKQuantity addPower) (toHKQuantity baseCurve) (toHKQuantity diameter)

-- | @- init@
init_ :: IsHKContactsLensSpecification hkContactsLensSpecification => hkContactsLensSpecification -> IO (Id HKContactsLensSpecification)
init_ hkContactsLensSpecification =
  sendOwnedMessage hkContactsLensSpecification initSelector

-- | @+ new@
new :: IO (Id HKContactsLensSpecification)
new  =
  do
    cls' <- getRequiredClass "HKContactsLensSpecification"
    sendOwnedClassMessage cls' newSelector

-- | baseCurve
--
-- The curvature of the back surface of the lens (measured in mm)
--
-- ObjC selector: @- baseCurve@
baseCurve :: IsHKContactsLensSpecification hkContactsLensSpecification => hkContactsLensSpecification -> IO (Id HKQuantity)
baseCurve hkContactsLensSpecification =
  sendMessage hkContactsLensSpecification baseCurveSelector

-- | diameter
--
-- The width of the lens from edge to edge (measured in mm)
--
-- ObjC selector: @- diameter@
diameter :: IsHKContactsLensSpecification hkContactsLensSpecification => hkContactsLensSpecification -> IO (Id HKQuantity)
diameter hkContactsLensSpecification =
  sendMessage hkContactsLensSpecification diameterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSphere:cylinder:axis:addPower:baseCurve:diameter:@
initWithSphere_cylinder_axis_addPower_baseCurve_diameterSelector :: Selector '[Id HKQuantity, Id HKQuantity, Id HKQuantity, Id HKQuantity, Id HKQuantity, Id HKQuantity] (Id HKContactsLensSpecification)
initWithSphere_cylinder_axis_addPower_baseCurve_diameterSelector = mkSelector "initWithSphere:cylinder:axis:addPower:baseCurve:diameter:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKContactsLensSpecification)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKContactsLensSpecification)
newSelector = mkSelector "new"

-- | @Selector@ for @baseCurve@
baseCurveSelector :: Selector '[] (Id HKQuantity)
baseCurveSelector = mkSelector "baseCurve"

-- | @Selector@ for @diameter@
diameterSelector :: Selector '[] (Id HKQuantity)
diameterSelector = mkSelector "diameter"

