{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKGlassesLensSpecification
--
-- An object subclass representing lens specification for glasses
--
-- Generated bindings for @HKGlassesLensSpecification@.
module ObjC.HealthKit.HKGlassesLensSpecification
  ( HKGlassesLensSpecification
  , IsHKGlassesLensSpecification(..)
  , initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistance
  , init_
  , new
  , vertexDistance
  , prism
  , farPupillaryDistance
  , nearPupillaryDistance
  , farPupillaryDistanceSelector
  , initSelector
  , initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistanceSelector
  , nearPupillaryDistanceSelector
  , newSelector
  , prismSelector
  , vertexDistanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance
--
-- @sphere@ — The lens power to correct nearsightedness or farsightedness
--
-- @cylinder@ — The lens power required to correct astigmatism
--
-- @axis@ — The angle along which cylindrical power should be positioned to correct astigmatism
--
-- @addPower@ — The power adjustment applied to a multifocal lens to correct presbyopia
--
-- @vertexDistance@ — The distance between the back of the eyeglass lens and the eye
--
-- @prism@ — The object encapsulating the prism fields
--
-- @farPupillaryDistance@ — The distance from each pupil to the center of the nose (measured in mm) when looking at a far target.                                        Can be described as combined or individual value. For distance prescriptions, the pupillary distance will be a far value.
--
-- @nearPupillaryDistance@ — The distance from each pupil to the center of the nose (measured in mm) when looking at a near target.                                        Can be described as combined or individual value. For near prescriptions, the pupillary distance will be a near value.
--
-- ObjC selector: @- initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance:@
initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistance :: (IsHKGlassesLensSpecification hkGlassesLensSpecification, IsHKQuantity sphere, IsHKQuantity cylinder, IsHKQuantity axis, IsHKQuantity addPower, IsHKQuantity vertexDistance, IsHKVisionPrism prism, IsHKQuantity farPupillaryDistance, IsHKQuantity nearPupillaryDistance) => hkGlassesLensSpecification -> sphere -> cylinder -> axis -> addPower -> vertexDistance -> prism -> farPupillaryDistance -> nearPupillaryDistance -> IO (Id HKGlassesLensSpecification)
initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistance hkGlassesLensSpecification sphere cylinder axis addPower vertexDistance prism farPupillaryDistance nearPupillaryDistance =
  sendOwnedMessage hkGlassesLensSpecification initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistanceSelector (toHKQuantity sphere) (toHKQuantity cylinder) (toHKQuantity axis) (toHKQuantity addPower) (toHKQuantity vertexDistance) (toHKVisionPrism prism) (toHKQuantity farPupillaryDistance) (toHKQuantity nearPupillaryDistance)

-- | @- init@
init_ :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKGlassesLensSpecification)
init_ hkGlassesLensSpecification =
  sendOwnedMessage hkGlassesLensSpecification initSelector

-- | @+ new@
new :: IO (Id HKGlassesLensSpecification)
new  =
  do
    cls' <- getRequiredClass "HKGlassesLensSpecification"
    sendOwnedClassMessage cls' newSelector

-- | vertexDistance
--
-- The distance between the back of the eyeglass lens and the eye (measured in mm)
--
-- ObjC selector: @- vertexDistance@
vertexDistance :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKQuantity)
vertexDistance hkGlassesLensSpecification =
  sendMessage hkGlassesLensSpecification vertexDistanceSelector

-- | prism
--
-- The object encapsulating the prism fields
--
-- ObjC selector: @- prism@
prism :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKVisionPrism)
prism hkGlassesLensSpecification =
  sendMessage hkGlassesLensSpecification prismSelector

-- | farPupillaryDistance
--
-- The distance from each pupil to the center of the nose (measured in mm) when looking at a far target.                Can be described as combined or individual value. For distance prescriptions, the pupillary distance will be a far value.
--
-- ObjC selector: @- farPupillaryDistance@
farPupillaryDistance :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKQuantity)
farPupillaryDistance hkGlassesLensSpecification =
  sendMessage hkGlassesLensSpecification farPupillaryDistanceSelector

-- | nearPupillaryDistance
--
-- The distance from each pupil to the center of the nose (measured in mm) when looking at a near target.                Can be described as combined or individual value. For near prescriptions, the pupillary distance will be a near value.
--
-- ObjC selector: @- nearPupillaryDistance@
nearPupillaryDistance :: IsHKGlassesLensSpecification hkGlassesLensSpecification => hkGlassesLensSpecification -> IO (Id HKQuantity)
nearPupillaryDistance hkGlassesLensSpecification =
  sendMessage hkGlassesLensSpecification nearPupillaryDistanceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance:@
initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistanceSelector :: Selector '[Id HKQuantity, Id HKQuantity, Id HKQuantity, Id HKQuantity, Id HKQuantity, Id HKVisionPrism, Id HKQuantity, Id HKQuantity] (Id HKGlassesLensSpecification)
initWithSphere_cylinder_axis_addPower_vertexDistance_prism_farPupillaryDistance_nearPupillaryDistanceSelector = mkSelector "initWithSphere:cylinder:axis:addPower:vertexDistance:prism:farPupillaryDistance:nearPupillaryDistance:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKGlassesLensSpecification)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKGlassesLensSpecification)
newSelector = mkSelector "new"

-- | @Selector@ for @vertexDistance@
vertexDistanceSelector :: Selector '[] (Id HKQuantity)
vertexDistanceSelector = mkSelector "vertexDistance"

-- | @Selector@ for @prism@
prismSelector :: Selector '[] (Id HKVisionPrism)
prismSelector = mkSelector "prism"

-- | @Selector@ for @farPupillaryDistance@
farPupillaryDistanceSelector :: Selector '[] (Id HKQuantity)
farPupillaryDistanceSelector = mkSelector "farPupillaryDistance"

-- | @Selector@ for @nearPupillaryDistance@
nearPupillaryDistanceSelector :: Selector '[] (Id HKQuantity)
nearPupillaryDistanceSelector = mkSelector "nearPupillaryDistance"

