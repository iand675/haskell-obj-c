{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNFloor
--
-- SCNFloor represents an infinite plane geometry.
--
-- Generated bindings for @SCNFloor@.
module ObjC.SceneKit.SCNFloor
  ( SCNFloor
  , IsSCNFloor(..)
  , floor
  , reflectivity
  , setReflectivity
  , reflectionFalloffStart
  , setReflectionFalloffStart
  , reflectionFalloffEnd
  , setReflectionFalloffEnd
  , reflectionCategoryBitMask
  , setReflectionCategoryBitMask
  , width
  , setWidth
  , length_
  , setLength
  , reflectionResolutionScaleFactor
  , setReflectionResolutionScaleFactor
  , floorSelector
  , lengthSelector
  , reflectionCategoryBitMaskSelector
  , reflectionFalloffEndSelector
  , reflectionFalloffStartSelector
  , reflectionResolutionScaleFactorSelector
  , reflectivitySelector
  , setLengthSelector
  , setReflectionCategoryBitMaskSelector
  , setReflectionFalloffEndSelector
  , setReflectionFalloffStartSelector
  , setReflectionResolutionScaleFactorSelector
  , setReflectivitySelector
  , setWidthSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | floor
--
-- Creates and returns a floor.
--
-- A floor is an infinite plane.
--
-- ObjC selector: @+ floor@
floor :: IO (Id SCNFloor)
floor  =
  do
    cls' <- getRequiredClass "SCNFloor"
    sendClassMessage cls' floorSelector

-- | reflectivity
--
-- Specifies the reflectivity of the floor. Animatable.
--
-- If the value is greater than zero then the surface will reflect other objects in the scene. The default value is 0.25.
--
-- ObjC selector: @- reflectivity@
reflectivity :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
reflectivity scnFloor =
  sendMessage scnFloor reflectivitySelector

-- | reflectivity
--
-- Specifies the reflectivity of the floor. Animatable.
--
-- If the value is greater than zero then the surface will reflect other objects in the scene. The default value is 0.25.
--
-- ObjC selector: @- setReflectivity:@
setReflectivity :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setReflectivity scnFloor value =
  sendMessage scnFloor setReflectivitySelector value

-- | reflectionFalloffStart
--
-- Specifies the distance from the floor where the falloff begins. Animatable.
--
-- The default value is 0.
--
-- ObjC selector: @- reflectionFalloffStart@
reflectionFalloffStart :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
reflectionFalloffStart scnFloor =
  sendMessage scnFloor reflectionFalloffStartSelector

-- | reflectionFalloffStart
--
-- Specifies the distance from the floor where the falloff begins. Animatable.
--
-- The default value is 0.
--
-- ObjC selector: @- setReflectionFalloffStart:@
setReflectionFalloffStart :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setReflectionFalloffStart scnFloor value =
  sendMessage scnFloor setReflectionFalloffStartSelector value

-- | reflectionFalloffEnd
--
-- Specifies the distance from the floor where the falloff finishes. Animatable.
--
-- If the value is 0 then there is no falloff. The default value is 0.
--
-- ObjC selector: @- reflectionFalloffEnd@
reflectionFalloffEnd :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
reflectionFalloffEnd scnFloor =
  sendMessage scnFloor reflectionFalloffEndSelector

-- | reflectionFalloffEnd
--
-- Specifies the distance from the floor where the falloff finishes. Animatable.
--
-- If the value is 0 then there is no falloff. The default value is 0.
--
-- ObjC selector: @- setReflectionFalloffEnd:@
setReflectionFalloffEnd :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setReflectionFalloffEnd scnFloor value =
  sendMessage scnFloor setReflectionFalloffEndSelector value

-- | reflectionCategoryBitMask
--
-- Determines the node categories to reflect. Defaults to all bits set.
--
-- ObjC selector: @- reflectionCategoryBitMask@
reflectionCategoryBitMask :: IsSCNFloor scnFloor => scnFloor -> IO CULong
reflectionCategoryBitMask scnFloor =
  sendMessage scnFloor reflectionCategoryBitMaskSelector

-- | reflectionCategoryBitMask
--
-- Determines the node categories to reflect. Defaults to all bits set.
--
-- ObjC selector: @- setReflectionCategoryBitMask:@
setReflectionCategoryBitMask :: IsSCNFloor scnFloor => scnFloor -> CULong -> IO ()
setReflectionCategoryBitMask scnFloor value =
  sendMessage scnFloor setReflectionCategoryBitMaskSelector value

-- | width
--
-- The floor extent along the X axis. Animatable.
--
-- If the value is equal to 0, the floor is infinite on the X axis. The default value is 0.
--
-- ObjC selector: @- width@
width :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
width scnFloor =
  sendMessage scnFloor widthSelector

-- | width
--
-- The floor extent along the X axis. Animatable.
--
-- If the value is equal to 0, the floor is infinite on the X axis. The default value is 0.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setWidth scnFloor value =
  sendMessage scnFloor setWidthSelector value

-- | length
--
-- The floor extent along the Z axis. Animatable.
--
-- If the value is equal to 0, the floor is infinite on the Z axis. The default value is 0.
--
-- ObjC selector: @- length@
length_ :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
length_ scnFloor =
  sendMessage scnFloor lengthSelector

-- | length
--
-- The floor extent along the Z axis. Animatable.
--
-- If the value is equal to 0, the floor is infinite on the Z axis. The default value is 0.
--
-- ObjC selector: @- setLength:@
setLength :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setLength scnFloor value =
  sendMessage scnFloor setLengthSelector value

-- | reflectionResolutionScaleFactor
--
-- Specifies the resolution scale factor of the buffer used to render the reflection.
--
-- Defaults to 1.0.
--
-- ObjC selector: @- reflectionResolutionScaleFactor@
reflectionResolutionScaleFactor :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
reflectionResolutionScaleFactor scnFloor =
  sendMessage scnFloor reflectionResolutionScaleFactorSelector

-- | reflectionResolutionScaleFactor
--
-- Specifies the resolution scale factor of the buffer used to render the reflection.
--
-- Defaults to 1.0.
--
-- ObjC selector: @- setReflectionResolutionScaleFactor:@
setReflectionResolutionScaleFactor :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setReflectionResolutionScaleFactor scnFloor value =
  sendMessage scnFloor setReflectionResolutionScaleFactorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @floor@
floorSelector :: Selector '[] (Id SCNFloor)
floorSelector = mkSelector "floor"

-- | @Selector@ for @reflectivity@
reflectivitySelector :: Selector '[] CDouble
reflectivitySelector = mkSelector "reflectivity"

-- | @Selector@ for @setReflectivity:@
setReflectivitySelector :: Selector '[CDouble] ()
setReflectivitySelector = mkSelector "setReflectivity:"

-- | @Selector@ for @reflectionFalloffStart@
reflectionFalloffStartSelector :: Selector '[] CDouble
reflectionFalloffStartSelector = mkSelector "reflectionFalloffStart"

-- | @Selector@ for @setReflectionFalloffStart:@
setReflectionFalloffStartSelector :: Selector '[CDouble] ()
setReflectionFalloffStartSelector = mkSelector "setReflectionFalloffStart:"

-- | @Selector@ for @reflectionFalloffEnd@
reflectionFalloffEndSelector :: Selector '[] CDouble
reflectionFalloffEndSelector = mkSelector "reflectionFalloffEnd"

-- | @Selector@ for @setReflectionFalloffEnd:@
setReflectionFalloffEndSelector :: Selector '[CDouble] ()
setReflectionFalloffEndSelector = mkSelector "setReflectionFalloffEnd:"

-- | @Selector@ for @reflectionCategoryBitMask@
reflectionCategoryBitMaskSelector :: Selector '[] CULong
reflectionCategoryBitMaskSelector = mkSelector "reflectionCategoryBitMask"

-- | @Selector@ for @setReflectionCategoryBitMask:@
setReflectionCategoryBitMaskSelector :: Selector '[CULong] ()
setReflectionCategoryBitMaskSelector = mkSelector "setReflectionCategoryBitMask:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CDouble
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CDouble] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CDouble
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector '[CDouble] ()
setLengthSelector = mkSelector "setLength:"

-- | @Selector@ for @reflectionResolutionScaleFactor@
reflectionResolutionScaleFactorSelector :: Selector '[] CDouble
reflectionResolutionScaleFactorSelector = mkSelector "reflectionResolutionScaleFactor"

-- | @Selector@ for @setReflectionResolutionScaleFactor:@
setReflectionResolutionScaleFactorSelector :: Selector '[CDouble] ()
setReflectionResolutionScaleFactorSelector = mkSelector "setReflectionResolutionScaleFactor:"

