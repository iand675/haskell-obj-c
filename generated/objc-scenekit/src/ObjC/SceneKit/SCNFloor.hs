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
  , reflectivitySelector
  , setReflectivitySelector
  , reflectionFalloffStartSelector
  , setReflectionFalloffStartSelector
  , reflectionFalloffEndSelector
  , setReflectionFalloffEndSelector
  , reflectionCategoryBitMaskSelector
  , setReflectionCategoryBitMaskSelector
  , widthSelector
  , setWidthSelector
  , lengthSelector
  , setLengthSelector
  , reflectionResolutionScaleFactorSelector
  , setReflectionResolutionScaleFactorSelector


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
    sendClassMsg cls' (mkSelector "floor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | reflectivity
--
-- Specifies the reflectivity of the floor. Animatable.
--
-- If the value is greater than zero then the surface will reflect other objects in the scene. The default value is 0.25.
--
-- ObjC selector: @- reflectivity@
reflectivity :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
reflectivity scnFloor  =
  sendMsg scnFloor (mkSelector "reflectivity") retCDouble []

-- | reflectivity
--
-- Specifies the reflectivity of the floor. Animatable.
--
-- If the value is greater than zero then the surface will reflect other objects in the scene. The default value is 0.25.
--
-- ObjC selector: @- setReflectivity:@
setReflectivity :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setReflectivity scnFloor  value =
  sendMsg scnFloor (mkSelector "setReflectivity:") retVoid [argCDouble (fromIntegral value)]

-- | reflectionFalloffStart
--
-- Specifies the distance from the floor where the falloff begins. Animatable.
--
-- The default value is 0.
--
-- ObjC selector: @- reflectionFalloffStart@
reflectionFalloffStart :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
reflectionFalloffStart scnFloor  =
  sendMsg scnFloor (mkSelector "reflectionFalloffStart") retCDouble []

-- | reflectionFalloffStart
--
-- Specifies the distance from the floor where the falloff begins. Animatable.
--
-- The default value is 0.
--
-- ObjC selector: @- setReflectionFalloffStart:@
setReflectionFalloffStart :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setReflectionFalloffStart scnFloor  value =
  sendMsg scnFloor (mkSelector "setReflectionFalloffStart:") retVoid [argCDouble (fromIntegral value)]

-- | reflectionFalloffEnd
--
-- Specifies the distance from the floor where the falloff finishes. Animatable.
--
-- If the value is 0 then there is no falloff. The default value is 0.
--
-- ObjC selector: @- reflectionFalloffEnd@
reflectionFalloffEnd :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
reflectionFalloffEnd scnFloor  =
  sendMsg scnFloor (mkSelector "reflectionFalloffEnd") retCDouble []

-- | reflectionFalloffEnd
--
-- Specifies the distance from the floor where the falloff finishes. Animatable.
--
-- If the value is 0 then there is no falloff. The default value is 0.
--
-- ObjC selector: @- setReflectionFalloffEnd:@
setReflectionFalloffEnd :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setReflectionFalloffEnd scnFloor  value =
  sendMsg scnFloor (mkSelector "setReflectionFalloffEnd:") retVoid [argCDouble (fromIntegral value)]

-- | reflectionCategoryBitMask
--
-- Determines the node categories to reflect. Defaults to all bits set.
--
-- ObjC selector: @- reflectionCategoryBitMask@
reflectionCategoryBitMask :: IsSCNFloor scnFloor => scnFloor -> IO CULong
reflectionCategoryBitMask scnFloor  =
  sendMsg scnFloor (mkSelector "reflectionCategoryBitMask") retCULong []

-- | reflectionCategoryBitMask
--
-- Determines the node categories to reflect. Defaults to all bits set.
--
-- ObjC selector: @- setReflectionCategoryBitMask:@
setReflectionCategoryBitMask :: IsSCNFloor scnFloor => scnFloor -> CULong -> IO ()
setReflectionCategoryBitMask scnFloor  value =
  sendMsg scnFloor (mkSelector "setReflectionCategoryBitMask:") retVoid [argCULong (fromIntegral value)]

-- | width
--
-- The floor extent along the X axis. Animatable.
--
-- If the value is equal to 0, the floor is infinite on the X axis. The default value is 0.
--
-- ObjC selector: @- width@
width :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
width scnFloor  =
  sendMsg scnFloor (mkSelector "width") retCDouble []

-- | width
--
-- The floor extent along the X axis. Animatable.
--
-- If the value is equal to 0, the floor is infinite on the X axis. The default value is 0.
--
-- ObjC selector: @- setWidth:@
setWidth :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setWidth scnFloor  value =
  sendMsg scnFloor (mkSelector "setWidth:") retVoid [argCDouble (fromIntegral value)]

-- | length
--
-- The floor extent along the Z axis. Animatable.
--
-- If the value is equal to 0, the floor is infinite on the Z axis. The default value is 0.
--
-- ObjC selector: @- length@
length_ :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
length_ scnFloor  =
  sendMsg scnFloor (mkSelector "length") retCDouble []

-- | length
--
-- The floor extent along the Z axis. Animatable.
--
-- If the value is equal to 0, the floor is infinite on the Z axis. The default value is 0.
--
-- ObjC selector: @- setLength:@
setLength :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setLength scnFloor  value =
  sendMsg scnFloor (mkSelector "setLength:") retVoid [argCDouble (fromIntegral value)]

-- | reflectionResolutionScaleFactor
--
-- Specifies the resolution scale factor of the buffer used to render the reflection.
--
-- Defaults to 1.0.
--
-- ObjC selector: @- reflectionResolutionScaleFactor@
reflectionResolutionScaleFactor :: IsSCNFloor scnFloor => scnFloor -> IO CDouble
reflectionResolutionScaleFactor scnFloor  =
  sendMsg scnFloor (mkSelector "reflectionResolutionScaleFactor") retCDouble []

-- | reflectionResolutionScaleFactor
--
-- Specifies the resolution scale factor of the buffer used to render the reflection.
--
-- Defaults to 1.0.
--
-- ObjC selector: @- setReflectionResolutionScaleFactor:@
setReflectionResolutionScaleFactor :: IsSCNFloor scnFloor => scnFloor -> CDouble -> IO ()
setReflectionResolutionScaleFactor scnFloor  value =
  sendMsg scnFloor (mkSelector "setReflectionResolutionScaleFactor:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @floor@
floorSelector :: Selector
floorSelector = mkSelector "floor"

-- | @Selector@ for @reflectivity@
reflectivitySelector :: Selector
reflectivitySelector = mkSelector "reflectivity"

-- | @Selector@ for @setReflectivity:@
setReflectivitySelector :: Selector
setReflectivitySelector = mkSelector "setReflectivity:"

-- | @Selector@ for @reflectionFalloffStart@
reflectionFalloffStartSelector :: Selector
reflectionFalloffStartSelector = mkSelector "reflectionFalloffStart"

-- | @Selector@ for @setReflectionFalloffStart:@
setReflectionFalloffStartSelector :: Selector
setReflectionFalloffStartSelector = mkSelector "setReflectionFalloffStart:"

-- | @Selector@ for @reflectionFalloffEnd@
reflectionFalloffEndSelector :: Selector
reflectionFalloffEndSelector = mkSelector "reflectionFalloffEnd"

-- | @Selector@ for @setReflectionFalloffEnd:@
setReflectionFalloffEndSelector :: Selector
setReflectionFalloffEndSelector = mkSelector "setReflectionFalloffEnd:"

-- | @Selector@ for @reflectionCategoryBitMask@
reflectionCategoryBitMaskSelector :: Selector
reflectionCategoryBitMaskSelector = mkSelector "reflectionCategoryBitMask"

-- | @Selector@ for @setReflectionCategoryBitMask:@
setReflectionCategoryBitMaskSelector :: Selector
setReflectionCategoryBitMaskSelector = mkSelector "setReflectionCategoryBitMask:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @setLength:@
setLengthSelector :: Selector
setLengthSelector = mkSelector "setLength:"

-- | @Selector@ for @reflectionResolutionScaleFactor@
reflectionResolutionScaleFactorSelector :: Selector
reflectionResolutionScaleFactorSelector = mkSelector "reflectionResolutionScaleFactor"

-- | @Selector@ for @setReflectionResolutionScaleFactor:@
setReflectionResolutionScaleFactorSelector :: Selector
setReflectionResolutionScaleFactorSelector = mkSelector "setReflectionResolutionScaleFactor:"

