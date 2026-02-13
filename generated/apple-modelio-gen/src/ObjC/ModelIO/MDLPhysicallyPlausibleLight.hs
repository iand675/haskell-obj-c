{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLPhysicallyPlausibleLight
--
-- A light with characteristics representing plausible real world lights
--
-- color The color of the light.
--
-- lumens The brightness of the light.
--
-- innerConeAngle Within this cone, light is at maximum brightness. Units are degrees.
--
-- outerConeAngle Between the inner cone angle and the outer, light
--
-- quadratically attenuates to zero.
--
-- attenuationStartDistance. Within the attenuation start distance, the
--
-- light is maximally bright.
--
-- attenuationEndDistance. Beyond this distance, there is no light.
--
-- Generated bindings for @MDLPhysicallyPlausibleLight@.
module ObjC.ModelIO.MDLPhysicallyPlausibleLight
  ( MDLPhysicallyPlausibleLight
  , IsMDLPhysicallyPlausibleLight(..)
  , setColorByTemperature
  , color
  , setColor
  , lumens
  , setLumens
  , innerConeAngle
  , setInnerConeAngle
  , outerConeAngle
  , setOuterConeAngle
  , attenuationStartDistance
  , setAttenuationStartDistance
  , attenuationEndDistance
  , setAttenuationEndDistance
  , attenuationEndDistanceSelector
  , attenuationStartDistanceSelector
  , colorSelector
  , innerConeAngleSelector
  , lumensSelector
  , outerConeAngleSelector
  , setAttenuationEndDistanceSelector
  , setAttenuationStartDistanceSelector
  , setColorByTemperatureSelector
  , setColorSelector
  , setInnerConeAngleSelector
  , setLumensSelector
  , setOuterConeAngleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Light color specified by color temperature, in degrees Kelvin
--
-- default color is 6500K, cool daylight.
--
-- ObjC selector: @- setColorByTemperature:@
setColorByTemperature :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setColorByTemperature mdlPhysicallyPlausibleLight temperature =
  sendMessage mdlPhysicallyPlausibleLight setColorByTemperatureSelector temperature

-- | @- color@
color :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO (Ptr ())
color mdlPhysicallyPlausibleLight =
  sendMessage mdlPhysicallyPlausibleLight colorSelector

-- | @- setColor:@
setColor :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> Ptr () -> IO ()
setColor mdlPhysicallyPlausibleLight value =
  sendMessage mdlPhysicallyPlausibleLight setColorSelector value

-- | @- lumens@
lumens :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
lumens mdlPhysicallyPlausibleLight =
  sendMessage mdlPhysicallyPlausibleLight lumensSelector

-- | @- setLumens:@
setLumens :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setLumens mdlPhysicallyPlausibleLight value =
  sendMessage mdlPhysicallyPlausibleLight setLumensSelector value

-- | @- innerConeAngle@
innerConeAngle :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
innerConeAngle mdlPhysicallyPlausibleLight =
  sendMessage mdlPhysicallyPlausibleLight innerConeAngleSelector

-- | @- setInnerConeAngle:@
setInnerConeAngle :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setInnerConeAngle mdlPhysicallyPlausibleLight value =
  sendMessage mdlPhysicallyPlausibleLight setInnerConeAngleSelector value

-- | @- outerConeAngle@
outerConeAngle :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
outerConeAngle mdlPhysicallyPlausibleLight =
  sendMessage mdlPhysicallyPlausibleLight outerConeAngleSelector

-- | @- setOuterConeAngle:@
setOuterConeAngle :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setOuterConeAngle mdlPhysicallyPlausibleLight value =
  sendMessage mdlPhysicallyPlausibleLight setOuterConeAngleSelector value

-- | @- attenuationStartDistance@
attenuationStartDistance :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
attenuationStartDistance mdlPhysicallyPlausibleLight =
  sendMessage mdlPhysicallyPlausibleLight attenuationStartDistanceSelector

-- | @- setAttenuationStartDistance:@
setAttenuationStartDistance :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setAttenuationStartDistance mdlPhysicallyPlausibleLight value =
  sendMessage mdlPhysicallyPlausibleLight setAttenuationStartDistanceSelector value

-- | @- attenuationEndDistance@
attenuationEndDistance :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
attenuationEndDistance mdlPhysicallyPlausibleLight =
  sendMessage mdlPhysicallyPlausibleLight attenuationEndDistanceSelector

-- | @- setAttenuationEndDistance:@
setAttenuationEndDistance :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setAttenuationEndDistance mdlPhysicallyPlausibleLight value =
  sendMessage mdlPhysicallyPlausibleLight setAttenuationEndDistanceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setColorByTemperature:@
setColorByTemperatureSelector :: Selector '[CFloat] ()
setColorByTemperatureSelector = mkSelector "setColorByTemperature:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Ptr ())
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Ptr ()] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @lumens@
lumensSelector :: Selector '[] CFloat
lumensSelector = mkSelector "lumens"

-- | @Selector@ for @setLumens:@
setLumensSelector :: Selector '[CFloat] ()
setLumensSelector = mkSelector "setLumens:"

-- | @Selector@ for @innerConeAngle@
innerConeAngleSelector :: Selector '[] CFloat
innerConeAngleSelector = mkSelector "innerConeAngle"

-- | @Selector@ for @setInnerConeAngle:@
setInnerConeAngleSelector :: Selector '[CFloat] ()
setInnerConeAngleSelector = mkSelector "setInnerConeAngle:"

-- | @Selector@ for @outerConeAngle@
outerConeAngleSelector :: Selector '[] CFloat
outerConeAngleSelector = mkSelector "outerConeAngle"

-- | @Selector@ for @setOuterConeAngle:@
setOuterConeAngleSelector :: Selector '[CFloat] ()
setOuterConeAngleSelector = mkSelector "setOuterConeAngle:"

-- | @Selector@ for @attenuationStartDistance@
attenuationStartDistanceSelector :: Selector '[] CFloat
attenuationStartDistanceSelector = mkSelector "attenuationStartDistance"

-- | @Selector@ for @setAttenuationStartDistance:@
setAttenuationStartDistanceSelector :: Selector '[CFloat] ()
setAttenuationStartDistanceSelector = mkSelector "setAttenuationStartDistance:"

-- | @Selector@ for @attenuationEndDistance@
attenuationEndDistanceSelector :: Selector '[] CFloat
attenuationEndDistanceSelector = mkSelector "attenuationEndDistance"

-- | @Selector@ for @setAttenuationEndDistance:@
setAttenuationEndDistanceSelector :: Selector '[CFloat] ()
setAttenuationEndDistanceSelector = mkSelector "setAttenuationEndDistance:"

