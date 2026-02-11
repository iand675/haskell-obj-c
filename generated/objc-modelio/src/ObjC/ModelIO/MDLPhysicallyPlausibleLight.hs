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
  , setColorByTemperatureSelector
  , colorSelector
  , setColorSelector
  , lumensSelector
  , setLumensSelector
  , innerConeAngleSelector
  , setInnerConeAngleSelector
  , outerConeAngleSelector
  , setOuterConeAngleSelector
  , attenuationStartDistanceSelector
  , setAttenuationStartDistanceSelector
  , attenuationEndDistanceSelector
  , setAttenuationEndDistanceSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Light color specified by color temperature, in degrees Kelvin
--
-- default color is 6500K, cool daylight.
--
-- ObjC selector: @- setColorByTemperature:@
setColorByTemperature :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setColorByTemperature mdlPhysicallyPlausibleLight  temperature =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "setColorByTemperature:") retVoid [argCFloat (fromIntegral temperature)]

-- | @- color@
color :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO (Ptr ())
color mdlPhysicallyPlausibleLight  =
  fmap castPtr $ sendMsg mdlPhysicallyPlausibleLight (mkSelector "color") (retPtr retVoid) []

-- | @- setColor:@
setColor :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> Ptr () -> IO ()
setColor mdlPhysicallyPlausibleLight  value =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "setColor:") retVoid [argPtr value]

-- | @- lumens@
lumens :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
lumens mdlPhysicallyPlausibleLight  =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "lumens") retCFloat []

-- | @- setLumens:@
setLumens :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setLumens mdlPhysicallyPlausibleLight  value =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "setLumens:") retVoid [argCFloat (fromIntegral value)]

-- | @- innerConeAngle@
innerConeAngle :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
innerConeAngle mdlPhysicallyPlausibleLight  =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "innerConeAngle") retCFloat []

-- | @- setInnerConeAngle:@
setInnerConeAngle :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setInnerConeAngle mdlPhysicallyPlausibleLight  value =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "setInnerConeAngle:") retVoid [argCFloat (fromIntegral value)]

-- | @- outerConeAngle@
outerConeAngle :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
outerConeAngle mdlPhysicallyPlausibleLight  =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "outerConeAngle") retCFloat []

-- | @- setOuterConeAngle:@
setOuterConeAngle :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setOuterConeAngle mdlPhysicallyPlausibleLight  value =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "setOuterConeAngle:") retVoid [argCFloat (fromIntegral value)]

-- | @- attenuationStartDistance@
attenuationStartDistance :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
attenuationStartDistance mdlPhysicallyPlausibleLight  =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "attenuationStartDistance") retCFloat []

-- | @- setAttenuationStartDistance:@
setAttenuationStartDistance :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setAttenuationStartDistance mdlPhysicallyPlausibleLight  value =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "setAttenuationStartDistance:") retVoid [argCFloat (fromIntegral value)]

-- | @- attenuationEndDistance@
attenuationEndDistance :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> IO CFloat
attenuationEndDistance mdlPhysicallyPlausibleLight  =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "attenuationEndDistance") retCFloat []

-- | @- setAttenuationEndDistance:@
setAttenuationEndDistance :: IsMDLPhysicallyPlausibleLight mdlPhysicallyPlausibleLight => mdlPhysicallyPlausibleLight -> CFloat -> IO ()
setAttenuationEndDistance mdlPhysicallyPlausibleLight  value =
  sendMsg mdlPhysicallyPlausibleLight (mkSelector "setAttenuationEndDistance:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setColorByTemperature:@
setColorByTemperatureSelector :: Selector
setColorByTemperatureSelector = mkSelector "setColorByTemperature:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @lumens@
lumensSelector :: Selector
lumensSelector = mkSelector "lumens"

-- | @Selector@ for @setLumens:@
setLumensSelector :: Selector
setLumensSelector = mkSelector "setLumens:"

-- | @Selector@ for @innerConeAngle@
innerConeAngleSelector :: Selector
innerConeAngleSelector = mkSelector "innerConeAngle"

-- | @Selector@ for @setInnerConeAngle:@
setInnerConeAngleSelector :: Selector
setInnerConeAngleSelector = mkSelector "setInnerConeAngle:"

-- | @Selector@ for @outerConeAngle@
outerConeAngleSelector :: Selector
outerConeAngleSelector = mkSelector "outerConeAngle"

-- | @Selector@ for @setOuterConeAngle:@
setOuterConeAngleSelector :: Selector
setOuterConeAngleSelector = mkSelector "setOuterConeAngle:"

-- | @Selector@ for @attenuationStartDistance@
attenuationStartDistanceSelector :: Selector
attenuationStartDistanceSelector = mkSelector "attenuationStartDistance"

-- | @Selector@ for @setAttenuationStartDistance:@
setAttenuationStartDistanceSelector :: Selector
setAttenuationStartDistanceSelector = mkSelector "setAttenuationStartDistance:"

-- | @Selector@ for @attenuationEndDistance@
attenuationEndDistanceSelector :: Selector
attenuationEndDistanceSelector = mkSelector "attenuationEndDistance"

-- | @Selector@ for @setAttenuationEndDistance:@
setAttenuationEndDistanceSelector :: Selector
setAttenuationEndDistanceSelector = mkSelector "setAttenuationEndDistance:"

