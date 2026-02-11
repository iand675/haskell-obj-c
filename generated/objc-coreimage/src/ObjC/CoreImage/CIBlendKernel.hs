{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIBlendKernel@.
module ObjC.CoreImage.CIBlendKernel
  ( CIBlendKernel
  , IsCIBlendKernel(..)
  , kernelWithString
  , applyWithForeground_background
  , applyWithForeground_background_colorSpace
  , componentAdd
  , componentMultiply
  , componentMin
  , componentMax
  , clear
  , source
  , destination
  , sourceOver
  , destinationOver
  , sourceIn
  , destinationIn
  , sourceOut
  , destinationOut
  , sourceAtop
  , destinationAtop
  , exclusiveOr
  , multiply
  , screen
  , overlay
  , darken
  , lighten
  , colorDodge
  , colorBurn
  , hardLight
  , softLight
  , difference
  , exclusion
  , hue
  , saturation
  , color
  , luminosity
  , subtract
  , divide
  , linearBurn
  , linearDodge
  , vividLight
  , linearLight
  , pinLight
  , hardMix
  , darkerColor
  , lighterColor
  , kernelWithStringSelector
  , applyWithForeground_backgroundSelector
  , applyWithForeground_background_colorSpaceSelector
  , componentAddSelector
  , componentMultiplySelector
  , componentMinSelector
  , componentMaxSelector
  , clearSelector
  , sourceSelector
  , destinationSelector
  , sourceOverSelector
  , destinationOverSelector
  , sourceInSelector
  , destinationInSelector
  , sourceOutSelector
  , destinationOutSelector
  , sourceAtopSelector
  , destinationAtopSelector
  , exclusiveOrSelector
  , multiplySelector
  , screenSelector
  , overlaySelector
  , darkenSelector
  , lightenSelector
  , colorDodgeSelector
  , colorBurnSelector
  , hardLightSelector
  , softLightSelector
  , differenceSelector
  , exclusionSelector
  , hueSelector
  , saturationSelector
  , colorSelector
  , luminositySelector
  , subtractSelector
  , divideSelector
  , linearBurnSelector
  , linearDodgeSelector
  , vividLightSelector
  , linearLightSelector
  , pinLightSelector
  , hardMixSelector
  , darkerColorSelector
  , lighterColorSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ kernelWithString:@
kernelWithString :: IsNSString string => string -> IO (Id CIBlendKernel)
kernelWithString string =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "kernelWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- applyWithForeground:background:@
applyWithForeground_background :: (IsCIBlendKernel ciBlendKernel, IsCIImage foreground, IsCIImage background) => ciBlendKernel -> foreground -> background -> IO (Id CIImage)
applyWithForeground_background ciBlendKernel  foreground background =
withObjCPtr foreground $ \raw_foreground ->
  withObjCPtr background $ \raw_background ->
      sendMsg ciBlendKernel (mkSelector "applyWithForeground:background:") (retPtr retVoid) [argPtr (castPtr raw_foreground :: Ptr ()), argPtr (castPtr raw_background :: Ptr ())] >>= retainedObject . castPtr

-- | @- applyWithForeground:background:colorSpace:@
applyWithForeground_background_colorSpace :: (IsCIBlendKernel ciBlendKernel, IsCIImage foreground, IsCIImage background) => ciBlendKernel -> foreground -> background -> Ptr () -> IO (Id CIImage)
applyWithForeground_background_colorSpace ciBlendKernel  foreground background colorSpace =
withObjCPtr foreground $ \raw_foreground ->
  withObjCPtr background $ \raw_background ->
      sendMsg ciBlendKernel (mkSelector "applyWithForeground:background:colorSpace:") (retPtr retVoid) [argPtr (castPtr raw_foreground :: Ptr ()), argPtr (castPtr raw_background :: Ptr ()), argPtr colorSpace] >>= retainedObject . castPtr

-- | @+ componentAdd@
componentAdd :: IO (Id CIBlendKernel)
componentAdd  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "componentAdd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ componentMultiply@
componentMultiply :: IO (Id CIBlendKernel)
componentMultiply  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "componentMultiply") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ componentMin@
componentMin :: IO (Id CIBlendKernel)
componentMin  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "componentMin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ componentMax@
componentMax :: IO (Id CIBlendKernel)
componentMax  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "componentMax") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ clear@
clear :: IO (Id CIBlendKernel)
clear  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "clear") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ source@
source :: IO (Id CIBlendKernel)
source  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ destination@
destination :: IO (Id CIBlendKernel)
destination  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "destination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sourceOver@
sourceOver :: IO (Id CIBlendKernel)
sourceOver  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "sourceOver") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ destinationOver@
destinationOver :: IO (Id CIBlendKernel)
destinationOver  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "destinationOver") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sourceIn@
sourceIn :: IO (Id CIBlendKernel)
sourceIn  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "sourceIn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ destinationIn@
destinationIn :: IO (Id CIBlendKernel)
destinationIn  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "destinationIn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sourceOut@
sourceOut :: IO (Id CIBlendKernel)
sourceOut  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "sourceOut") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ destinationOut@
destinationOut :: IO (Id CIBlendKernel)
destinationOut  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "destinationOut") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sourceAtop@
sourceAtop :: IO (Id CIBlendKernel)
sourceAtop  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "sourceAtop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ destinationAtop@
destinationAtop :: IO (Id CIBlendKernel)
destinationAtop  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "destinationAtop") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ exclusiveOr@
exclusiveOr :: IO (Id CIBlendKernel)
exclusiveOr  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "exclusiveOr") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ multiply@
multiply :: IO (Id CIBlendKernel)
multiply  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "multiply") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ screen@
screen :: IO (Id CIBlendKernel)
screen  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "screen") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ overlay@
overlay :: IO (Id CIBlendKernel)
overlay  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "overlay") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ darken@
darken :: IO (Id CIBlendKernel)
darken  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "darken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ lighten@
lighten :: IO (Id CIBlendKernel)
lighten  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "lighten") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ colorDodge@
colorDodge :: IO (Id CIBlendKernel)
colorDodge  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "colorDodge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ colorBurn@
colorBurn :: IO (Id CIBlendKernel)
colorBurn  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "colorBurn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hardLight@
hardLight :: IO (Id CIBlendKernel)
hardLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "hardLight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ softLight@
softLight :: IO (Id CIBlendKernel)
softLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "softLight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ difference@
difference :: IO (Id CIBlendKernel)
difference  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "difference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ exclusion@
exclusion :: IO (Id CIBlendKernel)
exclusion  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "exclusion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hue@
hue :: IO (Id CIBlendKernel)
hue  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "hue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ saturation@
saturation :: IO (Id CIBlendKernel)
saturation  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "saturation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ color@
color :: IO (Id CIBlendKernel)
color  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ luminosity@
luminosity :: IO (Id CIBlendKernel)
luminosity  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "luminosity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ subtract@
subtract :: IO (Id CIBlendKernel)
subtract  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "subtract") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ divide@
divide :: IO (Id CIBlendKernel)
divide  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "divide") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ linearBurn@
linearBurn :: IO (Id CIBlendKernel)
linearBurn  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "linearBurn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ linearDodge@
linearDodge :: IO (Id CIBlendKernel)
linearDodge  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "linearDodge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ vividLight@
vividLight :: IO (Id CIBlendKernel)
vividLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "vividLight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ linearLight@
linearLight :: IO (Id CIBlendKernel)
linearLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "linearLight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ pinLight@
pinLight :: IO (Id CIBlendKernel)
pinLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "pinLight") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hardMix@
hardMix :: IO (Id CIBlendKernel)
hardMix  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "hardMix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ darkerColor@
darkerColor :: IO (Id CIBlendKernel)
darkerColor  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "darkerColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ lighterColor@
lighterColor :: IO (Id CIBlendKernel)
lighterColor  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMsg cls' (mkSelector "lighterColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelWithString:@
kernelWithStringSelector :: Selector
kernelWithStringSelector = mkSelector "kernelWithString:"

-- | @Selector@ for @applyWithForeground:background:@
applyWithForeground_backgroundSelector :: Selector
applyWithForeground_backgroundSelector = mkSelector "applyWithForeground:background:"

-- | @Selector@ for @applyWithForeground:background:colorSpace:@
applyWithForeground_background_colorSpaceSelector :: Selector
applyWithForeground_background_colorSpaceSelector = mkSelector "applyWithForeground:background:colorSpace:"

-- | @Selector@ for @componentAdd@
componentAddSelector :: Selector
componentAddSelector = mkSelector "componentAdd"

-- | @Selector@ for @componentMultiply@
componentMultiplySelector :: Selector
componentMultiplySelector = mkSelector "componentMultiply"

-- | @Selector@ for @componentMin@
componentMinSelector :: Selector
componentMinSelector = mkSelector "componentMin"

-- | @Selector@ for @componentMax@
componentMaxSelector :: Selector
componentMaxSelector = mkSelector "componentMax"

-- | @Selector@ for @clear@
clearSelector :: Selector
clearSelector = mkSelector "clear"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @sourceOver@
sourceOverSelector :: Selector
sourceOverSelector = mkSelector "sourceOver"

-- | @Selector@ for @destinationOver@
destinationOverSelector :: Selector
destinationOverSelector = mkSelector "destinationOver"

-- | @Selector@ for @sourceIn@
sourceInSelector :: Selector
sourceInSelector = mkSelector "sourceIn"

-- | @Selector@ for @destinationIn@
destinationInSelector :: Selector
destinationInSelector = mkSelector "destinationIn"

-- | @Selector@ for @sourceOut@
sourceOutSelector :: Selector
sourceOutSelector = mkSelector "sourceOut"

-- | @Selector@ for @destinationOut@
destinationOutSelector :: Selector
destinationOutSelector = mkSelector "destinationOut"

-- | @Selector@ for @sourceAtop@
sourceAtopSelector :: Selector
sourceAtopSelector = mkSelector "sourceAtop"

-- | @Selector@ for @destinationAtop@
destinationAtopSelector :: Selector
destinationAtopSelector = mkSelector "destinationAtop"

-- | @Selector@ for @exclusiveOr@
exclusiveOrSelector :: Selector
exclusiveOrSelector = mkSelector "exclusiveOr"

-- | @Selector@ for @multiply@
multiplySelector :: Selector
multiplySelector = mkSelector "multiply"

-- | @Selector@ for @screen@
screenSelector :: Selector
screenSelector = mkSelector "screen"

-- | @Selector@ for @overlay@
overlaySelector :: Selector
overlaySelector = mkSelector "overlay"

-- | @Selector@ for @darken@
darkenSelector :: Selector
darkenSelector = mkSelector "darken"

-- | @Selector@ for @lighten@
lightenSelector :: Selector
lightenSelector = mkSelector "lighten"

-- | @Selector@ for @colorDodge@
colorDodgeSelector :: Selector
colorDodgeSelector = mkSelector "colorDodge"

-- | @Selector@ for @colorBurn@
colorBurnSelector :: Selector
colorBurnSelector = mkSelector "colorBurn"

-- | @Selector@ for @hardLight@
hardLightSelector :: Selector
hardLightSelector = mkSelector "hardLight"

-- | @Selector@ for @softLight@
softLightSelector :: Selector
softLightSelector = mkSelector "softLight"

-- | @Selector@ for @difference@
differenceSelector :: Selector
differenceSelector = mkSelector "difference"

-- | @Selector@ for @exclusion@
exclusionSelector :: Selector
exclusionSelector = mkSelector "exclusion"

-- | @Selector@ for @hue@
hueSelector :: Selector
hueSelector = mkSelector "hue"

-- | @Selector@ for @saturation@
saturationSelector :: Selector
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @luminosity@
luminositySelector :: Selector
luminositySelector = mkSelector "luminosity"

-- | @Selector@ for @subtract@
subtractSelector :: Selector
subtractSelector = mkSelector "subtract"

-- | @Selector@ for @divide@
divideSelector :: Selector
divideSelector = mkSelector "divide"

-- | @Selector@ for @linearBurn@
linearBurnSelector :: Selector
linearBurnSelector = mkSelector "linearBurn"

-- | @Selector@ for @linearDodge@
linearDodgeSelector :: Selector
linearDodgeSelector = mkSelector "linearDodge"

-- | @Selector@ for @vividLight@
vividLightSelector :: Selector
vividLightSelector = mkSelector "vividLight"

-- | @Selector@ for @linearLight@
linearLightSelector :: Selector
linearLightSelector = mkSelector "linearLight"

-- | @Selector@ for @pinLight@
pinLightSelector :: Selector
pinLightSelector = mkSelector "pinLight"

-- | @Selector@ for @hardMix@
hardMixSelector :: Selector
hardMixSelector = mkSelector "hardMix"

-- | @Selector@ for @darkerColor@
darkerColorSelector :: Selector
darkerColorSelector = mkSelector "darkerColor"

-- | @Selector@ for @lighterColor@
lighterColorSelector :: Selector
lighterColorSelector = mkSelector "lighterColor"

