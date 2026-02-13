{-# LANGUAGE DataKinds #-}
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
  , subtract_
  , divide
  , linearBurn
  , linearDodge
  , vividLight
  , linearLight
  , pinLight
  , hardMix
  , darkerColor
  , lighterColor
  , applyWithForeground_backgroundSelector
  , applyWithForeground_background_colorSpaceSelector
  , clearSelector
  , colorBurnSelector
  , colorDodgeSelector
  , colorSelector
  , componentAddSelector
  , componentMaxSelector
  , componentMinSelector
  , componentMultiplySelector
  , darkenSelector
  , darkerColorSelector
  , destinationAtopSelector
  , destinationInSelector
  , destinationOutSelector
  , destinationOverSelector
  , destinationSelector
  , differenceSelector
  , divideSelector
  , exclusionSelector
  , exclusiveOrSelector
  , hardLightSelector
  , hardMixSelector
  , hueSelector
  , kernelWithStringSelector
  , lightenSelector
  , lighterColorSelector
  , linearBurnSelector
  , linearDodgeSelector
  , linearLightSelector
  , luminositySelector
  , multiplySelector
  , overlaySelector
  , pinLightSelector
  , saturationSelector
  , screenSelector
  , softLightSelector
  , sourceAtopSelector
  , sourceInSelector
  , sourceOutSelector
  , sourceOverSelector
  , sourceSelector
  , subtractSelector
  , vividLightSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ kernelWithString:@
kernelWithString :: IsNSString string => string -> IO (Id CIBlendKernel)
kernelWithString string =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' kernelWithStringSelector (toNSString string)

-- | @- applyWithForeground:background:@
applyWithForeground_background :: (IsCIBlendKernel ciBlendKernel, IsCIImage foreground, IsCIImage background) => ciBlendKernel -> foreground -> background -> IO (Id CIImage)
applyWithForeground_background ciBlendKernel foreground background =
  sendMessage ciBlendKernel applyWithForeground_backgroundSelector (toCIImage foreground) (toCIImage background)

-- | @- applyWithForeground:background:colorSpace:@
applyWithForeground_background_colorSpace :: (IsCIBlendKernel ciBlendKernel, IsCIImage foreground, IsCIImage background) => ciBlendKernel -> foreground -> background -> Ptr () -> IO (Id CIImage)
applyWithForeground_background_colorSpace ciBlendKernel foreground background colorSpace =
  sendMessage ciBlendKernel applyWithForeground_background_colorSpaceSelector (toCIImage foreground) (toCIImage background) colorSpace

-- | @+ componentAdd@
componentAdd :: IO (Id CIBlendKernel)
componentAdd  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' componentAddSelector

-- | @+ componentMultiply@
componentMultiply :: IO (Id CIBlendKernel)
componentMultiply  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' componentMultiplySelector

-- | @+ componentMin@
componentMin :: IO (Id CIBlendKernel)
componentMin  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' componentMinSelector

-- | @+ componentMax@
componentMax :: IO (Id CIBlendKernel)
componentMax  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' componentMaxSelector

-- | @+ clear@
clear :: IO (Id CIBlendKernel)
clear  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' clearSelector

-- | @+ source@
source :: IO (Id CIBlendKernel)
source  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' sourceSelector

-- | @+ destination@
destination :: IO (Id CIBlendKernel)
destination  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' destinationSelector

-- | @+ sourceOver@
sourceOver :: IO (Id CIBlendKernel)
sourceOver  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' sourceOverSelector

-- | @+ destinationOver@
destinationOver :: IO (Id CIBlendKernel)
destinationOver  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' destinationOverSelector

-- | @+ sourceIn@
sourceIn :: IO (Id CIBlendKernel)
sourceIn  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' sourceInSelector

-- | @+ destinationIn@
destinationIn :: IO (Id CIBlendKernel)
destinationIn  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' destinationInSelector

-- | @+ sourceOut@
sourceOut :: IO (Id CIBlendKernel)
sourceOut  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' sourceOutSelector

-- | @+ destinationOut@
destinationOut :: IO (Id CIBlendKernel)
destinationOut  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' destinationOutSelector

-- | @+ sourceAtop@
sourceAtop :: IO (Id CIBlendKernel)
sourceAtop  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' sourceAtopSelector

-- | @+ destinationAtop@
destinationAtop :: IO (Id CIBlendKernel)
destinationAtop  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' destinationAtopSelector

-- | @+ exclusiveOr@
exclusiveOr :: IO (Id CIBlendKernel)
exclusiveOr  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' exclusiveOrSelector

-- | @+ multiply@
multiply :: IO (Id CIBlendKernel)
multiply  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' multiplySelector

-- | @+ screen@
screen :: IO (Id CIBlendKernel)
screen  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' screenSelector

-- | @+ overlay@
overlay :: IO (Id CIBlendKernel)
overlay  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' overlaySelector

-- | @+ darken@
darken :: IO (Id CIBlendKernel)
darken  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' darkenSelector

-- | @+ lighten@
lighten :: IO (Id CIBlendKernel)
lighten  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' lightenSelector

-- | @+ colorDodge@
colorDodge :: IO (Id CIBlendKernel)
colorDodge  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' colorDodgeSelector

-- | @+ colorBurn@
colorBurn :: IO (Id CIBlendKernel)
colorBurn  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' colorBurnSelector

-- | @+ hardLight@
hardLight :: IO (Id CIBlendKernel)
hardLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' hardLightSelector

-- | @+ softLight@
softLight :: IO (Id CIBlendKernel)
softLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' softLightSelector

-- | @+ difference@
difference :: IO (Id CIBlendKernel)
difference  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' differenceSelector

-- | @+ exclusion@
exclusion :: IO (Id CIBlendKernel)
exclusion  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' exclusionSelector

-- | @+ hue@
hue :: IO (Id CIBlendKernel)
hue  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' hueSelector

-- | @+ saturation@
saturation :: IO (Id CIBlendKernel)
saturation  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' saturationSelector

-- | @+ color@
color :: IO (Id CIBlendKernel)
color  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' colorSelector

-- | @+ luminosity@
luminosity :: IO (Id CIBlendKernel)
luminosity  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' luminositySelector

-- | @+ subtract@
subtract_ :: IO (Id CIBlendKernel)
subtract_  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' subtractSelector

-- | @+ divide@
divide :: IO (Id CIBlendKernel)
divide  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' divideSelector

-- | @+ linearBurn@
linearBurn :: IO (Id CIBlendKernel)
linearBurn  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' linearBurnSelector

-- | @+ linearDodge@
linearDodge :: IO (Id CIBlendKernel)
linearDodge  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' linearDodgeSelector

-- | @+ vividLight@
vividLight :: IO (Id CIBlendKernel)
vividLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' vividLightSelector

-- | @+ linearLight@
linearLight :: IO (Id CIBlendKernel)
linearLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' linearLightSelector

-- | @+ pinLight@
pinLight :: IO (Id CIBlendKernel)
pinLight  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' pinLightSelector

-- | @+ hardMix@
hardMix :: IO (Id CIBlendKernel)
hardMix  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' hardMixSelector

-- | @+ darkerColor@
darkerColor :: IO (Id CIBlendKernel)
darkerColor  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' darkerColorSelector

-- | @+ lighterColor@
lighterColor :: IO (Id CIBlendKernel)
lighterColor  =
  do
    cls' <- getRequiredClass "CIBlendKernel"
    sendClassMessage cls' lighterColorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelWithString:@
kernelWithStringSelector :: Selector '[Id NSString] (Id CIBlendKernel)
kernelWithStringSelector = mkSelector "kernelWithString:"

-- | @Selector@ for @applyWithForeground:background:@
applyWithForeground_backgroundSelector :: Selector '[Id CIImage, Id CIImage] (Id CIImage)
applyWithForeground_backgroundSelector = mkSelector "applyWithForeground:background:"

-- | @Selector@ for @applyWithForeground:background:colorSpace:@
applyWithForeground_background_colorSpaceSelector :: Selector '[Id CIImage, Id CIImage, Ptr ()] (Id CIImage)
applyWithForeground_background_colorSpaceSelector = mkSelector "applyWithForeground:background:colorSpace:"

-- | @Selector@ for @componentAdd@
componentAddSelector :: Selector '[] (Id CIBlendKernel)
componentAddSelector = mkSelector "componentAdd"

-- | @Selector@ for @componentMultiply@
componentMultiplySelector :: Selector '[] (Id CIBlendKernel)
componentMultiplySelector = mkSelector "componentMultiply"

-- | @Selector@ for @componentMin@
componentMinSelector :: Selector '[] (Id CIBlendKernel)
componentMinSelector = mkSelector "componentMin"

-- | @Selector@ for @componentMax@
componentMaxSelector :: Selector '[] (Id CIBlendKernel)
componentMaxSelector = mkSelector "componentMax"

-- | @Selector@ for @clear@
clearSelector :: Selector '[] (Id CIBlendKernel)
clearSelector = mkSelector "clear"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id CIBlendKernel)
sourceSelector = mkSelector "source"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id CIBlendKernel)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @sourceOver@
sourceOverSelector :: Selector '[] (Id CIBlendKernel)
sourceOverSelector = mkSelector "sourceOver"

-- | @Selector@ for @destinationOver@
destinationOverSelector :: Selector '[] (Id CIBlendKernel)
destinationOverSelector = mkSelector "destinationOver"

-- | @Selector@ for @sourceIn@
sourceInSelector :: Selector '[] (Id CIBlendKernel)
sourceInSelector = mkSelector "sourceIn"

-- | @Selector@ for @destinationIn@
destinationInSelector :: Selector '[] (Id CIBlendKernel)
destinationInSelector = mkSelector "destinationIn"

-- | @Selector@ for @sourceOut@
sourceOutSelector :: Selector '[] (Id CIBlendKernel)
sourceOutSelector = mkSelector "sourceOut"

-- | @Selector@ for @destinationOut@
destinationOutSelector :: Selector '[] (Id CIBlendKernel)
destinationOutSelector = mkSelector "destinationOut"

-- | @Selector@ for @sourceAtop@
sourceAtopSelector :: Selector '[] (Id CIBlendKernel)
sourceAtopSelector = mkSelector "sourceAtop"

-- | @Selector@ for @destinationAtop@
destinationAtopSelector :: Selector '[] (Id CIBlendKernel)
destinationAtopSelector = mkSelector "destinationAtop"

-- | @Selector@ for @exclusiveOr@
exclusiveOrSelector :: Selector '[] (Id CIBlendKernel)
exclusiveOrSelector = mkSelector "exclusiveOr"

-- | @Selector@ for @multiply@
multiplySelector :: Selector '[] (Id CIBlendKernel)
multiplySelector = mkSelector "multiply"

-- | @Selector@ for @screen@
screenSelector :: Selector '[] (Id CIBlendKernel)
screenSelector = mkSelector "screen"

-- | @Selector@ for @overlay@
overlaySelector :: Selector '[] (Id CIBlendKernel)
overlaySelector = mkSelector "overlay"

-- | @Selector@ for @darken@
darkenSelector :: Selector '[] (Id CIBlendKernel)
darkenSelector = mkSelector "darken"

-- | @Selector@ for @lighten@
lightenSelector :: Selector '[] (Id CIBlendKernel)
lightenSelector = mkSelector "lighten"

-- | @Selector@ for @colorDodge@
colorDodgeSelector :: Selector '[] (Id CIBlendKernel)
colorDodgeSelector = mkSelector "colorDodge"

-- | @Selector@ for @colorBurn@
colorBurnSelector :: Selector '[] (Id CIBlendKernel)
colorBurnSelector = mkSelector "colorBurn"

-- | @Selector@ for @hardLight@
hardLightSelector :: Selector '[] (Id CIBlendKernel)
hardLightSelector = mkSelector "hardLight"

-- | @Selector@ for @softLight@
softLightSelector :: Selector '[] (Id CIBlendKernel)
softLightSelector = mkSelector "softLight"

-- | @Selector@ for @difference@
differenceSelector :: Selector '[] (Id CIBlendKernel)
differenceSelector = mkSelector "difference"

-- | @Selector@ for @exclusion@
exclusionSelector :: Selector '[] (Id CIBlendKernel)
exclusionSelector = mkSelector "exclusion"

-- | @Selector@ for @hue@
hueSelector :: Selector '[] (Id CIBlendKernel)
hueSelector = mkSelector "hue"

-- | @Selector@ for @saturation@
saturationSelector :: Selector '[] (Id CIBlendKernel)
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id CIBlendKernel)
colorSelector = mkSelector "color"

-- | @Selector@ for @luminosity@
luminositySelector :: Selector '[] (Id CIBlendKernel)
luminositySelector = mkSelector "luminosity"

-- | @Selector@ for @subtract@
subtractSelector :: Selector '[] (Id CIBlendKernel)
subtractSelector = mkSelector "subtract"

-- | @Selector@ for @divide@
divideSelector :: Selector '[] (Id CIBlendKernel)
divideSelector = mkSelector "divide"

-- | @Selector@ for @linearBurn@
linearBurnSelector :: Selector '[] (Id CIBlendKernel)
linearBurnSelector = mkSelector "linearBurn"

-- | @Selector@ for @linearDodge@
linearDodgeSelector :: Selector '[] (Id CIBlendKernel)
linearDodgeSelector = mkSelector "linearDodge"

-- | @Selector@ for @vividLight@
vividLightSelector :: Selector '[] (Id CIBlendKernel)
vividLightSelector = mkSelector "vividLight"

-- | @Selector@ for @linearLight@
linearLightSelector :: Selector '[] (Id CIBlendKernel)
linearLightSelector = mkSelector "linearLight"

-- | @Selector@ for @pinLight@
pinLightSelector :: Selector '[] (Id CIBlendKernel)
pinLightSelector = mkSelector "pinLight"

-- | @Selector@ for @hardMix@
hardMixSelector :: Selector '[] (Id CIBlendKernel)
hardMixSelector = mkSelector "hardMix"

-- | @Selector@ for @darkerColor@
darkerColorSelector :: Selector '[] (Id CIBlendKernel)
darkerColorSelector = mkSelector "darkerColor"

-- | @Selector@ for @lighterColor@
lighterColorSelector :: Selector '[] (Id CIBlendKernel)
lighterColorSelector = mkSelector "lighterColor"

