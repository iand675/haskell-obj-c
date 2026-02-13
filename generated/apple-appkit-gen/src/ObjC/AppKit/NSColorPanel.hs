{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSColorPanel@.
module ObjC.AppKit.NSColorPanel
  ( NSColorPanel
  , IsNSColorPanel(..)
  , dragColor_withEvent_fromView
  , setPickerMask
  , setPickerMode
  , setAction
  , setTarget
  , attachColorList
  , detachColorList
  , sharedColorPanel
  , sharedColorPanelExists
  , accessoryView
  , setAccessoryView
  , continuous
  , setContinuous
  , showsAlpha
  , setShowsAlpha
  , mode
  , setMode
  , color
  , setColor
  , alpha
  , maximumLinearExposure
  , setMaximumLinearExposure
  , accessoryViewSelector
  , alphaSelector
  , attachColorListSelector
  , colorSelector
  , continuousSelector
  , detachColorListSelector
  , dragColor_withEvent_fromViewSelector
  , maximumLinearExposureSelector
  , modeSelector
  , setAccessoryViewSelector
  , setActionSelector
  , setColorSelector
  , setContinuousSelector
  , setMaximumLinearExposureSelector
  , setModeSelector
  , setPickerMaskSelector
  , setPickerModeSelector
  , setShowsAlphaSelector
  , setTargetSelector
  , sharedColorPanelExistsSelector
  , sharedColorPanelSelector
  , showsAlphaSelector

  -- * Enum types
  , NSColorPanelMode(NSColorPanelMode)
  , pattern NSColorPanelModeNone
  , pattern NSColorPanelModeGray
  , pattern NSColorPanelModeRGB
  , pattern NSColorPanelModeCMYK
  , pattern NSColorPanelModeHSB
  , pattern NSColorPanelModeCustomPalette
  , pattern NSColorPanelModeColorList
  , pattern NSColorPanelModeWheel
  , pattern NSColorPanelModeCrayon
  , NSColorPanelOptions(NSColorPanelOptions)
  , pattern NSColorPanelGrayModeMask
  , pattern NSColorPanelRGBModeMask
  , pattern NSColorPanelCMYKModeMask
  , pattern NSColorPanelHSBModeMask
  , pattern NSColorPanelCustomPaletteModeMask
  , pattern NSColorPanelColorListModeMask
  , pattern NSColorPanelWheelModeMask
  , pattern NSColorPanelCrayonModeMask
  , pattern NSColorPanelAllModesMask

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ dragColor:withEvent:fromView:@
dragColor_withEvent_fromView :: (IsNSColor color, IsNSEvent event, IsNSView sourceView) => color -> event -> sourceView -> IO Bool
dragColor_withEvent_fromView color event sourceView =
  do
    cls' <- getRequiredClass "NSColorPanel"
    sendClassMessage cls' dragColor_withEvent_fromViewSelector (toNSColor color) (toNSEvent event) (toNSView sourceView)

-- | @+ setPickerMask:@
setPickerMask :: NSColorPanelOptions -> IO ()
setPickerMask mask =
  do
    cls' <- getRequiredClass "NSColorPanel"
    sendClassMessage cls' setPickerMaskSelector mask

-- | @+ setPickerMode:@
setPickerMode :: NSColorPanelMode -> IO ()
setPickerMode mode =
  do
    cls' <- getRequiredClass "NSColorPanel"
    sendClassMessage cls' setPickerModeSelector mode

-- | @- setAction:@
setAction :: IsNSColorPanel nsColorPanel => nsColorPanel -> Sel -> IO ()
setAction nsColorPanel selector =
  sendMessage nsColorPanel setActionSelector selector

-- | @- setTarget:@
setTarget :: IsNSColorPanel nsColorPanel => nsColorPanel -> RawId -> IO ()
setTarget nsColorPanel target =
  sendMessage nsColorPanel setTargetSelector target

-- | @- attachColorList:@
attachColorList :: (IsNSColorPanel nsColorPanel, IsNSColorList colorList) => nsColorPanel -> colorList -> IO ()
attachColorList nsColorPanel colorList =
  sendMessage nsColorPanel attachColorListSelector (toNSColorList colorList)

-- | @- detachColorList:@
detachColorList :: (IsNSColorPanel nsColorPanel, IsNSColorList colorList) => nsColorPanel -> colorList -> IO ()
detachColorList nsColorPanel colorList =
  sendMessage nsColorPanel detachColorListSelector (toNSColorList colorList)

-- | @+ sharedColorPanel@
sharedColorPanel :: IO (Id NSColorPanel)
sharedColorPanel  =
  do
    cls' <- getRequiredClass "NSColorPanel"
    sendClassMessage cls' sharedColorPanelSelector

-- | @+ sharedColorPanelExists@
sharedColorPanelExists :: IO Bool
sharedColorPanelExists  =
  do
    cls' <- getRequiredClass "NSColorPanel"
    sendClassMessage cls' sharedColorPanelExistsSelector

-- | @- accessoryView@
accessoryView :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO (Id NSView)
accessoryView nsColorPanel =
  sendMessage nsColorPanel accessoryViewSelector

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSColorPanel nsColorPanel, IsNSView value) => nsColorPanel -> value -> IO ()
setAccessoryView nsColorPanel value =
  sendMessage nsColorPanel setAccessoryViewSelector (toNSView value)

-- | @- continuous@
continuous :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO Bool
continuous nsColorPanel =
  sendMessage nsColorPanel continuousSelector

-- | @- setContinuous:@
setContinuous :: IsNSColorPanel nsColorPanel => nsColorPanel -> Bool -> IO ()
setContinuous nsColorPanel value =
  sendMessage nsColorPanel setContinuousSelector value

-- | @- showsAlpha@
showsAlpha :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO Bool
showsAlpha nsColorPanel =
  sendMessage nsColorPanel showsAlphaSelector

-- | @- setShowsAlpha:@
setShowsAlpha :: IsNSColorPanel nsColorPanel => nsColorPanel -> Bool -> IO ()
setShowsAlpha nsColorPanel value =
  sendMessage nsColorPanel setShowsAlphaSelector value

-- | @- mode@
mode :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO NSColorPanelMode
mode nsColorPanel =
  sendMessage nsColorPanel modeSelector

-- | @- setMode:@
setMode :: IsNSColorPanel nsColorPanel => nsColorPanel -> NSColorPanelMode -> IO ()
setMode nsColorPanel value =
  sendMessage nsColorPanel setModeSelector value

-- | @- color@
color :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO (Id NSColor)
color nsColorPanel =
  sendMessage nsColorPanel colorSelector

-- | @- setColor:@
setColor :: (IsNSColorPanel nsColorPanel, IsNSColor value) => nsColorPanel -> value -> IO ()
setColor nsColorPanel value =
  sendMessage nsColorPanel setColorSelector (toNSColor value)

-- | @- alpha@
alpha :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO CDouble
alpha nsColorPanel =
  sendMessage nsColorPanel alphaSelector

-- | The maximum linear exposure that can be set on a color picked in the color panel. Defaults to 1 and ignores any value less than 1. If set to a value >= 2, the color picked by the panel may have a linear exposure applied to it.
--
-- ObjC selector: @- maximumLinearExposure@
maximumLinearExposure :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO CDouble
maximumLinearExposure nsColorPanel =
  sendMessage nsColorPanel maximumLinearExposureSelector

-- | The maximum linear exposure that can be set on a color picked in the color panel. Defaults to 1 and ignores any value less than 1. If set to a value >= 2, the color picked by the panel may have a linear exposure applied to it.
--
-- ObjC selector: @- setMaximumLinearExposure:@
setMaximumLinearExposure :: IsNSColorPanel nsColorPanel => nsColorPanel -> CDouble -> IO ()
setMaximumLinearExposure nsColorPanel value =
  sendMessage nsColorPanel setMaximumLinearExposureSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dragColor:withEvent:fromView:@
dragColor_withEvent_fromViewSelector :: Selector '[Id NSColor, Id NSEvent, Id NSView] Bool
dragColor_withEvent_fromViewSelector = mkSelector "dragColor:withEvent:fromView:"

-- | @Selector@ for @setPickerMask:@
setPickerMaskSelector :: Selector '[NSColorPanelOptions] ()
setPickerMaskSelector = mkSelector "setPickerMask:"

-- | @Selector@ for @setPickerMode:@
setPickerModeSelector :: Selector '[NSColorPanelMode] ()
setPickerModeSelector = mkSelector "setPickerMode:"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @attachColorList:@
attachColorListSelector :: Selector '[Id NSColorList] ()
attachColorListSelector = mkSelector "attachColorList:"

-- | @Selector@ for @detachColorList:@
detachColorListSelector :: Selector '[Id NSColorList] ()
detachColorListSelector = mkSelector "detachColorList:"

-- | @Selector@ for @sharedColorPanel@
sharedColorPanelSelector :: Selector '[] (Id NSColorPanel)
sharedColorPanelSelector = mkSelector "sharedColorPanel"

-- | @Selector@ for @sharedColorPanelExists@
sharedColorPanelExistsSelector :: Selector '[] Bool
sharedColorPanelExistsSelector = mkSelector "sharedColorPanelExists"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @continuous@
continuousSelector :: Selector '[] Bool
continuousSelector = mkSelector "continuous"

-- | @Selector@ for @setContinuous:@
setContinuousSelector :: Selector '[Bool] ()
setContinuousSelector = mkSelector "setContinuous:"

-- | @Selector@ for @showsAlpha@
showsAlphaSelector :: Selector '[] Bool
showsAlphaSelector = mkSelector "showsAlpha"

-- | @Selector@ for @setShowsAlpha:@
setShowsAlphaSelector :: Selector '[Bool] ()
setShowsAlphaSelector = mkSelector "setShowsAlpha:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] NSColorPanelMode
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[NSColorPanelMode] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSColor)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSColor] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @maximumLinearExposure@
maximumLinearExposureSelector :: Selector '[] CDouble
maximumLinearExposureSelector = mkSelector "maximumLinearExposure"

-- | @Selector@ for @setMaximumLinearExposure:@
setMaximumLinearExposureSelector :: Selector '[CDouble] ()
setMaximumLinearExposureSelector = mkSelector "setMaximumLinearExposure:"

