{-# LANGUAGE PatternSynonyms #-}
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
  , dragColor_withEvent_fromViewSelector
  , setPickerMaskSelector
  , setPickerModeSelector
  , setActionSelector
  , setTargetSelector
  , attachColorListSelector
  , detachColorListSelector
  , sharedColorPanelSelector
  , sharedColorPanelExistsSelector
  , accessoryViewSelector
  , setAccessoryViewSelector
  , continuousSelector
  , setContinuousSelector
  , showsAlphaSelector
  , setShowsAlphaSelector
  , modeSelector
  , setModeSelector
  , colorSelector
  , setColorSelector
  , alphaSelector
  , maximumLinearExposureSelector
  , setMaximumLinearExposureSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ dragColor:withEvent:fromView:@
dragColor_withEvent_fromView :: (IsNSColor color, IsNSEvent event, IsNSView sourceView) => color -> event -> sourceView -> IO Bool
dragColor_withEvent_fromView color event sourceView =
  do
    cls' <- getRequiredClass "NSColorPanel"
    withObjCPtr color $ \raw_color ->
      withObjCPtr event $ \raw_event ->
        withObjCPtr sourceView $ \raw_sourceView ->
          fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "dragColor:withEvent:fromView:") retCULong [argPtr (castPtr raw_color :: Ptr ()), argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr raw_sourceView :: Ptr ())]

-- | @+ setPickerMask:@
setPickerMask :: NSColorPanelOptions -> IO ()
setPickerMask mask =
  do
    cls' <- getRequiredClass "NSColorPanel"
    sendClassMsg cls' (mkSelector "setPickerMask:") retVoid [argCULong (coerce mask)]

-- | @+ setPickerMode:@
setPickerMode :: NSColorPanelMode -> IO ()
setPickerMode mode =
  do
    cls' <- getRequiredClass "NSColorPanel"
    sendClassMsg cls' (mkSelector "setPickerMode:") retVoid [argCLong (coerce mode)]

-- | @- setAction:@
setAction :: IsNSColorPanel nsColorPanel => nsColorPanel -> Selector -> IO ()
setAction nsColorPanel  selector =
  sendMsg nsColorPanel (mkSelector "setAction:") retVoid [argPtr (unSelector selector)]

-- | @- setTarget:@
setTarget :: IsNSColorPanel nsColorPanel => nsColorPanel -> RawId -> IO ()
setTarget nsColorPanel  target =
  sendMsg nsColorPanel (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId target) :: Ptr ())]

-- | @- attachColorList:@
attachColorList :: (IsNSColorPanel nsColorPanel, IsNSColorList colorList) => nsColorPanel -> colorList -> IO ()
attachColorList nsColorPanel  colorList =
withObjCPtr colorList $ \raw_colorList ->
    sendMsg nsColorPanel (mkSelector "attachColorList:") retVoid [argPtr (castPtr raw_colorList :: Ptr ())]

-- | @- detachColorList:@
detachColorList :: (IsNSColorPanel nsColorPanel, IsNSColorList colorList) => nsColorPanel -> colorList -> IO ()
detachColorList nsColorPanel  colorList =
withObjCPtr colorList $ \raw_colorList ->
    sendMsg nsColorPanel (mkSelector "detachColorList:") retVoid [argPtr (castPtr raw_colorList :: Ptr ())]

-- | @+ sharedColorPanel@
sharedColorPanel :: IO (Id NSColorPanel)
sharedColorPanel  =
  do
    cls' <- getRequiredClass "NSColorPanel"
    sendClassMsg cls' (mkSelector "sharedColorPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ sharedColorPanelExists@
sharedColorPanelExists :: IO Bool
sharedColorPanelExists  =
  do
    cls' <- getRequiredClass "NSColorPanel"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "sharedColorPanelExists") retCULong []

-- | @- accessoryView@
accessoryView :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO (Id NSView)
accessoryView nsColorPanel  =
  sendMsg nsColorPanel (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSColorPanel nsColorPanel, IsNSView value) => nsColorPanel -> value -> IO ()
setAccessoryView nsColorPanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsColorPanel (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- continuous@
continuous :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO Bool
continuous nsColorPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorPanel (mkSelector "continuous") retCULong []

-- | @- setContinuous:@
setContinuous :: IsNSColorPanel nsColorPanel => nsColorPanel -> Bool -> IO ()
setContinuous nsColorPanel  value =
  sendMsg nsColorPanel (mkSelector "setContinuous:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsAlpha@
showsAlpha :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO Bool
showsAlpha nsColorPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorPanel (mkSelector "showsAlpha") retCULong []

-- | @- setShowsAlpha:@
setShowsAlpha :: IsNSColorPanel nsColorPanel => nsColorPanel -> Bool -> IO ()
setShowsAlpha nsColorPanel  value =
  sendMsg nsColorPanel (mkSelector "setShowsAlpha:") retVoid [argCULong (if value then 1 else 0)]

-- | @- mode@
mode :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO NSColorPanelMode
mode nsColorPanel  =
  fmap (coerce :: CLong -> NSColorPanelMode) $ sendMsg nsColorPanel (mkSelector "mode") retCLong []

-- | @- setMode:@
setMode :: IsNSColorPanel nsColorPanel => nsColorPanel -> NSColorPanelMode -> IO ()
setMode nsColorPanel  value =
  sendMsg nsColorPanel (mkSelector "setMode:") retVoid [argCLong (coerce value)]

-- | @- color@
color :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO (Id NSColor)
color nsColorPanel  =
  sendMsg nsColorPanel (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsNSColorPanel nsColorPanel, IsNSColor value) => nsColorPanel -> value -> IO ()
setColor nsColorPanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsColorPanel (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alpha@
alpha :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO CDouble
alpha nsColorPanel  =
  sendMsg nsColorPanel (mkSelector "alpha") retCDouble []

-- | The maximum linear exposure that can be set on a color picked in the color panel. Defaults to 1 and ignores any value less than 1. If set to a value >= 2, the color picked by the panel may have a linear exposure applied to it.
--
-- ObjC selector: @- maximumLinearExposure@
maximumLinearExposure :: IsNSColorPanel nsColorPanel => nsColorPanel -> IO CDouble
maximumLinearExposure nsColorPanel  =
  sendMsg nsColorPanel (mkSelector "maximumLinearExposure") retCDouble []

-- | The maximum linear exposure that can be set on a color picked in the color panel. Defaults to 1 and ignores any value less than 1. If set to a value >= 2, the color picked by the panel may have a linear exposure applied to it.
--
-- ObjC selector: @- setMaximumLinearExposure:@
setMaximumLinearExposure :: IsNSColorPanel nsColorPanel => nsColorPanel -> CDouble -> IO ()
setMaximumLinearExposure nsColorPanel  value =
  sendMsg nsColorPanel (mkSelector "setMaximumLinearExposure:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dragColor:withEvent:fromView:@
dragColor_withEvent_fromViewSelector :: Selector
dragColor_withEvent_fromViewSelector = mkSelector "dragColor:withEvent:fromView:"

-- | @Selector@ for @setPickerMask:@
setPickerMaskSelector :: Selector
setPickerMaskSelector = mkSelector "setPickerMask:"

-- | @Selector@ for @setPickerMode:@
setPickerModeSelector :: Selector
setPickerModeSelector = mkSelector "setPickerMode:"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @attachColorList:@
attachColorListSelector :: Selector
attachColorListSelector = mkSelector "attachColorList:"

-- | @Selector@ for @detachColorList:@
detachColorListSelector :: Selector
detachColorListSelector = mkSelector "detachColorList:"

-- | @Selector@ for @sharedColorPanel@
sharedColorPanelSelector :: Selector
sharedColorPanelSelector = mkSelector "sharedColorPanel"

-- | @Selector@ for @sharedColorPanelExists@
sharedColorPanelExistsSelector :: Selector
sharedColorPanelExistsSelector = mkSelector "sharedColorPanelExists"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @continuous@
continuousSelector :: Selector
continuousSelector = mkSelector "continuous"

-- | @Selector@ for @setContinuous:@
setContinuousSelector :: Selector
setContinuousSelector = mkSelector "setContinuous:"

-- | @Selector@ for @showsAlpha@
showsAlphaSelector :: Selector
showsAlphaSelector = mkSelector "showsAlpha"

-- | @Selector@ for @setShowsAlpha:@
setShowsAlphaSelector :: Selector
setShowsAlphaSelector = mkSelector "setShowsAlpha:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @maximumLinearExposure@
maximumLinearExposureSelector :: Selector
maximumLinearExposureSelector = mkSelector "maximumLinearExposure"

-- | @Selector@ for @setMaximumLinearExposure:@
setMaximumLinearExposureSelector :: Selector
setMaximumLinearExposureSelector = mkSelector "setMaximumLinearExposure:"

