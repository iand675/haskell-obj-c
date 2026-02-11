{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSColorWell@.
module ObjC.AppKit.NSColorWell
  ( NSColorWell
  , IsNSColorWell(..)
  , colorWellWithStyle
  , deactivate
  , activate
  , drawWellInside
  , takeColorFrom
  , active
  , bordered
  , setBordered
  , color
  , setColor
  , colorWellStyle
  , setColorWellStyle
  , pulldownTarget
  , setPulldownTarget
  , pulldownAction
  , setPulldownAction
  , supportsAlpha
  , setSupportsAlpha
  , maximumLinearExposure
  , setMaximumLinearExposure
  , colorWellWithStyleSelector
  , deactivateSelector
  , activateSelector
  , drawWellInsideSelector
  , takeColorFromSelector
  , activeSelector
  , borderedSelector
  , setBorderedSelector
  , colorSelector
  , setColorSelector
  , colorWellStyleSelector
  , setColorWellStyleSelector
  , pulldownTargetSelector
  , setPulldownTargetSelector
  , pulldownActionSelector
  , setPulldownActionSelector
  , supportsAlphaSelector
  , setSupportsAlphaSelector
  , maximumLinearExposureSelector
  , setMaximumLinearExposureSelector

  -- * Enum types
  , NSColorWellStyle(NSColorWellStyle)
  , pattern NSColorWellStyleDefault
  , pattern NSColorWellStyleMinimal
  , pattern NSColorWellStyleExpanded

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ colorWellWithStyle:@
colorWellWithStyle :: NSColorWellStyle -> IO (Id NSColorWell)
colorWellWithStyle style =
  do
    cls' <- getRequiredClass "NSColorWell"
    sendClassMsg cls' (mkSelector "colorWellWithStyle:") (retPtr retVoid) [argCLong (coerce style)] >>= retainedObject . castPtr

-- | Instantiates a well in the given style with a default configuration.
--
-- ObjC selector: @- deactivate@
deactivate :: IsNSColorWell nsColorWell => nsColorWell -> IO ()
deactivate nsColorWell  =
  sendMsg nsColorWell (mkSelector "deactivate") retVoid []

-- | @- activate:@
activate :: IsNSColorWell nsColorWell => nsColorWell -> Bool -> IO ()
activate nsColorWell  exclusive =
  sendMsg nsColorWell (mkSelector "activate:") retVoid [argCULong (if exclusive then 1 else 0)]

-- | @- drawWellInside:@
drawWellInside :: IsNSColorWell nsColorWell => nsColorWell -> NSRect -> IO ()
drawWellInside nsColorWell  insideRect =
  sendMsg nsColorWell (mkSelector "drawWellInside:") retVoid [argNSRect insideRect]

-- | @- takeColorFrom:@
takeColorFrom :: IsNSColorWell nsColorWell => nsColorWell -> RawId -> IO ()
takeColorFrom nsColorWell  sender =
  sendMsg nsColorWell (mkSelector "takeColorFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- active@
active :: IsNSColorWell nsColorWell => nsColorWell -> IO Bool
active nsColorWell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorWell (mkSelector "active") retCULong []

-- | @- bordered@
bordered :: IsNSColorWell nsColorWell => nsColorWell -> IO Bool
bordered nsColorWell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorWell (mkSelector "bordered") retCULong []

-- | @- setBordered:@
setBordered :: IsNSColorWell nsColorWell => nsColorWell -> Bool -> IO ()
setBordered nsColorWell  value =
  sendMsg nsColorWell (mkSelector "setBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- color@
color :: IsNSColorWell nsColorWell => nsColorWell -> IO (Id NSColor)
color nsColorWell  =
  sendMsg nsColorWell (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsNSColorWell nsColorWell, IsNSColor value) => nsColorWell -> value -> IO ()
setColor nsColorWell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsColorWell (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- colorWellStyle@
colorWellStyle :: IsNSColorWell nsColorWell => nsColorWell -> IO NSColorWellStyle
colorWellStyle nsColorWell  =
  fmap (coerce :: CLong -> NSColorWellStyle) $ sendMsg nsColorWell (mkSelector "colorWellStyle") retCLong []

-- | @- setColorWellStyle:@
setColorWellStyle :: IsNSColorWell nsColorWell => nsColorWell -> NSColorWellStyle -> IO ()
setColorWellStyle nsColorWell  value =
  sendMsg nsColorWell (mkSelector "setColorWellStyle:") retVoid [argCLong (coerce value)]

-- | The image that appears on the button portion of the expanded control. This property only applicable when @colorWellStyle@ is @NSColorWellStyleExpanded@.
--
-- ObjC selector: @- pulldownTarget@
pulldownTarget :: IsNSColorWell nsColorWell => nsColorWell -> IO RawId
pulldownTarget nsColorWell  =
  fmap (RawId . castPtr) $ sendMsg nsColorWell (mkSelector "pulldownTarget") (retPtr retVoid) []

-- | The image that appears on the button portion of the expanded control. This property only applicable when @colorWellStyle@ is @NSColorWellStyleExpanded@.
--
-- ObjC selector: @- setPulldownTarget:@
setPulldownTarget :: IsNSColorWell nsColorWell => nsColorWell -> RawId -> IO ()
setPulldownTarget nsColorWell  value =
  sendMsg nsColorWell (mkSelector "setPulldownTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The target which @pulldownAction@ is sent to. This property only applicable when @colorWellStyle@ is @NSColorWellStyleExpanded@ or @NSColorWellStyleMinimal@.
--
-- ObjC selector: @- pulldownAction@
pulldownAction :: IsNSColorWell nsColorWell => nsColorWell -> IO Selector
pulldownAction nsColorWell  =
  fmap (Selector . castPtr) $ sendMsg nsColorWell (mkSelector "pulldownAction") (retPtr retVoid) []

-- | The target which @pulldownAction@ is sent to. This property only applicable when @colorWellStyle@ is @NSColorWellStyleExpanded@ or @NSColorWellStyleMinimal@.
--
-- ObjC selector: @- setPulldownAction:@
setPulldownAction :: IsNSColorWell nsColorWell => nsColorWell -> Selector -> IO ()
setPulldownAction nsColorWell  value =
  sendMsg nsColorWell (mkSelector "setPulldownAction:") retVoid [argPtr (unSelector value)]

-- | Controls alpha support for the current color well, and the visibility of alpha slider in the color panel. When @NSColor.ignoresAlpha@ (deprecated) is set to @YES@, this property will always return @NO@ and alpha is not supported globally.
--
-- ObjC selector: @- supportsAlpha@
supportsAlpha :: IsNSColorWell nsColorWell => nsColorWell -> IO Bool
supportsAlpha nsColorWell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorWell (mkSelector "supportsAlpha") retCULong []

-- | Controls alpha support for the current color well, and the visibility of alpha slider in the color panel. When @NSColor.ignoresAlpha@ (deprecated) is set to @YES@, this property will always return @NO@ and alpha is not supported globally.
--
-- ObjC selector: @- setSupportsAlpha:@
setSupportsAlpha :: IsNSColorWell nsColorWell => nsColorWell -> Bool -> IO ()
setSupportsAlpha nsColorWell  value =
  sendMsg nsColorWell (mkSelector "setSupportsAlpha:") retVoid [argCULong (if value then 1 else 0)]

-- | The maximum linear exposure a color in this color well can be set to. Defaults to 1 and ignores any value less than 1. If set to a value >= 2, the color picked for this well may have a linear exposure applied to it.
--
-- ObjC selector: @- maximumLinearExposure@
maximumLinearExposure :: IsNSColorWell nsColorWell => nsColorWell -> IO CDouble
maximumLinearExposure nsColorWell  =
  sendMsg nsColorWell (mkSelector "maximumLinearExposure") retCDouble []

-- | The maximum linear exposure a color in this color well can be set to. Defaults to 1 and ignores any value less than 1. If set to a value >= 2, the color picked for this well may have a linear exposure applied to it.
--
-- ObjC selector: @- setMaximumLinearExposure:@
setMaximumLinearExposure :: IsNSColorWell nsColorWell => nsColorWell -> CDouble -> IO ()
setMaximumLinearExposure nsColorWell  value =
  sendMsg nsColorWell (mkSelector "setMaximumLinearExposure:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorWellWithStyle:@
colorWellWithStyleSelector :: Selector
colorWellWithStyleSelector = mkSelector "colorWellWithStyle:"

-- | @Selector@ for @deactivate@
deactivateSelector :: Selector
deactivateSelector = mkSelector "deactivate"

-- | @Selector@ for @activate:@
activateSelector :: Selector
activateSelector = mkSelector "activate:"

-- | @Selector@ for @drawWellInside:@
drawWellInsideSelector :: Selector
drawWellInsideSelector = mkSelector "drawWellInside:"

-- | @Selector@ for @takeColorFrom:@
takeColorFromSelector :: Selector
takeColorFromSelector = mkSelector "takeColorFrom:"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @bordered@
borderedSelector :: Selector
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @colorWellStyle@
colorWellStyleSelector :: Selector
colorWellStyleSelector = mkSelector "colorWellStyle"

-- | @Selector@ for @setColorWellStyle:@
setColorWellStyleSelector :: Selector
setColorWellStyleSelector = mkSelector "setColorWellStyle:"

-- | @Selector@ for @pulldownTarget@
pulldownTargetSelector :: Selector
pulldownTargetSelector = mkSelector "pulldownTarget"

-- | @Selector@ for @setPulldownTarget:@
setPulldownTargetSelector :: Selector
setPulldownTargetSelector = mkSelector "setPulldownTarget:"

-- | @Selector@ for @pulldownAction@
pulldownActionSelector :: Selector
pulldownActionSelector = mkSelector "pulldownAction"

-- | @Selector@ for @setPulldownAction:@
setPulldownActionSelector :: Selector
setPulldownActionSelector = mkSelector "setPulldownAction:"

-- | @Selector@ for @supportsAlpha@
supportsAlphaSelector :: Selector
supportsAlphaSelector = mkSelector "supportsAlpha"

-- | @Selector@ for @setSupportsAlpha:@
setSupportsAlphaSelector :: Selector
setSupportsAlphaSelector = mkSelector "setSupportsAlpha:"

-- | @Selector@ for @maximumLinearExposure@
maximumLinearExposureSelector :: Selector
maximumLinearExposureSelector = mkSelector "maximumLinearExposure"

-- | @Selector@ for @setMaximumLinearExposure:@
setMaximumLinearExposureSelector :: Selector
setMaximumLinearExposureSelector = mkSelector "setMaximumLinearExposure:"

