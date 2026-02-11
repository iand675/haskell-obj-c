{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSColorPickerTouchBarItem@.
module ObjC.AppKit.NSColorPickerTouchBarItem
  ( NSColorPickerTouchBarItem
  , IsNSColorPickerTouchBarItem(..)
  , colorPickerWithIdentifier
  , textColorPickerWithIdentifier
  , strokeColorPickerWithIdentifier
  , colorPickerWithIdentifier_buttonImage
  , color
  , setColor
  , showsAlpha
  , setShowsAlpha
  , customizationLabel
  , setCustomizationLabel
  , target
  , setTarget
  , action
  , setAction
  , enabled
  , setEnabled
  , colorPickerWithIdentifierSelector
  , textColorPickerWithIdentifierSelector
  , strokeColorPickerWithIdentifierSelector
  , colorPickerWithIdentifier_buttonImageSelector
  , colorSelector
  , setColorSelector
  , showsAlphaSelector
  , setShowsAlphaSelector
  , customizationLabelSelector
  , setCustomizationLabelSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , enabledSelector
  , setEnabledSelector


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
import ObjC.Foundation.Internal.Classes

-- | Creates a bar item containing a button with the standard color picker icon that invokes the color picker.
--
-- ObjC selector: @+ colorPickerWithIdentifier:@
colorPickerWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSColorPickerTouchBarItem)
colorPickerWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSColorPickerTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "colorPickerWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a bar item containing a button with the standard text color picker icon that invokes the color picker. Should be used when the item is used for picking text colors.
--
-- ObjC selector: @+ textColorPickerWithIdentifier:@
textColorPickerWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSColorPickerTouchBarItem)
textColorPickerWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSColorPickerTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "textColorPickerWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a bar item containing a button with the standard stroke color picker icon that invokes the color picker. Should be used when the item is used for picking stroke colors.
--
-- ObjC selector: @+ strokeColorPickerWithIdentifier:@
strokeColorPickerWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSColorPickerTouchBarItem)
strokeColorPickerWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSColorPickerTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "strokeColorPickerWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ colorPickerWithIdentifier:buttonImage:@
colorPickerWithIdentifier_buttonImage :: (IsNSString identifier, IsNSImage image) => identifier -> image -> IO (Id NSColorPickerTouchBarItem)
colorPickerWithIdentifier_buttonImage identifier image =
  do
    cls' <- getRequiredClass "NSColorPickerTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr image $ \raw_image ->
        sendClassMsg cls' (mkSelector "colorPickerWithIdentifier:buttonImage:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- | @- color@
color :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO (Id NSColor)
color nsColorPickerTouchBarItem  =
  sendMsg nsColorPickerTouchBarItem (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem, IsNSColor value) => nsColorPickerTouchBarItem -> value -> IO ()
setColor nsColorPickerTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsColorPickerTouchBarItem (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether or not the picker should allow picking a color with non-1.0 alpha. Defaults to @!NSColor.ignoresAlpha@.
--
-- ObjC selector: @- showsAlpha@
showsAlpha :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO Bool
showsAlpha nsColorPickerTouchBarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorPickerTouchBarItem (mkSelector "showsAlpha") retCULong []

-- | Whether or not the picker should allow picking a color with non-1.0 alpha. Defaults to @!NSColor.ignoresAlpha@.
--
-- ObjC selector: @- setShowsAlpha:@
setShowsAlpha :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> Bool -> IO ()
setShowsAlpha nsColorPickerTouchBarItem  value =
  sendMsg nsColorPickerTouchBarItem (mkSelector "setShowsAlpha:") retVoid [argCULong (if value then 1 else 0)]

-- | The localized string labelling this item during user customization. The default value is the localized string of "Color Picker".
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO (Id NSString)
customizationLabel nsColorPickerTouchBarItem  =
  sendMsg nsColorPickerTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized string labelling this item during user customization. The default value is the localized string of "Color Picker".
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem, IsNSString value) => nsColorPickerTouchBarItem -> value -> IO ()
setCustomizationLabel nsColorPickerTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsColorPickerTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- target@
target :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO RawId
target nsColorPickerTouchBarItem  =
  fmap (RawId . castPtr) $ sendMsg nsColorPickerTouchBarItem (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> RawId -> IO ()
setTarget nsColorPickerTouchBarItem  value =
  sendMsg nsColorPickerTouchBarItem (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO Selector
action nsColorPickerTouchBarItem  =
  fmap (Selector . castPtr) $ sendMsg nsColorPickerTouchBarItem (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> Selector -> IO ()
setAction nsColorPickerTouchBarItem  value =
  sendMsg nsColorPickerTouchBarItem (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | Enables or disabled the color picker. If it is currently being shown in a popover, it will be dismissed.
--
-- ObjC selector: @- enabled@
enabled :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO Bool
enabled nsColorPickerTouchBarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsColorPickerTouchBarItem (mkSelector "enabled") retCULong []

-- | Enables or disabled the color picker. If it is currently being shown in a popover, it will be dismissed.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> Bool -> IO ()
setEnabled nsColorPickerTouchBarItem  value =
  sendMsg nsColorPickerTouchBarItem (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorPickerWithIdentifier:@
colorPickerWithIdentifierSelector :: Selector
colorPickerWithIdentifierSelector = mkSelector "colorPickerWithIdentifier:"

-- | @Selector@ for @textColorPickerWithIdentifier:@
textColorPickerWithIdentifierSelector :: Selector
textColorPickerWithIdentifierSelector = mkSelector "textColorPickerWithIdentifier:"

-- | @Selector@ for @strokeColorPickerWithIdentifier:@
strokeColorPickerWithIdentifierSelector :: Selector
strokeColorPickerWithIdentifierSelector = mkSelector "strokeColorPickerWithIdentifier:"

-- | @Selector@ for @colorPickerWithIdentifier:buttonImage:@
colorPickerWithIdentifier_buttonImageSelector :: Selector
colorPickerWithIdentifier_buttonImageSelector = mkSelector "colorPickerWithIdentifier:buttonImage:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @showsAlpha@
showsAlphaSelector :: Selector
showsAlphaSelector = mkSelector "showsAlpha"

-- | @Selector@ for @setShowsAlpha:@
setShowsAlphaSelector :: Selector
setShowsAlphaSelector = mkSelector "setShowsAlpha:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

