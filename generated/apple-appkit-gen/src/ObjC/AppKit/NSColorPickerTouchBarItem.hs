{-# LANGUAGE DataKinds #-}
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
  , allowedColorSpaces
  , setAllowedColorSpaces
  , colorList
  , setColorList
  , customizationLabel
  , setCustomizationLabel
  , target
  , setTarget
  , action
  , setAction
  , enabled
  , setEnabled
  , actionSelector
  , allowedColorSpacesSelector
  , colorListSelector
  , colorPickerWithIdentifierSelector
  , colorPickerWithIdentifier_buttonImageSelector
  , colorSelector
  , customizationLabelSelector
  , enabledSelector
  , setActionSelector
  , setAllowedColorSpacesSelector
  , setColorListSelector
  , setColorSelector
  , setCustomizationLabelSelector
  , setEnabledSelector
  , setShowsAlphaSelector
  , setTargetSelector
  , showsAlphaSelector
  , strokeColorPickerWithIdentifierSelector
  , targetSelector
  , textColorPickerWithIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' colorPickerWithIdentifierSelector (toNSString identifier)

-- | Creates a bar item containing a button with the standard text color picker icon that invokes the color picker. Should be used when the item is used for picking text colors.
--
-- ObjC selector: @+ textColorPickerWithIdentifier:@
textColorPickerWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSColorPickerTouchBarItem)
textColorPickerWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSColorPickerTouchBarItem"
    sendClassMessage cls' textColorPickerWithIdentifierSelector (toNSString identifier)

-- | Creates a bar item containing a button with the standard stroke color picker icon that invokes the color picker. Should be used when the item is used for picking stroke colors.
--
-- ObjC selector: @+ strokeColorPickerWithIdentifier:@
strokeColorPickerWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSColorPickerTouchBarItem)
strokeColorPickerWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSColorPickerTouchBarItem"
    sendClassMessage cls' strokeColorPickerWithIdentifierSelector (toNSString identifier)

-- | @+ colorPickerWithIdentifier:buttonImage:@
colorPickerWithIdentifier_buttonImage :: (IsNSString identifier, IsNSImage image) => identifier -> image -> IO (Id NSColorPickerTouchBarItem)
colorPickerWithIdentifier_buttonImage identifier image =
  do
    cls' <- getRequiredClass "NSColorPickerTouchBarItem"
    sendClassMessage cls' colorPickerWithIdentifier_buttonImageSelector (toNSString identifier) (toNSImage image)

-- | @- color@
color :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO (Id NSColor)
color nsColorPickerTouchBarItem =
  sendMessage nsColorPickerTouchBarItem colorSelector

-- | @- setColor:@
setColor :: (IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem, IsNSColor value) => nsColorPickerTouchBarItem -> value -> IO ()
setColor nsColorPickerTouchBarItem value =
  sendMessage nsColorPickerTouchBarItem setColorSelector (toNSColor value)

-- | Whether or not the picker should allow picking a color with non-1.0 alpha. Defaults to @!NSColor.ignoresAlpha@.
--
-- ObjC selector: @- showsAlpha@
showsAlpha :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO Bool
showsAlpha nsColorPickerTouchBarItem =
  sendMessage nsColorPickerTouchBarItem showsAlphaSelector

-- | Whether or not the picker should allow picking a color with non-1.0 alpha. Defaults to @!NSColor.ignoresAlpha@.
--
-- ObjC selector: @- setShowsAlpha:@
setShowsAlpha :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> Bool -> IO ()
setShowsAlpha nsColorPickerTouchBarItem value =
  sendMessage nsColorPickerTouchBarItem setShowsAlphaSelector value

-- | Controls the color spaces that the receiver is able to produce. If a color outside of the allowed spaces are displayed or selected, it will first be converted to the first color space in the array. @nil@ signifies any color space is allowed. Empty array is an invalid value and will raise an exception if set. Defaults to @nil@.
--
-- ObjC selector: @- allowedColorSpaces@
allowedColorSpaces :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO (Id NSArray)
allowedColorSpaces nsColorPickerTouchBarItem =
  sendMessage nsColorPickerTouchBarItem allowedColorSpacesSelector

-- | Controls the color spaces that the receiver is able to produce. If a color outside of the allowed spaces are displayed or selected, it will first be converted to the first color space in the array. @nil@ signifies any color space is allowed. Empty array is an invalid value and will raise an exception if set. Defaults to @nil@.
--
-- ObjC selector: @- setAllowedColorSpaces:@
setAllowedColorSpaces :: (IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem, IsNSArray value) => nsColorPickerTouchBarItem -> value -> IO ()
setAllowedColorSpaces nsColorPickerTouchBarItem value =
  sendMessage nsColorPickerTouchBarItem setAllowedColorSpacesSelector (toNSArray value)

-- | The color list displayed in the list color picker. Defaults to the standard system color list. Setting a custom color list will disable the additional tints/shades that appear on long-press.
--
-- ObjC selector: @- colorList@
colorList :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO (Id NSColorList)
colorList nsColorPickerTouchBarItem =
  sendMessage nsColorPickerTouchBarItem colorListSelector

-- | The color list displayed in the list color picker. Defaults to the standard system color list. Setting a custom color list will disable the additional tints/shades that appear on long-press.
--
-- ObjC selector: @- setColorList:@
setColorList :: (IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem, IsNSColorList value) => nsColorPickerTouchBarItem -> value -> IO ()
setColorList nsColorPickerTouchBarItem value =
  sendMessage nsColorPickerTouchBarItem setColorListSelector (toNSColorList value)

-- | The localized string labelling this item during user customization. The default value is the localized string of "Color Picker".
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO (Id NSString)
customizationLabel nsColorPickerTouchBarItem =
  sendMessage nsColorPickerTouchBarItem customizationLabelSelector

-- | The localized string labelling this item during user customization. The default value is the localized string of "Color Picker".
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem, IsNSString value) => nsColorPickerTouchBarItem -> value -> IO ()
setCustomizationLabel nsColorPickerTouchBarItem value =
  sendMessage nsColorPickerTouchBarItem setCustomizationLabelSelector (toNSString value)

-- | @- target@
target :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO RawId
target nsColorPickerTouchBarItem =
  sendMessage nsColorPickerTouchBarItem targetSelector

-- | @- setTarget:@
setTarget :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> RawId -> IO ()
setTarget nsColorPickerTouchBarItem value =
  sendMessage nsColorPickerTouchBarItem setTargetSelector value

-- | @- action@
action :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO Sel
action nsColorPickerTouchBarItem =
  sendMessage nsColorPickerTouchBarItem actionSelector

-- | @- setAction:@
setAction :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> Sel -> IO ()
setAction nsColorPickerTouchBarItem value =
  sendMessage nsColorPickerTouchBarItem setActionSelector value

-- | Enables or disabled the color picker. If it is currently being shown in a popover, it will be dismissed.
--
-- ObjC selector: @- enabled@
enabled :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> IO Bool
enabled nsColorPickerTouchBarItem =
  sendMessage nsColorPickerTouchBarItem enabledSelector

-- | Enables or disabled the color picker. If it is currently being shown in a popover, it will be dismissed.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNSColorPickerTouchBarItem nsColorPickerTouchBarItem => nsColorPickerTouchBarItem -> Bool -> IO ()
setEnabled nsColorPickerTouchBarItem value =
  sendMessage nsColorPickerTouchBarItem setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorPickerWithIdentifier:@
colorPickerWithIdentifierSelector :: Selector '[Id NSString] (Id NSColorPickerTouchBarItem)
colorPickerWithIdentifierSelector = mkSelector "colorPickerWithIdentifier:"

-- | @Selector@ for @textColorPickerWithIdentifier:@
textColorPickerWithIdentifierSelector :: Selector '[Id NSString] (Id NSColorPickerTouchBarItem)
textColorPickerWithIdentifierSelector = mkSelector "textColorPickerWithIdentifier:"

-- | @Selector@ for @strokeColorPickerWithIdentifier:@
strokeColorPickerWithIdentifierSelector :: Selector '[Id NSString] (Id NSColorPickerTouchBarItem)
strokeColorPickerWithIdentifierSelector = mkSelector "strokeColorPickerWithIdentifier:"

-- | @Selector@ for @colorPickerWithIdentifier:buttonImage:@
colorPickerWithIdentifier_buttonImageSelector :: Selector '[Id NSString, Id NSImage] (Id NSColorPickerTouchBarItem)
colorPickerWithIdentifier_buttonImageSelector = mkSelector "colorPickerWithIdentifier:buttonImage:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Id NSColor)
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Id NSColor] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @showsAlpha@
showsAlphaSelector :: Selector '[] Bool
showsAlphaSelector = mkSelector "showsAlpha"

-- | @Selector@ for @setShowsAlpha:@
setShowsAlphaSelector :: Selector '[Bool] ()
setShowsAlphaSelector = mkSelector "setShowsAlpha:"

-- | @Selector@ for @allowedColorSpaces@
allowedColorSpacesSelector :: Selector '[] (Id NSArray)
allowedColorSpacesSelector = mkSelector "allowedColorSpaces"

-- | @Selector@ for @setAllowedColorSpaces:@
setAllowedColorSpacesSelector :: Selector '[Id NSArray] ()
setAllowedColorSpacesSelector = mkSelector "setAllowedColorSpaces:"

-- | @Selector@ for @colorList@
colorListSelector :: Selector '[] (Id NSColorList)
colorListSelector = mkSelector "colorList"

-- | @Selector@ for @setColorList:@
setColorListSelector :: Selector '[Id NSColorList] ()
setColorListSelector = mkSelector "setColorList:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector '[Id NSString] ()
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] Sel
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

