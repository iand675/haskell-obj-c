{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSButtonTouchBarItem@.
module ObjC.AppKit.NSButtonTouchBarItem
  ( NSButtonTouchBarItem
  , IsNSButtonTouchBarItem(..)
  , buttonTouchBarItemWithIdentifier_title_target_action
  , buttonTouchBarItemWithIdentifier_image_target_action
  , buttonTouchBarItemWithIdentifier_title_image_target_action
  , title
  , setTitle
  , image
  , setImage
  , bezelColor
  , setBezelColor
  , target
  , setTarget
  , action
  , setAction
  , enabled
  , setEnabled
  , customizationLabel
  , setCustomizationLabel
  , actionSelector
  , bezelColorSelector
  , buttonTouchBarItemWithIdentifier_image_target_actionSelector
  , buttonTouchBarItemWithIdentifier_title_image_target_actionSelector
  , buttonTouchBarItemWithIdentifier_title_target_actionSelector
  , customizationLabelSelector
  , enabledSelector
  , imageSelector
  , setActionSelector
  , setBezelColorSelector
  , setCustomizationLabelSelector
  , setEnabledSelector
  , setImageSelector
  , setTargetSelector
  , setTitleSelector
  , targetSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ buttonTouchBarItemWithIdentifier:title:target:action:@
buttonTouchBarItemWithIdentifier_title_target_action :: (IsNSString identifier, IsNSString title) => identifier -> title -> RawId -> Sel -> IO (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_title_target_action identifier title target action =
  do
    cls' <- getRequiredClass "NSButtonTouchBarItem"
    sendClassMessage cls' buttonTouchBarItemWithIdentifier_title_target_actionSelector (toNSString identifier) (toNSString title) target action

-- | @+ buttonTouchBarItemWithIdentifier:image:target:action:@
buttonTouchBarItemWithIdentifier_image_target_action :: (IsNSString identifier, IsNSImage image) => identifier -> image -> RawId -> Sel -> IO (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_image_target_action identifier image target action =
  do
    cls' <- getRequiredClass "NSButtonTouchBarItem"
    sendClassMessage cls' buttonTouchBarItemWithIdentifier_image_target_actionSelector (toNSString identifier) (toNSImage image) target action

-- | @+ buttonTouchBarItemWithIdentifier:title:image:target:action:@
buttonTouchBarItemWithIdentifier_title_image_target_action :: (IsNSString identifier, IsNSString title, IsNSImage image) => identifier -> title -> image -> RawId -> Sel -> IO (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_title_image_target_action identifier title image target action =
  do
    cls' <- getRequiredClass "NSButtonTouchBarItem"
    sendClassMessage cls' buttonTouchBarItemWithIdentifier_title_image_target_actionSelector (toNSString identifier) (toNSString title) (toNSImage image) target action

-- | @- title@
title :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO (Id NSString)
title nsButtonTouchBarItem =
  sendMessage nsButtonTouchBarItem titleSelector

-- | @- setTitle:@
setTitle :: (IsNSButtonTouchBarItem nsButtonTouchBarItem, IsNSString value) => nsButtonTouchBarItem -> value -> IO ()
setTitle nsButtonTouchBarItem value =
  sendMessage nsButtonTouchBarItem setTitleSelector (toNSString value)

-- | @- image@
image :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO (Id NSImage)
image nsButtonTouchBarItem =
  sendMessage nsButtonTouchBarItem imageSelector

-- | @- setImage:@
setImage :: (IsNSButtonTouchBarItem nsButtonTouchBarItem, IsNSImage value) => nsButtonTouchBarItem -> value -> IO ()
setImage nsButtonTouchBarItem value =
  sendMessage nsButtonTouchBarItem setImageSelector (toNSImage value)

-- | @- bezelColor@
bezelColor :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO (Id NSColor)
bezelColor nsButtonTouchBarItem =
  sendMessage nsButtonTouchBarItem bezelColorSelector

-- | @- setBezelColor:@
setBezelColor :: (IsNSButtonTouchBarItem nsButtonTouchBarItem, IsNSColor value) => nsButtonTouchBarItem -> value -> IO ()
setBezelColor nsButtonTouchBarItem value =
  sendMessage nsButtonTouchBarItem setBezelColorSelector (toNSColor value)

-- | @- target@
target :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO RawId
target nsButtonTouchBarItem =
  sendMessage nsButtonTouchBarItem targetSelector

-- | @- setTarget:@
setTarget :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> RawId -> IO ()
setTarget nsButtonTouchBarItem value =
  sendMessage nsButtonTouchBarItem setTargetSelector value

-- | @- action@
action :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO Sel
action nsButtonTouchBarItem =
  sendMessage nsButtonTouchBarItem actionSelector

-- | @- setAction:@
setAction :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> Sel -> IO ()
setAction nsButtonTouchBarItem value =
  sendMessage nsButtonTouchBarItem setActionSelector value

-- | @- enabled@
enabled :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO Bool
enabled nsButtonTouchBarItem =
  sendMessage nsButtonTouchBarItem enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> Bool -> IO ()
setEnabled nsButtonTouchBarItem value =
  sendMessage nsButtonTouchBarItem setEnabledSelector value

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO (Id NSString)
customizationLabel nsButtonTouchBarItem =
  sendMessage nsButtonTouchBarItem customizationLabelSelector

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSButtonTouchBarItem nsButtonTouchBarItem, IsNSString value) => nsButtonTouchBarItem -> value -> IO ()
setCustomizationLabel nsButtonTouchBarItem value =
  sendMessage nsButtonTouchBarItem setCustomizationLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonTouchBarItemWithIdentifier:title:target:action:@
buttonTouchBarItemWithIdentifier_title_target_actionSelector :: Selector '[Id NSString, Id NSString, RawId, Sel] (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_title_target_actionSelector = mkSelector "buttonTouchBarItemWithIdentifier:title:target:action:"

-- | @Selector@ for @buttonTouchBarItemWithIdentifier:image:target:action:@
buttonTouchBarItemWithIdentifier_image_target_actionSelector :: Selector '[Id NSString, Id NSImage, RawId, Sel] (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_image_target_actionSelector = mkSelector "buttonTouchBarItemWithIdentifier:image:target:action:"

-- | @Selector@ for @buttonTouchBarItemWithIdentifier:title:image:target:action:@
buttonTouchBarItemWithIdentifier_title_image_target_actionSelector :: Selector '[Id NSString, Id NSString, Id NSImage, RawId, Sel] (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_title_image_target_actionSelector = mkSelector "buttonTouchBarItemWithIdentifier:title:image:target:action:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @bezelColor@
bezelColorSelector :: Selector '[] (Id NSColor)
bezelColorSelector = mkSelector "bezelColor"

-- | @Selector@ for @setBezelColor:@
setBezelColorSelector :: Selector '[Id NSColor] ()
setBezelColorSelector = mkSelector "setBezelColor:"

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

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector '[Id NSString] ()
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

