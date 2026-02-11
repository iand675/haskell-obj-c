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
  , buttonTouchBarItemWithIdentifier_title_target_actionSelector
  , buttonTouchBarItemWithIdentifier_image_target_actionSelector
  , buttonTouchBarItemWithIdentifier_title_image_target_actionSelector
  , titleSelector
  , setTitleSelector
  , imageSelector
  , setImageSelector
  , bezelColorSelector
  , setBezelColorSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , enabledSelector
  , setEnabledSelector
  , customizationLabelSelector
  , setCustomizationLabelSelector


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

-- | @+ buttonTouchBarItemWithIdentifier:title:target:action:@
buttonTouchBarItemWithIdentifier_title_target_action :: (IsNSString identifier, IsNSString title) => identifier -> title -> RawId -> Selector -> IO (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_title_target_action identifier title target action =
  do
    cls' <- getRequiredClass "NSButtonTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr title $ \raw_title ->
        sendClassMsg cls' (mkSelector "buttonTouchBarItemWithIdentifier:title:target:action:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | @+ buttonTouchBarItemWithIdentifier:image:target:action:@
buttonTouchBarItemWithIdentifier_image_target_action :: (IsNSString identifier, IsNSImage image) => identifier -> image -> RawId -> Selector -> IO (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_image_target_action identifier image target action =
  do
    cls' <- getRequiredClass "NSButtonTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr image $ \raw_image ->
        sendClassMsg cls' (mkSelector "buttonTouchBarItemWithIdentifier:image:target:action:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | @+ buttonTouchBarItemWithIdentifier:title:image:target:action:@
buttonTouchBarItemWithIdentifier_title_image_target_action :: (IsNSString identifier, IsNSString title, IsNSImage image) => identifier -> title -> image -> RawId -> Selector -> IO (Id NSButtonTouchBarItem)
buttonTouchBarItemWithIdentifier_title_image_target_action identifier title image target action =
  do
    cls' <- getRequiredClass "NSButtonTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr title $ \raw_title ->
        withObjCPtr image $ \raw_image ->
          sendClassMsg cls' (mkSelector "buttonTouchBarItemWithIdentifier:title:image:target:action:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | @- title@
title :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO (Id NSString)
title nsButtonTouchBarItem  =
  sendMsg nsButtonTouchBarItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSButtonTouchBarItem nsButtonTouchBarItem, IsNSString value) => nsButtonTouchBarItem -> value -> IO ()
setTitle nsButtonTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsButtonTouchBarItem (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- image@
image :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO (Id NSImage)
image nsButtonTouchBarItem  =
  sendMsg nsButtonTouchBarItem (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSButtonTouchBarItem nsButtonTouchBarItem, IsNSImage value) => nsButtonTouchBarItem -> value -> IO ()
setImage nsButtonTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsButtonTouchBarItem (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bezelColor@
bezelColor :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO (Id NSColor)
bezelColor nsButtonTouchBarItem  =
  sendMsg nsButtonTouchBarItem (mkSelector "bezelColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBezelColor:@
setBezelColor :: (IsNSButtonTouchBarItem nsButtonTouchBarItem, IsNSColor value) => nsButtonTouchBarItem -> value -> IO ()
setBezelColor nsButtonTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsButtonTouchBarItem (mkSelector "setBezelColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- target@
target :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO RawId
target nsButtonTouchBarItem  =
  fmap (RawId . castPtr) $ sendMsg nsButtonTouchBarItem (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> RawId -> IO ()
setTarget nsButtonTouchBarItem  value =
  sendMsg nsButtonTouchBarItem (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO Selector
action nsButtonTouchBarItem  =
  fmap (Selector . castPtr) $ sendMsg nsButtonTouchBarItem (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> Selector -> IO ()
setAction nsButtonTouchBarItem  value =
  sendMsg nsButtonTouchBarItem (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- enabled@
enabled :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO Bool
enabled nsButtonTouchBarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButtonTouchBarItem (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> Bool -> IO ()
setEnabled nsButtonTouchBarItem  value =
  sendMsg nsButtonTouchBarItem (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- customizationLabel@
customizationLabel :: IsNSButtonTouchBarItem nsButtonTouchBarItem => nsButtonTouchBarItem -> IO (Id NSString)
customizationLabel nsButtonTouchBarItem  =
  sendMsg nsButtonTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized string labelling this item during user customization. The default value is empty string.
--
-- ObjC selector: @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSButtonTouchBarItem nsButtonTouchBarItem, IsNSString value) => nsButtonTouchBarItem -> value -> IO ()
setCustomizationLabel nsButtonTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsButtonTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonTouchBarItemWithIdentifier:title:target:action:@
buttonTouchBarItemWithIdentifier_title_target_actionSelector :: Selector
buttonTouchBarItemWithIdentifier_title_target_actionSelector = mkSelector "buttonTouchBarItemWithIdentifier:title:target:action:"

-- | @Selector@ for @buttonTouchBarItemWithIdentifier:image:target:action:@
buttonTouchBarItemWithIdentifier_image_target_actionSelector :: Selector
buttonTouchBarItemWithIdentifier_image_target_actionSelector = mkSelector "buttonTouchBarItemWithIdentifier:image:target:action:"

-- | @Selector@ for @buttonTouchBarItemWithIdentifier:title:image:target:action:@
buttonTouchBarItemWithIdentifier_title_image_target_actionSelector :: Selector
buttonTouchBarItemWithIdentifier_title_image_target_actionSelector = mkSelector "buttonTouchBarItemWithIdentifier:title:image:target:action:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @bezelColor@
bezelColorSelector :: Selector
bezelColorSelector = mkSelector "bezelColor"

-- | @Selector@ for @setBezelColor:@
setBezelColorSelector :: Selector
setBezelColorSelector = mkSelector "setBezelColor:"

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

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

