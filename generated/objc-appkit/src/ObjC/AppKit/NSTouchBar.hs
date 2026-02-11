{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTouchBar@.
module ObjC.AppKit.NSTouchBar
  ( NSTouchBar
  , IsNSTouchBar(..)
  , init_
  , initWithCoder
  , itemForIdentifier
  , customizationIdentifier
  , setCustomizationIdentifier
  , customizationAllowedItemIdentifiers
  , setCustomizationAllowedItemIdentifiers
  , customizationRequiredItemIdentifiers
  , setCustomizationRequiredItemIdentifiers
  , defaultItemIdentifiers
  , setDefaultItemIdentifiers
  , itemIdentifiers
  , principalItemIdentifier
  , setPrincipalItemIdentifier
  , escapeKeyReplacementItemIdentifier
  , setEscapeKeyReplacementItemIdentifier
  , templateItems
  , setTemplateItems
  , visible
  , automaticCustomizeTouchBarMenuItemEnabled
  , setAutomaticCustomizeTouchBarMenuItemEnabled
  , initSelector
  , initWithCoderSelector
  , itemForIdentifierSelector
  , customizationIdentifierSelector
  , setCustomizationIdentifierSelector
  , customizationAllowedItemIdentifiersSelector
  , setCustomizationAllowedItemIdentifiersSelector
  , customizationRequiredItemIdentifiersSelector
  , setCustomizationRequiredItemIdentifiersSelector
  , defaultItemIdentifiersSelector
  , setDefaultItemIdentifiersSelector
  , itemIdentifiersSelector
  , principalItemIdentifierSelector
  , setPrincipalItemIdentifierSelector
  , escapeKeyReplacementItemIdentifierSelector
  , setEscapeKeyReplacementItemIdentifierSelector
  , templateItemsSelector
  , setTemplateItemsSelector
  , visibleSelector
  , automaticCustomizeTouchBarMenuItemEnabledSelector
  , setAutomaticCustomizeTouchBarMenuItemEnabledSelector


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

-- | @- init@
init_ :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSTouchBar)
init_ nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTouchBar nsTouchBar, IsNSCoder coder) => nsTouchBar -> coder -> IO (Id NSTouchBar)
initWithCoder nsTouchBar  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsTouchBar (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- itemForIdentifier:@
itemForIdentifier :: (IsNSTouchBar nsTouchBar, IsNSString identifier) => nsTouchBar -> identifier -> IO (Id NSTouchBarItem)
itemForIdentifier nsTouchBar  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsTouchBar (mkSelector "itemForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- customizationIdentifier@
customizationIdentifier :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSString)
customizationIdentifier nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "customizationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomizationIdentifier:@
setCustomizationIdentifier :: (IsNSTouchBar nsTouchBar, IsNSString value) => nsTouchBar -> value -> IO ()
setCustomizationIdentifier nsTouchBar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTouchBar (mkSelector "setCustomizationIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- customizationAllowedItemIdentifiers@
customizationAllowedItemIdentifiers :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSArray)
customizationAllowedItemIdentifiers nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "customizationAllowedItemIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomizationAllowedItemIdentifiers:@
setCustomizationAllowedItemIdentifiers :: (IsNSTouchBar nsTouchBar, IsNSArray value) => nsTouchBar -> value -> IO ()
setCustomizationAllowedItemIdentifiers nsTouchBar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTouchBar (mkSelector "setCustomizationAllowedItemIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- customizationRequiredItemIdentifiers@
customizationRequiredItemIdentifiers :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSArray)
customizationRequiredItemIdentifiers nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "customizationRequiredItemIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomizationRequiredItemIdentifiers:@
setCustomizationRequiredItemIdentifiers :: (IsNSTouchBar nsTouchBar, IsNSArray value) => nsTouchBar -> value -> IO ()
setCustomizationRequiredItemIdentifiers nsTouchBar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTouchBar (mkSelector "setCustomizationRequiredItemIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultItemIdentifiers@
defaultItemIdentifiers :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSArray)
defaultItemIdentifiers nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "defaultItemIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultItemIdentifiers:@
setDefaultItemIdentifiers :: (IsNSTouchBar nsTouchBar, IsNSArray value) => nsTouchBar -> value -> IO ()
setDefaultItemIdentifiers nsTouchBar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTouchBar (mkSelector "setDefaultItemIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- itemIdentifiers@
itemIdentifiers :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSArray)
itemIdentifiers nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "itemIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- principalItemIdentifier@
principalItemIdentifier :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSString)
principalItemIdentifier nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "principalItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrincipalItemIdentifier:@
setPrincipalItemIdentifier :: (IsNSTouchBar nsTouchBar, IsNSString value) => nsTouchBar -> value -> IO ()
setPrincipalItemIdentifier nsTouchBar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTouchBar (mkSelector "setPrincipalItemIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- escapeKeyReplacementItemIdentifier@
escapeKeyReplacementItemIdentifier :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSString)
escapeKeyReplacementItemIdentifier nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "escapeKeyReplacementItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEscapeKeyReplacementItemIdentifier:@
setEscapeKeyReplacementItemIdentifier :: (IsNSTouchBar nsTouchBar, IsNSString value) => nsTouchBar -> value -> IO ()
setEscapeKeyReplacementItemIdentifier nsTouchBar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTouchBar (mkSelector "setEscapeKeyReplacementItemIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- templateItems@
templateItems :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSSet)
templateItems nsTouchBar  =
  sendMsg nsTouchBar (mkSelector "templateItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTemplateItems:@
setTemplateItems :: (IsNSTouchBar nsTouchBar, IsNSSet value) => nsTouchBar -> value -> IO ()
setTemplateItems nsTouchBar  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTouchBar (mkSelector "setTemplateItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- visible@
visible :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO Bool
visible nsTouchBar  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTouchBar (mkSelector "visible") retCULong []

-- | @+ automaticCustomizeTouchBarMenuItemEnabled@
automaticCustomizeTouchBarMenuItemEnabled :: IO Bool
automaticCustomizeTouchBarMenuItemEnabled  =
  do
    cls' <- getRequiredClass "NSTouchBar"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticCustomizeTouchBarMenuItemEnabled") retCULong []

-- | @+ setAutomaticCustomizeTouchBarMenuItemEnabled:@
setAutomaticCustomizeTouchBarMenuItemEnabled :: Bool -> IO ()
setAutomaticCustomizeTouchBarMenuItemEnabled value =
  do
    cls' <- getRequiredClass "NSTouchBar"
    sendClassMsg cls' (mkSelector "setAutomaticCustomizeTouchBarMenuItemEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @itemForIdentifier:@
itemForIdentifierSelector :: Selector
itemForIdentifierSelector = mkSelector "itemForIdentifier:"

-- | @Selector@ for @customizationIdentifier@
customizationIdentifierSelector :: Selector
customizationIdentifierSelector = mkSelector "customizationIdentifier"

-- | @Selector@ for @setCustomizationIdentifier:@
setCustomizationIdentifierSelector :: Selector
setCustomizationIdentifierSelector = mkSelector "setCustomizationIdentifier:"

-- | @Selector@ for @customizationAllowedItemIdentifiers@
customizationAllowedItemIdentifiersSelector :: Selector
customizationAllowedItemIdentifiersSelector = mkSelector "customizationAllowedItemIdentifiers"

-- | @Selector@ for @setCustomizationAllowedItemIdentifiers:@
setCustomizationAllowedItemIdentifiersSelector :: Selector
setCustomizationAllowedItemIdentifiersSelector = mkSelector "setCustomizationAllowedItemIdentifiers:"

-- | @Selector@ for @customizationRequiredItemIdentifiers@
customizationRequiredItemIdentifiersSelector :: Selector
customizationRequiredItemIdentifiersSelector = mkSelector "customizationRequiredItemIdentifiers"

-- | @Selector@ for @setCustomizationRequiredItemIdentifiers:@
setCustomizationRequiredItemIdentifiersSelector :: Selector
setCustomizationRequiredItemIdentifiersSelector = mkSelector "setCustomizationRequiredItemIdentifiers:"

-- | @Selector@ for @defaultItemIdentifiers@
defaultItemIdentifiersSelector :: Selector
defaultItemIdentifiersSelector = mkSelector "defaultItemIdentifiers"

-- | @Selector@ for @setDefaultItemIdentifiers:@
setDefaultItemIdentifiersSelector :: Selector
setDefaultItemIdentifiersSelector = mkSelector "setDefaultItemIdentifiers:"

-- | @Selector@ for @itemIdentifiers@
itemIdentifiersSelector :: Selector
itemIdentifiersSelector = mkSelector "itemIdentifiers"

-- | @Selector@ for @principalItemIdentifier@
principalItemIdentifierSelector :: Selector
principalItemIdentifierSelector = mkSelector "principalItemIdentifier"

-- | @Selector@ for @setPrincipalItemIdentifier:@
setPrincipalItemIdentifierSelector :: Selector
setPrincipalItemIdentifierSelector = mkSelector "setPrincipalItemIdentifier:"

-- | @Selector@ for @escapeKeyReplacementItemIdentifier@
escapeKeyReplacementItemIdentifierSelector :: Selector
escapeKeyReplacementItemIdentifierSelector = mkSelector "escapeKeyReplacementItemIdentifier"

-- | @Selector@ for @setEscapeKeyReplacementItemIdentifier:@
setEscapeKeyReplacementItemIdentifierSelector :: Selector
setEscapeKeyReplacementItemIdentifierSelector = mkSelector "setEscapeKeyReplacementItemIdentifier:"

-- | @Selector@ for @templateItems@
templateItemsSelector :: Selector
templateItemsSelector = mkSelector "templateItems"

-- | @Selector@ for @setTemplateItems:@
setTemplateItemsSelector :: Selector
setTemplateItemsSelector = mkSelector "setTemplateItems:"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

-- | @Selector@ for @automaticCustomizeTouchBarMenuItemEnabled@
automaticCustomizeTouchBarMenuItemEnabledSelector :: Selector
automaticCustomizeTouchBarMenuItemEnabledSelector = mkSelector "automaticCustomizeTouchBarMenuItemEnabled"

-- | @Selector@ for @setAutomaticCustomizeTouchBarMenuItemEnabled:@
setAutomaticCustomizeTouchBarMenuItemEnabledSelector :: Selector
setAutomaticCustomizeTouchBarMenuItemEnabledSelector = mkSelector "setAutomaticCustomizeTouchBarMenuItemEnabled:"

