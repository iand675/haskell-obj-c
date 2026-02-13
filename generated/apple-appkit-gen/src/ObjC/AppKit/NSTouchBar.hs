{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , visible
  , automaticCustomizeTouchBarMenuItemEnabled
  , setAutomaticCustomizeTouchBarMenuItemEnabled
  , automaticCustomizeTouchBarMenuItemEnabledSelector
  , customizationAllowedItemIdentifiersSelector
  , customizationIdentifierSelector
  , customizationRequiredItemIdentifiersSelector
  , defaultItemIdentifiersSelector
  , delegateSelector
  , escapeKeyReplacementItemIdentifierSelector
  , initSelector
  , initWithCoderSelector
  , itemForIdentifierSelector
  , itemIdentifiersSelector
  , principalItemIdentifierSelector
  , setAutomaticCustomizeTouchBarMenuItemEnabledSelector
  , setCustomizationAllowedItemIdentifiersSelector
  , setCustomizationIdentifierSelector
  , setCustomizationRequiredItemIdentifiersSelector
  , setDefaultItemIdentifiersSelector
  , setDelegateSelector
  , setEscapeKeyReplacementItemIdentifierSelector
  , setPrincipalItemIdentifierSelector
  , setTemplateItemsSelector
  , templateItemsSelector
  , visibleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSTouchBar)
init_ nsTouchBar =
  sendOwnedMessage nsTouchBar initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSTouchBar nsTouchBar, IsNSCoder coder) => nsTouchBar -> coder -> IO (Id NSTouchBar)
initWithCoder nsTouchBar coder =
  sendOwnedMessage nsTouchBar initWithCoderSelector (toNSCoder coder)

-- | @- itemForIdentifier:@
itemForIdentifier :: (IsNSTouchBar nsTouchBar, IsNSString identifier) => nsTouchBar -> identifier -> IO (Id NSTouchBarItem)
itemForIdentifier nsTouchBar identifier =
  sendMessage nsTouchBar itemForIdentifierSelector (toNSString identifier)

-- | @- customizationIdentifier@
customizationIdentifier :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSString)
customizationIdentifier nsTouchBar =
  sendMessage nsTouchBar customizationIdentifierSelector

-- | @- setCustomizationIdentifier:@
setCustomizationIdentifier :: (IsNSTouchBar nsTouchBar, IsNSString value) => nsTouchBar -> value -> IO ()
setCustomizationIdentifier nsTouchBar value =
  sendMessage nsTouchBar setCustomizationIdentifierSelector (toNSString value)

-- | @- customizationAllowedItemIdentifiers@
customizationAllowedItemIdentifiers :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSArray)
customizationAllowedItemIdentifiers nsTouchBar =
  sendMessage nsTouchBar customizationAllowedItemIdentifiersSelector

-- | @- setCustomizationAllowedItemIdentifiers:@
setCustomizationAllowedItemIdentifiers :: (IsNSTouchBar nsTouchBar, IsNSArray value) => nsTouchBar -> value -> IO ()
setCustomizationAllowedItemIdentifiers nsTouchBar value =
  sendMessage nsTouchBar setCustomizationAllowedItemIdentifiersSelector (toNSArray value)

-- | @- customizationRequiredItemIdentifiers@
customizationRequiredItemIdentifiers :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSArray)
customizationRequiredItemIdentifiers nsTouchBar =
  sendMessage nsTouchBar customizationRequiredItemIdentifiersSelector

-- | @- setCustomizationRequiredItemIdentifiers:@
setCustomizationRequiredItemIdentifiers :: (IsNSTouchBar nsTouchBar, IsNSArray value) => nsTouchBar -> value -> IO ()
setCustomizationRequiredItemIdentifiers nsTouchBar value =
  sendMessage nsTouchBar setCustomizationRequiredItemIdentifiersSelector (toNSArray value)

-- | @- defaultItemIdentifiers@
defaultItemIdentifiers :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSArray)
defaultItemIdentifiers nsTouchBar =
  sendMessage nsTouchBar defaultItemIdentifiersSelector

-- | @- setDefaultItemIdentifiers:@
setDefaultItemIdentifiers :: (IsNSTouchBar nsTouchBar, IsNSArray value) => nsTouchBar -> value -> IO ()
setDefaultItemIdentifiers nsTouchBar value =
  sendMessage nsTouchBar setDefaultItemIdentifiersSelector (toNSArray value)

-- | @- itemIdentifiers@
itemIdentifiers :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSArray)
itemIdentifiers nsTouchBar =
  sendMessage nsTouchBar itemIdentifiersSelector

-- | @- principalItemIdentifier@
principalItemIdentifier :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSString)
principalItemIdentifier nsTouchBar =
  sendMessage nsTouchBar principalItemIdentifierSelector

-- | @- setPrincipalItemIdentifier:@
setPrincipalItemIdentifier :: (IsNSTouchBar nsTouchBar, IsNSString value) => nsTouchBar -> value -> IO ()
setPrincipalItemIdentifier nsTouchBar value =
  sendMessage nsTouchBar setPrincipalItemIdentifierSelector (toNSString value)

-- | @- escapeKeyReplacementItemIdentifier@
escapeKeyReplacementItemIdentifier :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSString)
escapeKeyReplacementItemIdentifier nsTouchBar =
  sendMessage nsTouchBar escapeKeyReplacementItemIdentifierSelector

-- | @- setEscapeKeyReplacementItemIdentifier:@
setEscapeKeyReplacementItemIdentifier :: (IsNSTouchBar nsTouchBar, IsNSString value) => nsTouchBar -> value -> IO ()
setEscapeKeyReplacementItemIdentifier nsTouchBar value =
  sendMessage nsTouchBar setEscapeKeyReplacementItemIdentifierSelector (toNSString value)

-- | @- templateItems@
templateItems :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO (Id NSSet)
templateItems nsTouchBar =
  sendMessage nsTouchBar templateItemsSelector

-- | @- setTemplateItems:@
setTemplateItems :: (IsNSTouchBar nsTouchBar, IsNSSet value) => nsTouchBar -> value -> IO ()
setTemplateItems nsTouchBar value =
  sendMessage nsTouchBar setTemplateItemsSelector (toNSSet value)

-- | @- delegate@
delegate :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO RawId
delegate nsTouchBar =
  sendMessage nsTouchBar delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTouchBar nsTouchBar => nsTouchBar -> RawId -> IO ()
setDelegate nsTouchBar value =
  sendMessage nsTouchBar setDelegateSelector value

-- | @- visible@
visible :: IsNSTouchBar nsTouchBar => nsTouchBar -> IO Bool
visible nsTouchBar =
  sendMessage nsTouchBar visibleSelector

-- | @+ automaticCustomizeTouchBarMenuItemEnabled@
automaticCustomizeTouchBarMenuItemEnabled :: IO Bool
automaticCustomizeTouchBarMenuItemEnabled  =
  do
    cls' <- getRequiredClass "NSTouchBar"
    sendClassMessage cls' automaticCustomizeTouchBarMenuItemEnabledSelector

-- | @+ setAutomaticCustomizeTouchBarMenuItemEnabled:@
setAutomaticCustomizeTouchBarMenuItemEnabled :: Bool -> IO ()
setAutomaticCustomizeTouchBarMenuItemEnabled value =
  do
    cls' <- getRequiredClass "NSTouchBar"
    sendClassMessage cls' setAutomaticCustomizeTouchBarMenuItemEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTouchBar)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTouchBar)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @itemForIdentifier:@
itemForIdentifierSelector :: Selector '[Id NSString] (Id NSTouchBarItem)
itemForIdentifierSelector = mkSelector "itemForIdentifier:"

-- | @Selector@ for @customizationIdentifier@
customizationIdentifierSelector :: Selector '[] (Id NSString)
customizationIdentifierSelector = mkSelector "customizationIdentifier"

-- | @Selector@ for @setCustomizationIdentifier:@
setCustomizationIdentifierSelector :: Selector '[Id NSString] ()
setCustomizationIdentifierSelector = mkSelector "setCustomizationIdentifier:"

-- | @Selector@ for @customizationAllowedItemIdentifiers@
customizationAllowedItemIdentifiersSelector :: Selector '[] (Id NSArray)
customizationAllowedItemIdentifiersSelector = mkSelector "customizationAllowedItemIdentifiers"

-- | @Selector@ for @setCustomizationAllowedItemIdentifiers:@
setCustomizationAllowedItemIdentifiersSelector :: Selector '[Id NSArray] ()
setCustomizationAllowedItemIdentifiersSelector = mkSelector "setCustomizationAllowedItemIdentifiers:"

-- | @Selector@ for @customizationRequiredItemIdentifiers@
customizationRequiredItemIdentifiersSelector :: Selector '[] (Id NSArray)
customizationRequiredItemIdentifiersSelector = mkSelector "customizationRequiredItemIdentifiers"

-- | @Selector@ for @setCustomizationRequiredItemIdentifiers:@
setCustomizationRequiredItemIdentifiersSelector :: Selector '[Id NSArray] ()
setCustomizationRequiredItemIdentifiersSelector = mkSelector "setCustomizationRequiredItemIdentifiers:"

-- | @Selector@ for @defaultItemIdentifiers@
defaultItemIdentifiersSelector :: Selector '[] (Id NSArray)
defaultItemIdentifiersSelector = mkSelector "defaultItemIdentifiers"

-- | @Selector@ for @setDefaultItemIdentifiers:@
setDefaultItemIdentifiersSelector :: Selector '[Id NSArray] ()
setDefaultItemIdentifiersSelector = mkSelector "setDefaultItemIdentifiers:"

-- | @Selector@ for @itemIdentifiers@
itemIdentifiersSelector :: Selector '[] (Id NSArray)
itemIdentifiersSelector = mkSelector "itemIdentifiers"

-- | @Selector@ for @principalItemIdentifier@
principalItemIdentifierSelector :: Selector '[] (Id NSString)
principalItemIdentifierSelector = mkSelector "principalItemIdentifier"

-- | @Selector@ for @setPrincipalItemIdentifier:@
setPrincipalItemIdentifierSelector :: Selector '[Id NSString] ()
setPrincipalItemIdentifierSelector = mkSelector "setPrincipalItemIdentifier:"

-- | @Selector@ for @escapeKeyReplacementItemIdentifier@
escapeKeyReplacementItemIdentifierSelector :: Selector '[] (Id NSString)
escapeKeyReplacementItemIdentifierSelector = mkSelector "escapeKeyReplacementItemIdentifier"

-- | @Selector@ for @setEscapeKeyReplacementItemIdentifier:@
setEscapeKeyReplacementItemIdentifierSelector :: Selector '[Id NSString] ()
setEscapeKeyReplacementItemIdentifierSelector = mkSelector "setEscapeKeyReplacementItemIdentifier:"

-- | @Selector@ for @templateItems@
templateItemsSelector :: Selector '[] (Id NSSet)
templateItemsSelector = mkSelector "templateItems"

-- | @Selector@ for @setTemplateItems:@
setTemplateItemsSelector :: Selector '[Id NSSet] ()
setTemplateItemsSelector = mkSelector "setTemplateItems:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @visible@
visibleSelector :: Selector '[] Bool
visibleSelector = mkSelector "visible"

-- | @Selector@ for @automaticCustomizeTouchBarMenuItemEnabled@
automaticCustomizeTouchBarMenuItemEnabledSelector :: Selector '[] Bool
automaticCustomizeTouchBarMenuItemEnabledSelector = mkSelector "automaticCustomizeTouchBarMenuItemEnabled"

-- | @Selector@ for @setAutomaticCustomizeTouchBarMenuItemEnabled:@
setAutomaticCustomizeTouchBarMenuItemEnabledSelector :: Selector '[Bool] ()
setAutomaticCustomizeTouchBarMenuItemEnabledSelector = mkSelector "setAutomaticCustomizeTouchBarMenuItemEnabled:"

