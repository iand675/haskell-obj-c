{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSAccessibilityCustomRotors allow assistive technologies, like VoiceOver, to search applications for content related to the given label.
--
-- Generated bindings for @NSAccessibilityCustomRotor@.
module ObjC.AppKit.NSAccessibilityCustomRotor
  ( NSAccessibilityCustomRotor
  , IsNSAccessibilityCustomRotor(..)
  , initWithLabel_itemSearchDelegate
  , initWithRotorType_itemSearchDelegate
  , type_
  , setType
  , label
  , setLabel
  , itemSearchDelegate
  , setItemSearchDelegate
  , itemLoadingDelegate
  , setItemLoadingDelegate
  , initWithLabel_itemSearchDelegateSelector
  , initWithRotorType_itemSearchDelegateSelector
  , itemLoadingDelegateSelector
  , itemSearchDelegateSelector
  , labelSelector
  , setItemLoadingDelegateSelector
  , setItemSearchDelegateSelector
  , setLabelSelector
  , setTypeSelector
  , typeSelector

  -- * Enum types
  , NSAccessibilityCustomRotorType(NSAccessibilityCustomRotorType)
  , pattern NSAccessibilityCustomRotorTypeCustom
  , pattern NSAccessibilityCustomRotorTypeAny
  , pattern NSAccessibilityCustomRotorTypeAnnotation
  , pattern NSAccessibilityCustomRotorTypeBoldText
  , pattern NSAccessibilityCustomRotorTypeHeading
  , pattern NSAccessibilityCustomRotorTypeHeadingLevel1
  , pattern NSAccessibilityCustomRotorTypeHeadingLevel2
  , pattern NSAccessibilityCustomRotorTypeHeadingLevel3
  , pattern NSAccessibilityCustomRotorTypeHeadingLevel4
  , pattern NSAccessibilityCustomRotorTypeHeadingLevel5
  , pattern NSAccessibilityCustomRotorTypeHeadingLevel6
  , pattern NSAccessibilityCustomRotorTypeImage
  , pattern NSAccessibilityCustomRotorTypeItalicText
  , pattern NSAccessibilityCustomRotorTypeLandmark
  , pattern NSAccessibilityCustomRotorTypeLink
  , pattern NSAccessibilityCustomRotorTypeList
  , pattern NSAccessibilityCustomRotorTypeMisspelledWord
  , pattern NSAccessibilityCustomRotorTypeTable
  , pattern NSAccessibilityCustomRotorTypeTextField
  , pattern NSAccessibilityCustomRotorTypeUnderlinedText
  , pattern NSAccessibilityCustomRotorTypeVisitedLink
  , pattern NSAccessibilityCustomRotorTypeAudiograph

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

-- | Convenience initializer that uses NSAccessibilityCustomRotorTypeCustom as the default rotor type. Use this initializer for custom rotors that are not one of the common types.
--
-- ObjC selector: @- initWithLabel:itemSearchDelegate:@
initWithLabel_itemSearchDelegate :: (IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor, IsNSString label) => nsAccessibilityCustomRotor -> label -> RawId -> IO (Id NSAccessibilityCustomRotor)
initWithLabel_itemSearchDelegate nsAccessibilityCustomRotor label itemSearchDelegate =
  sendOwnedMessage nsAccessibilityCustomRotor initWithLabel_itemSearchDelegateSelector (toNSString label) itemSearchDelegate

-- | Convenience initializer for custom rotors that use a common type such as links, headings, etc. A default label will be provided.
--
-- ObjC selector: @- initWithRotorType:itemSearchDelegate:@
initWithRotorType_itemSearchDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> NSAccessibilityCustomRotorType -> RawId -> IO (Id NSAccessibilityCustomRotor)
initWithRotorType_itemSearchDelegate nsAccessibilityCustomRotor rotorType itemSearchDelegate =
  sendOwnedMessage nsAccessibilityCustomRotor initWithRotorType_itemSearchDelegateSelector rotorType itemSearchDelegate

-- | The rotor type to provide results for.
--
-- The default type is NSAccessibilityCustomRotorTypeCustom, unless the rotor type was specified in the initializer.
--
-- ObjC selector: @- type@
type_ :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> IO NSAccessibilityCustomRotorType
type_ nsAccessibilityCustomRotor =
  sendMessage nsAccessibilityCustomRotor typeSelector

-- | The rotor type to provide results for.
--
-- The default type is NSAccessibilityCustomRotorTypeCustom, unless the rotor type was specified in the initializer.
--
-- ObjC selector: @- setType:@
setType :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> NSAccessibilityCustomRotorType -> IO ()
setType nsAccessibilityCustomRotor value =
  sendMessage nsAccessibilityCustomRotor setTypeSelector value

-- | The localized label assistive technologies will use to describe the custom rotor.
--
-- The label is only used when the rotor type is NSAccessibilityCustomRotorTypeCustom since a default is provided for all other types.
--
-- ObjC selector: @- label@
label :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> IO (Id NSString)
label nsAccessibilityCustomRotor =
  sendMessage nsAccessibilityCustomRotor labelSelector

-- | The localized label assistive technologies will use to describe the custom rotor.
--
-- The label is only used when the rotor type is NSAccessibilityCustomRotorTypeCustom since a default is provided for all other types.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor, IsNSString value) => nsAccessibilityCustomRotor -> value -> IO ()
setLabel nsAccessibilityCustomRotor value =
  sendMessage nsAccessibilityCustomRotor setLabelSelector (toNSString value)

-- | The itemSearchDelegate will be asked to find the next item result after performing a search with the given search parameters.
--
-- ObjC selector: @- itemSearchDelegate@
itemSearchDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> IO RawId
itemSearchDelegate nsAccessibilityCustomRotor =
  sendMessage nsAccessibilityCustomRotor itemSearchDelegateSelector

-- | The itemSearchDelegate will be asked to find the next item result after performing a search with the given search parameters.
--
-- ObjC selector: @- setItemSearchDelegate:@
setItemSearchDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> RawId -> IO ()
setItemSearchDelegate nsAccessibilityCustomRotor value =
  sendMessage nsAccessibilityCustomRotor setItemSearchDelegateSelector value

-- | Provide an item load delegate if the rotor vends item results that do not have a backing UI element yet. The loader will be asked to load an element via the accessibilityElementWithToken protocol method when the item result is selected by an assistive client. Applications can use the item result's token to determine which item to return.
--
-- ObjC selector: @- itemLoadingDelegate@
itemLoadingDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> IO RawId
itemLoadingDelegate nsAccessibilityCustomRotor =
  sendMessage nsAccessibilityCustomRotor itemLoadingDelegateSelector

-- | Provide an item load delegate if the rotor vends item results that do not have a backing UI element yet. The loader will be asked to load an element via the accessibilityElementWithToken protocol method when the item result is selected by an assistive client. Applications can use the item result's token to determine which item to return.
--
-- ObjC selector: @- setItemLoadingDelegate:@
setItemLoadingDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> RawId -> IO ()
setItemLoadingDelegate nsAccessibilityCustomRotor value =
  sendMessage nsAccessibilityCustomRotor setItemLoadingDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLabel:itemSearchDelegate:@
initWithLabel_itemSearchDelegateSelector :: Selector '[Id NSString, RawId] (Id NSAccessibilityCustomRotor)
initWithLabel_itemSearchDelegateSelector = mkSelector "initWithLabel:itemSearchDelegate:"

-- | @Selector@ for @initWithRotorType:itemSearchDelegate:@
initWithRotorType_itemSearchDelegateSelector :: Selector '[NSAccessibilityCustomRotorType, RawId] (Id NSAccessibilityCustomRotor)
initWithRotorType_itemSearchDelegateSelector = mkSelector "initWithRotorType:itemSearchDelegate:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NSAccessibilityCustomRotorType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[NSAccessibilityCustomRotorType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @itemSearchDelegate@
itemSearchDelegateSelector :: Selector '[] RawId
itemSearchDelegateSelector = mkSelector "itemSearchDelegate"

-- | @Selector@ for @setItemSearchDelegate:@
setItemSearchDelegateSelector :: Selector '[RawId] ()
setItemSearchDelegateSelector = mkSelector "setItemSearchDelegate:"

-- | @Selector@ for @itemLoadingDelegate@
itemLoadingDelegateSelector :: Selector '[] RawId
itemLoadingDelegateSelector = mkSelector "itemLoadingDelegate"

-- | @Selector@ for @setItemLoadingDelegate:@
setItemLoadingDelegateSelector :: Selector '[RawId] ()
setItemLoadingDelegateSelector = mkSelector "setItemLoadingDelegate:"

