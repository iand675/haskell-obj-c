{-# LANGUAGE PatternSynonyms #-}
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
  , typeSelector
  , setTypeSelector
  , labelSelector
  , setLabelSelector
  , itemSearchDelegateSelector
  , setItemSearchDelegateSelector
  , itemLoadingDelegateSelector
  , setItemLoadingDelegateSelector

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

-- | Convenience initializer that uses NSAccessibilityCustomRotorTypeCustom as the default rotor type. Use this initializer for custom rotors that are not one of the common types.
--
-- ObjC selector: @- initWithLabel:itemSearchDelegate:@
initWithLabel_itemSearchDelegate :: (IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor, IsNSString label) => nsAccessibilityCustomRotor -> label -> RawId -> IO (Id NSAccessibilityCustomRotor)
initWithLabel_itemSearchDelegate nsAccessibilityCustomRotor  label itemSearchDelegate =
  withObjCPtr label $ \raw_label ->
      sendMsg nsAccessibilityCustomRotor (mkSelector "initWithLabel:itemSearchDelegate:") (retPtr retVoid) [argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr (unRawId itemSearchDelegate) :: Ptr ())] >>= ownedObject . castPtr

-- | Convenience initializer for custom rotors that use a common type such as links, headings, etc. A default label will be provided.
--
-- ObjC selector: @- initWithRotorType:itemSearchDelegate:@
initWithRotorType_itemSearchDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> NSAccessibilityCustomRotorType -> RawId -> IO (Id NSAccessibilityCustomRotor)
initWithRotorType_itemSearchDelegate nsAccessibilityCustomRotor  rotorType itemSearchDelegate =
    sendMsg nsAccessibilityCustomRotor (mkSelector "initWithRotorType:itemSearchDelegate:") (retPtr retVoid) [argCLong (coerce rotorType), argPtr (castPtr (unRawId itemSearchDelegate) :: Ptr ())] >>= ownedObject . castPtr

-- | The rotor type to provide results for.
--
-- The default type is NSAccessibilityCustomRotorTypeCustom, unless the rotor type was specified in the initializer.
--
-- ObjC selector: @- type@
type_ :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> IO NSAccessibilityCustomRotorType
type_ nsAccessibilityCustomRotor  =
    fmap (coerce :: CLong -> NSAccessibilityCustomRotorType) $ sendMsg nsAccessibilityCustomRotor (mkSelector "type") retCLong []

-- | The rotor type to provide results for.
--
-- The default type is NSAccessibilityCustomRotorTypeCustom, unless the rotor type was specified in the initializer.
--
-- ObjC selector: @- setType:@
setType :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> NSAccessibilityCustomRotorType -> IO ()
setType nsAccessibilityCustomRotor  value =
    sendMsg nsAccessibilityCustomRotor (mkSelector "setType:") retVoid [argCLong (coerce value)]

-- | The localized label assistive technologies will use to describe the custom rotor.
--
-- The label is only used when the rotor type is NSAccessibilityCustomRotorTypeCustom since a default is provided for all other types.
--
-- ObjC selector: @- label@
label :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> IO (Id NSString)
label nsAccessibilityCustomRotor  =
    sendMsg nsAccessibilityCustomRotor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized label assistive technologies will use to describe the custom rotor.
--
-- The label is only used when the rotor type is NSAccessibilityCustomRotorTypeCustom since a default is provided for all other types.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor, IsNSString value) => nsAccessibilityCustomRotor -> value -> IO ()
setLabel nsAccessibilityCustomRotor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsAccessibilityCustomRotor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The itemSearchDelegate will be asked to find the next item result after performing a search with the given search parameters.
--
-- ObjC selector: @- itemSearchDelegate@
itemSearchDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> IO RawId
itemSearchDelegate nsAccessibilityCustomRotor  =
    fmap (RawId . castPtr) $ sendMsg nsAccessibilityCustomRotor (mkSelector "itemSearchDelegate") (retPtr retVoid) []

-- | The itemSearchDelegate will be asked to find the next item result after performing a search with the given search parameters.
--
-- ObjC selector: @- setItemSearchDelegate:@
setItemSearchDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> RawId -> IO ()
setItemSearchDelegate nsAccessibilityCustomRotor  value =
    sendMsg nsAccessibilityCustomRotor (mkSelector "setItemSearchDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Provide an item load delegate if the rotor vends item results that do not have a backing UI element yet. The loader will be asked to load an element via the accessibilityElementWithToken protocol method when the item result is selected by an assistive client. Applications can use the item result's token to determine which item to return.
--
-- ObjC selector: @- itemLoadingDelegate@
itemLoadingDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> IO RawId
itemLoadingDelegate nsAccessibilityCustomRotor  =
    fmap (RawId . castPtr) $ sendMsg nsAccessibilityCustomRotor (mkSelector "itemLoadingDelegate") (retPtr retVoid) []

-- | Provide an item load delegate if the rotor vends item results that do not have a backing UI element yet. The loader will be asked to load an element via the accessibilityElementWithToken protocol method when the item result is selected by an assistive client. Applications can use the item result's token to determine which item to return.
--
-- ObjC selector: @- setItemLoadingDelegate:@
setItemLoadingDelegate :: IsNSAccessibilityCustomRotor nsAccessibilityCustomRotor => nsAccessibilityCustomRotor -> RawId -> IO ()
setItemLoadingDelegate nsAccessibilityCustomRotor  value =
    sendMsg nsAccessibilityCustomRotor (mkSelector "setItemLoadingDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLabel:itemSearchDelegate:@
initWithLabel_itemSearchDelegateSelector :: Selector
initWithLabel_itemSearchDelegateSelector = mkSelector "initWithLabel:itemSearchDelegate:"

-- | @Selector@ for @initWithRotorType:itemSearchDelegate:@
initWithRotorType_itemSearchDelegateSelector :: Selector
initWithRotorType_itemSearchDelegateSelector = mkSelector "initWithRotorType:itemSearchDelegate:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @itemSearchDelegate@
itemSearchDelegateSelector :: Selector
itemSearchDelegateSelector = mkSelector "itemSearchDelegate"

-- | @Selector@ for @setItemSearchDelegate:@
setItemSearchDelegateSelector :: Selector
setItemSearchDelegateSelector = mkSelector "setItemSearchDelegate:"

-- | @Selector@ for @itemLoadingDelegate@
itemLoadingDelegateSelector :: Selector
itemLoadingDelegateSelector = mkSelector "itemLoadingDelegate"

-- | @Selector@ for @setItemLoadingDelegate:@
setItemLoadingDelegateSelector :: Selector
setItemLoadingDelegateSelector = mkSelector "setItemLoadingDelegate:"

