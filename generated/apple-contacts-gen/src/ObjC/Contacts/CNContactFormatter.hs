{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Formats a contact name.
--
-- This formatter handles international ordering and delimiting of the contact name components. This includes applying the user defaults when appropriate.
--
-- Generated bindings for @CNContactFormatter@.
module ObjC.Contacts.CNContactFormatter
  ( CNContactFormatter
  , IsCNContactFormatter(..)
  , descriptorForRequiredKeysForStyle
  , stringFromContact_style
  , attributedStringFromContact_style_defaultAttributes
  , nameOrderForContact
  , delimiterForContact
  , stringFromContact
  , attributedStringFromContact_defaultAttributes
  , descriptorForRequiredKeysForNameOrder
  , descriptorForRequiredKeysForDelimiter
  , style
  , setStyle
  , descriptorForRequiredKeysForStyleSelector
  , stringFromContact_styleSelector
  , attributedStringFromContact_style_defaultAttributesSelector
  , nameOrderForContactSelector
  , delimiterForContactSelector
  , stringFromContactSelector
  , attributedStringFromContact_defaultAttributesSelector
  , descriptorForRequiredKeysForNameOrderSelector
  , descriptorForRequiredKeysForDelimiterSelector
  , styleSelector
  , setStyleSelector

  -- * Enum types
  , CNContactDisplayNameOrder(CNContactDisplayNameOrder)
  , pattern CNContactDisplayNameOrderUserDefault
  , pattern CNContactDisplayNameOrderGivenNameFirst
  , pattern CNContactDisplayNameOrderFamilyNameFirst
  , CNContactFormatterStyle(CNContactFormatterStyle)
  , pattern CNContactFormatterStyleFullName
  , pattern CNContactFormatterStylePhoneticFullName

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

import ObjC.Contacts.Internal.Classes
import ObjC.Contacts.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The contact key descriptor required for the formatter.
--
-- Use to fetch all contact keys required for the formatter style. Can combine key descriptors for different formatter styles in the fetch.
--
-- @style@ — The formatting style to be used for the contact name.
--
-- Returns: The contact key descriptor for the formatting style.
--
-- ObjC selector: @+ descriptorForRequiredKeysForStyle:@
descriptorForRequiredKeysForStyle :: CNContactFormatterStyle -> IO RawId
descriptorForRequiredKeysForStyle style =
  do
    cls' <- getRequiredClass "CNContactFormatter"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "descriptorForRequiredKeysForStyle:") (retPtr retVoid) [argCLong (coerce style)]

-- | Formats the contact name.
--
-- @contact@ — The contact whose name is to be formatted.
--
-- @style@ — The formatting style to be used for the contact name.
--
-- Returns: The formatted contact name.
--
-- ObjC selector: @+ stringFromContact:style:@
stringFromContact_style :: IsCNContact contact => contact -> CNContactFormatterStyle -> IO (Id NSString)
stringFromContact_style contact style =
  do
    cls' <- getRequiredClass "CNContactFormatter"
    withObjCPtr contact $ \raw_contact ->
      sendClassMsg cls' (mkSelector "stringFromContact:style:") (retPtr retVoid) [argPtr (castPtr raw_contact :: Ptr ()), argCLong (coerce style)] >>= retainedObject . castPtr

-- | Formats the contact name returning an attributed string.
--
-- This behaves like +stringFromContact:style: except it returns an attributed string. Includes the attribute key CNContactPropertyAttribute.
--
-- @contact@ — The contact whose name is to be formatted.
--
-- @style@ — The formatting style to be used for the contact name.
--
-- @attributes@ — The default attributes to use. See NSFormatter for details.
--
-- Returns: The formatted contact name as an attributed string.
--
-- ObjC selector: @+ attributedStringFromContact:style:defaultAttributes:@
attributedStringFromContact_style_defaultAttributes :: (IsCNContact contact, IsNSDictionary attributes) => contact -> CNContactFormatterStyle -> attributes -> IO (Id NSAttributedString)
attributedStringFromContact_style_defaultAttributes contact style attributes =
  do
    cls' <- getRequiredClass "CNContactFormatter"
    withObjCPtr contact $ \raw_contact ->
      withObjCPtr attributes $ \raw_attributes ->
        sendClassMsg cls' (mkSelector "attributedStringFromContact:style:defaultAttributes:") (retPtr retVoid) [argPtr (castPtr raw_contact :: Ptr ()), argCLong (coerce style), argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | The recommended name order for a given contact.
--
-- ObjC selector: @+ nameOrderForContact:@
nameOrderForContact :: IsCNContact contact => contact -> IO CNContactDisplayNameOrder
nameOrderForContact contact =
  do
    cls' <- getRequiredClass "CNContactFormatter"
    withObjCPtr contact $ \raw_contact ->
      fmap (coerce :: CLong -> CNContactDisplayNameOrder) $ sendClassMsg cls' (mkSelector "nameOrderForContact:") retCLong [argPtr (castPtr raw_contact :: Ptr ())]

-- | The recommended delimiter to use between name components for a given contact.
--
-- ObjC selector: @+ delimiterForContact:@
delimiterForContact :: IsCNContact contact => contact -> IO (Id NSString)
delimiterForContact contact =
  do
    cls' <- getRequiredClass "CNContactFormatter"
    withObjCPtr contact $ \raw_contact ->
      sendClassMsg cls' (mkSelector "delimiterForContact:") (retPtr retVoid) [argPtr (castPtr raw_contact :: Ptr ())] >>= retainedObject . castPtr

-- | Formats the contact name.
--
-- @contact@ — The contact whose name is to be formatted.
--
-- Returns: The formatted contact name.
--
-- ObjC selector: @- stringFromContact:@
stringFromContact :: (IsCNContactFormatter cnContactFormatter, IsCNContact contact) => cnContactFormatter -> contact -> IO (Id NSString)
stringFromContact cnContactFormatter  contact =
  withObjCPtr contact $ \raw_contact ->
      sendMsg cnContactFormatter (mkSelector "stringFromContact:") (retPtr retVoid) [argPtr (castPtr raw_contact :: Ptr ())] >>= retainedObject . castPtr

-- | Formats the contact name returning an attributed string.
--
-- This behaves like -stringFromContact:style: except it returns an attributed string. CNContactPropertyAttribute key has the value of a CNContact name property key.
--
-- @contact@ — The contact whose name is to be formatted.
--
-- @attributes@ — The default attributes to use. See NSFormatter for details.
--
-- Returns: The formatted contact name as an attributed string.
--
-- ObjC selector: @- attributedStringFromContact:defaultAttributes:@
attributedStringFromContact_defaultAttributes :: (IsCNContactFormatter cnContactFormatter, IsCNContact contact, IsNSDictionary attributes) => cnContactFormatter -> contact -> attributes -> IO (Id NSAttributedString)
attributedStringFromContact_defaultAttributes cnContactFormatter  contact attributes =
  withObjCPtr contact $ \raw_contact ->
    withObjCPtr attributes $ \raw_attributes ->
        sendMsg cnContactFormatter (mkSelector "attributedStringFromContact:defaultAttributes:") (retPtr retVoid) [argPtr (castPtr raw_contact :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | The contact key descriptor required for the name order.
--
-- Use to fetch all contact keys required for +nameOrderForContact:. Can combine key descriptors for different formatter styles in the fetch.
--
-- Returns: The contact key descriptor for the name order.
--
-- ObjC selector: @+ descriptorForRequiredKeysForNameOrder@
descriptorForRequiredKeysForNameOrder :: IO RawId
descriptorForRequiredKeysForNameOrder  =
  do
    cls' <- getRequiredClass "CNContactFormatter"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "descriptorForRequiredKeysForNameOrder") (retPtr retVoid) []

-- | The contact key descriptor required for the name delimiter.
--
-- Use to fetch all contact keys required for +delimiterForContact:. Can combine key descriptors for different formatter styles in the fetch.
--
-- Returns: The contact key descriptor for the name delimiter.
--
-- ObjC selector: @+ descriptorForRequiredKeysForDelimiter@
descriptorForRequiredKeysForDelimiter :: IO RawId
descriptorForRequiredKeysForDelimiter  =
  do
    cls' <- getRequiredClass "CNContactFormatter"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "descriptorForRequiredKeysForDelimiter") (retPtr retVoid) []

-- | The style for a contact formatter instance.
--
-- The default value is CNContactFormatterStyleFullName.
--
-- ObjC selector: @- style@
style :: IsCNContactFormatter cnContactFormatter => cnContactFormatter -> IO CNContactFormatterStyle
style cnContactFormatter  =
    fmap (coerce :: CLong -> CNContactFormatterStyle) $ sendMsg cnContactFormatter (mkSelector "style") retCLong []

-- | The style for a contact formatter instance.
--
-- The default value is CNContactFormatterStyleFullName.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsCNContactFormatter cnContactFormatter => cnContactFormatter -> CNContactFormatterStyle -> IO ()
setStyle cnContactFormatter  value =
    sendMsg cnContactFormatter (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorForRequiredKeysForStyle:@
descriptorForRequiredKeysForStyleSelector :: Selector
descriptorForRequiredKeysForStyleSelector = mkSelector "descriptorForRequiredKeysForStyle:"

-- | @Selector@ for @stringFromContact:style:@
stringFromContact_styleSelector :: Selector
stringFromContact_styleSelector = mkSelector "stringFromContact:style:"

-- | @Selector@ for @attributedStringFromContact:style:defaultAttributes:@
attributedStringFromContact_style_defaultAttributesSelector :: Selector
attributedStringFromContact_style_defaultAttributesSelector = mkSelector "attributedStringFromContact:style:defaultAttributes:"

-- | @Selector@ for @nameOrderForContact:@
nameOrderForContactSelector :: Selector
nameOrderForContactSelector = mkSelector "nameOrderForContact:"

-- | @Selector@ for @delimiterForContact:@
delimiterForContactSelector :: Selector
delimiterForContactSelector = mkSelector "delimiterForContact:"

-- | @Selector@ for @stringFromContact:@
stringFromContactSelector :: Selector
stringFromContactSelector = mkSelector "stringFromContact:"

-- | @Selector@ for @attributedStringFromContact:defaultAttributes:@
attributedStringFromContact_defaultAttributesSelector :: Selector
attributedStringFromContact_defaultAttributesSelector = mkSelector "attributedStringFromContact:defaultAttributes:"

-- | @Selector@ for @descriptorForRequiredKeysForNameOrder@
descriptorForRequiredKeysForNameOrderSelector :: Selector
descriptorForRequiredKeysForNameOrderSelector = mkSelector "descriptorForRequiredKeysForNameOrder"

-- | @Selector@ for @descriptorForRequiredKeysForDelimiter@
descriptorForRequiredKeysForDelimiterSelector :: Selector
descriptorForRequiredKeysForDelimiterSelector = mkSelector "descriptorForRequiredKeysForDelimiter"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

