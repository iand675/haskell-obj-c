{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Formats a postal address.
--
-- This formatter handles international formatting of a postal address.
--
-- Generated bindings for @CNPostalAddressFormatter@.
module ObjC.Contacts.CNPostalAddressFormatter
  ( CNPostalAddressFormatter
  , IsCNPostalAddressFormatter(..)
  , stringFromPostalAddress_style
  , attributedStringFromPostalAddress_style_withDefaultAttributes
  , stringFromPostalAddress
  , attributedStringFromPostalAddress_withDefaultAttributes
  , style
  , setStyle
  , stringFromPostalAddress_styleSelector
  , attributedStringFromPostalAddress_style_withDefaultAttributesSelector
  , stringFromPostalAddressSelector
  , attributedStringFromPostalAddress_withDefaultAttributesSelector
  , styleSelector
  , setStyleSelector

  -- * Enum types
  , CNPostalAddressFormatterStyle(CNPostalAddressFormatterStyle)
  , pattern CNPostalAddressFormatterStyleMailingAddress

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

-- | Formats the postal address.
--
-- @postalAddress@ — The postal address to be formatted.
--
-- @style@ — The formatting style to be used for the postal address.
--
-- Returns: The formatted postal address.
--
-- ObjC selector: @+ stringFromPostalAddress:style:@
stringFromPostalAddress_style :: IsCNPostalAddress postalAddress => postalAddress -> CNPostalAddressFormatterStyle -> IO (Id NSString)
stringFromPostalAddress_style postalAddress style =
  do
    cls' <- getRequiredClass "CNPostalAddressFormatter"
    withObjCPtr postalAddress $ \raw_postalAddress ->
      sendClassMsg cls' (mkSelector "stringFromPostalAddress:style:") (retPtr retVoid) [argPtr (castPtr raw_postalAddress :: Ptr ()), argCLong (coerce style)] >>= retainedObject . castPtr

-- | Formats the postal address returning an attributed string.
--
-- This behaves like +stringFromPostalAddress: except it returns an attributed string. Includes attribute keys CNPostalAddressPropertyAttribute and CNPostalAddressLocalizedPropertyNameAttribute.
--
-- @postalAddress@ — The postal address to be formatted.
--
-- @style@ — The formatting style to be used for the postal address.
--
-- @attributes@ — The default attributes to use. See NSFormatter for details.
--
-- Returns: The formatted postal address as an attributed string.
--
-- ObjC selector: @+ attributedStringFromPostalAddress:style:withDefaultAttributes:@
attributedStringFromPostalAddress_style_withDefaultAttributes :: (IsCNPostalAddress postalAddress, IsNSDictionary attributes) => postalAddress -> CNPostalAddressFormatterStyle -> attributes -> IO (Id NSAttributedString)
attributedStringFromPostalAddress_style_withDefaultAttributes postalAddress style attributes =
  do
    cls' <- getRequiredClass "CNPostalAddressFormatter"
    withObjCPtr postalAddress $ \raw_postalAddress ->
      withObjCPtr attributes $ \raw_attributes ->
        sendClassMsg cls' (mkSelector "attributedStringFromPostalAddress:style:withDefaultAttributes:") (retPtr retVoid) [argPtr (castPtr raw_postalAddress :: Ptr ()), argCLong (coerce style), argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | Formats the postal address.
--
-- @postalAddress@ — The postal address to be formatted.
--
-- Returns: The formatted postal address.
--
-- ObjC selector: @- stringFromPostalAddress:@
stringFromPostalAddress :: (IsCNPostalAddressFormatter cnPostalAddressFormatter, IsCNPostalAddress postalAddress) => cnPostalAddressFormatter -> postalAddress -> IO (Id NSString)
stringFromPostalAddress cnPostalAddressFormatter  postalAddress =
withObjCPtr postalAddress $ \raw_postalAddress ->
    sendMsg cnPostalAddressFormatter (mkSelector "stringFromPostalAddress:") (retPtr retVoid) [argPtr (castPtr raw_postalAddress :: Ptr ())] >>= retainedObject . castPtr

-- | Formats the postal address returning an attributed string.
--
-- This behaves like +stringFromPostalAddress: except it returns an attributed string. Includes attribute keys CNPostalAddressPropertyAttribute and CNPostalAddressLocalizedPropertyNameAttribute.
--
-- @postalAddress@ — The postal address to be formatted.
--
-- @attributes@ — The default attributes to use. See NSFormatter for details.
--
-- Returns: The formatted postal address as an attributed string.
--
-- ObjC selector: @- attributedStringFromPostalAddress:withDefaultAttributes:@
attributedStringFromPostalAddress_withDefaultAttributes :: (IsCNPostalAddressFormatter cnPostalAddressFormatter, IsCNPostalAddress postalAddress, IsNSDictionary attributes) => cnPostalAddressFormatter -> postalAddress -> attributes -> IO (Id NSAttributedString)
attributedStringFromPostalAddress_withDefaultAttributes cnPostalAddressFormatter  postalAddress attributes =
withObjCPtr postalAddress $ \raw_postalAddress ->
  withObjCPtr attributes $ \raw_attributes ->
      sendMsg cnPostalAddressFormatter (mkSelector "attributedStringFromPostalAddress:withDefaultAttributes:") (retPtr retVoid) [argPtr (castPtr raw_postalAddress :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | The style for a postal address formatter instance.
--
-- The default value is CNPostalAddressFormatterStyleMailingAddress.
--
-- ObjC selector: @- style@
style :: IsCNPostalAddressFormatter cnPostalAddressFormatter => cnPostalAddressFormatter -> IO CNPostalAddressFormatterStyle
style cnPostalAddressFormatter  =
  fmap (coerce :: CLong -> CNPostalAddressFormatterStyle) $ sendMsg cnPostalAddressFormatter (mkSelector "style") retCLong []

-- | The style for a postal address formatter instance.
--
-- The default value is CNPostalAddressFormatterStyleMailingAddress.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsCNPostalAddressFormatter cnPostalAddressFormatter => cnPostalAddressFormatter -> CNPostalAddressFormatterStyle -> IO ()
setStyle cnPostalAddressFormatter  value =
  sendMsg cnPostalAddressFormatter (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromPostalAddress:style:@
stringFromPostalAddress_styleSelector :: Selector
stringFromPostalAddress_styleSelector = mkSelector "stringFromPostalAddress:style:"

-- | @Selector@ for @attributedStringFromPostalAddress:style:withDefaultAttributes:@
attributedStringFromPostalAddress_style_withDefaultAttributesSelector :: Selector
attributedStringFromPostalAddress_style_withDefaultAttributesSelector = mkSelector "attributedStringFromPostalAddress:style:withDefaultAttributes:"

-- | @Selector@ for @stringFromPostalAddress:@
stringFromPostalAddressSelector :: Selector
stringFromPostalAddressSelector = mkSelector "stringFromPostalAddress:"

-- | @Selector@ for @attributedStringFromPostalAddress:withDefaultAttributes:@
attributedStringFromPostalAddress_withDefaultAttributesSelector :: Selector
attributedStringFromPostalAddress_withDefaultAttributesSelector = mkSelector "attributedStringFromPostalAddress:withDefaultAttributes:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

