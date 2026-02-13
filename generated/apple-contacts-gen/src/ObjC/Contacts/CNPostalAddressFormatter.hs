{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , attributedStringFromPostalAddress_style_withDefaultAttributesSelector
  , attributedStringFromPostalAddress_withDefaultAttributesSelector
  , setStyleSelector
  , stringFromPostalAddressSelector
  , stringFromPostalAddress_styleSelector
  , styleSelector

  -- * Enum types
  , CNPostalAddressFormatterStyle(CNPostalAddressFormatterStyle)
  , pattern CNPostalAddressFormatterStyleMailingAddress

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' stringFromPostalAddress_styleSelector (toCNPostalAddress postalAddress) style

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
    sendClassMessage cls' attributedStringFromPostalAddress_style_withDefaultAttributesSelector (toCNPostalAddress postalAddress) style (toNSDictionary attributes)

-- | Formats the postal address.
--
-- @postalAddress@ — The postal address to be formatted.
--
-- Returns: The formatted postal address.
--
-- ObjC selector: @- stringFromPostalAddress:@
stringFromPostalAddress :: (IsCNPostalAddressFormatter cnPostalAddressFormatter, IsCNPostalAddress postalAddress) => cnPostalAddressFormatter -> postalAddress -> IO (Id NSString)
stringFromPostalAddress cnPostalAddressFormatter postalAddress =
  sendMessage cnPostalAddressFormatter stringFromPostalAddressSelector (toCNPostalAddress postalAddress)

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
attributedStringFromPostalAddress_withDefaultAttributes cnPostalAddressFormatter postalAddress attributes =
  sendMessage cnPostalAddressFormatter attributedStringFromPostalAddress_withDefaultAttributesSelector (toCNPostalAddress postalAddress) (toNSDictionary attributes)

-- | The style for a postal address formatter instance.
--
-- The default value is CNPostalAddressFormatterStyleMailingAddress.
--
-- ObjC selector: @- style@
style :: IsCNPostalAddressFormatter cnPostalAddressFormatter => cnPostalAddressFormatter -> IO CNPostalAddressFormatterStyle
style cnPostalAddressFormatter =
  sendMessage cnPostalAddressFormatter styleSelector

-- | The style for a postal address formatter instance.
--
-- The default value is CNPostalAddressFormatterStyleMailingAddress.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsCNPostalAddressFormatter cnPostalAddressFormatter => cnPostalAddressFormatter -> CNPostalAddressFormatterStyle -> IO ()
setStyle cnPostalAddressFormatter value =
  sendMessage cnPostalAddressFormatter setStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringFromPostalAddress:style:@
stringFromPostalAddress_styleSelector :: Selector '[Id CNPostalAddress, CNPostalAddressFormatterStyle] (Id NSString)
stringFromPostalAddress_styleSelector = mkSelector "stringFromPostalAddress:style:"

-- | @Selector@ for @attributedStringFromPostalAddress:style:withDefaultAttributes:@
attributedStringFromPostalAddress_style_withDefaultAttributesSelector :: Selector '[Id CNPostalAddress, CNPostalAddressFormatterStyle, Id NSDictionary] (Id NSAttributedString)
attributedStringFromPostalAddress_style_withDefaultAttributesSelector = mkSelector "attributedStringFromPostalAddress:style:withDefaultAttributes:"

-- | @Selector@ for @stringFromPostalAddress:@
stringFromPostalAddressSelector :: Selector '[Id CNPostalAddress] (Id NSString)
stringFromPostalAddressSelector = mkSelector "stringFromPostalAddress:"

-- | @Selector@ for @attributedStringFromPostalAddress:withDefaultAttributes:@
attributedStringFromPostalAddress_withDefaultAttributesSelector :: Selector '[Id CNPostalAddress, Id NSDictionary] (Id NSAttributedString)
attributedStringFromPostalAddress_withDefaultAttributesSelector = mkSelector "attributedStringFromPostalAddress:withDefaultAttributes:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] CNPostalAddressFormatterStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[CNPostalAddressFormatterStyle] ()
setStyleSelector = mkSelector "setStyle:"

