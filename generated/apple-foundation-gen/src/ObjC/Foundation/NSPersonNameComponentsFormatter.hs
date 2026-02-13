{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersonNameComponentsFormatter@.
module ObjC.Foundation.NSPersonNameComponentsFormatter
  ( NSPersonNameComponentsFormatter
  , IsNSPersonNameComponentsFormatter(..)
  , localizedStringFromPersonNameComponents_style_options
  , stringFromPersonNameComponents
  , annotatedStringFromPersonNameComponents
  , personNameComponentsFromString
  , getObjectValue_forString_errorDescription
  , style
  , setStyle
  , phonetic
  , setPhonetic
  , locale
  , setLocale
  , annotatedStringFromPersonNameComponentsSelector
  , getObjectValue_forString_errorDescriptionSelector
  , localeSelector
  , localizedStringFromPersonNameComponents_style_optionsSelector
  , personNameComponentsFromStringSelector
  , phoneticSelector
  , setLocaleSelector
  , setPhoneticSelector
  , setStyleSelector
  , stringFromPersonNameComponentsSelector
  , styleSelector

  -- * Enum types
  , NSPersonNameComponentsFormatterOptions(NSPersonNameComponentsFormatterOptions)
  , pattern NSPersonNameComponentsFormatterPhonetic
  , NSPersonNameComponentsFormatterStyle(NSPersonNameComponentsFormatterStyle)
  , pattern NSPersonNameComponentsFormatterStyleDefault
  , pattern NSPersonNameComponentsFormatterStyleShort
  , pattern NSPersonNameComponentsFormatterStyleMedium
  , pattern NSPersonNameComponentsFormatterStyleLong
  , pattern NSPersonNameComponentsFormatterStyleAbbreviated

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ localizedStringFromPersonNameComponents:style:options:@
localizedStringFromPersonNameComponents_style_options :: IsNSPersonNameComponents components => components -> NSPersonNameComponentsFormatterStyle -> NSPersonNameComponentsFormatterOptions -> IO (Id NSString)
localizedStringFromPersonNameComponents_style_options components nameFormatStyle nameOptions =
  do
    cls' <- getRequiredClass "NSPersonNameComponentsFormatter"
    sendClassMessage cls' localizedStringFromPersonNameComponents_style_optionsSelector (toNSPersonNameComponents components) nameFormatStyle nameOptions

-- | @- stringFromPersonNameComponents:@
stringFromPersonNameComponents :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSPersonNameComponents components) => nsPersonNameComponentsFormatter -> components -> IO (Id NSString)
stringFromPersonNameComponents nsPersonNameComponentsFormatter components =
  sendMessage nsPersonNameComponentsFormatter stringFromPersonNameComponentsSelector (toNSPersonNameComponents components)

-- | @- annotatedStringFromPersonNameComponents:@
annotatedStringFromPersonNameComponents :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSPersonNameComponents components) => nsPersonNameComponentsFormatter -> components -> IO (Id NSAttributedString)
annotatedStringFromPersonNameComponents nsPersonNameComponentsFormatter components =
  sendMessage nsPersonNameComponentsFormatter annotatedStringFromPersonNameComponentsSelector (toNSPersonNameComponents components)

-- | @- personNameComponentsFromString:@
personNameComponentsFromString :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSString string) => nsPersonNameComponentsFormatter -> string -> IO (Id NSPersonNameComponents)
personNameComponentsFromString nsPersonNameComponentsFormatter string =
  sendMessage nsPersonNameComponentsFormatter personNameComponentsFromStringSelector (toNSString string)

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSString string, IsNSString error_) => nsPersonNameComponentsFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsPersonNameComponentsFormatter obj_ string error_ =
  sendMessage nsPersonNameComponentsFormatter getObjectValue_forString_errorDescriptionSelector obj_ (toNSString string) (toNSString error_)

-- | @- style@
style :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> IO NSPersonNameComponentsFormatterStyle
style nsPersonNameComponentsFormatter =
  sendMessage nsPersonNameComponentsFormatter styleSelector

-- | @- setStyle:@
setStyle :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> NSPersonNameComponentsFormatterStyle -> IO ()
setStyle nsPersonNameComponentsFormatter value =
  sendMessage nsPersonNameComponentsFormatter setStyleSelector value

-- | @- phonetic@
phonetic :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> IO Bool
phonetic nsPersonNameComponentsFormatter =
  sendMessage nsPersonNameComponentsFormatter phoneticSelector

-- | @- setPhonetic:@
setPhonetic :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> Bool -> IO ()
setPhonetic nsPersonNameComponentsFormatter value =
  sendMessage nsPersonNameComponentsFormatter setPhoneticSelector value

-- | @- locale@
locale :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> IO (Id NSLocale)
locale nsPersonNameComponentsFormatter =
  sendMessage nsPersonNameComponentsFormatter localeSelector

-- | @- setLocale:@
setLocale :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSLocale value) => nsPersonNameComponentsFormatter -> value -> IO ()
setLocale nsPersonNameComponentsFormatter value =
  sendMessage nsPersonNameComponentsFormatter setLocaleSelector (toNSLocale value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedStringFromPersonNameComponents:style:options:@
localizedStringFromPersonNameComponents_style_optionsSelector :: Selector '[Id NSPersonNameComponents, NSPersonNameComponentsFormatterStyle, NSPersonNameComponentsFormatterOptions] (Id NSString)
localizedStringFromPersonNameComponents_style_optionsSelector = mkSelector "localizedStringFromPersonNameComponents:style:options:"

-- | @Selector@ for @stringFromPersonNameComponents:@
stringFromPersonNameComponentsSelector :: Selector '[Id NSPersonNameComponents] (Id NSString)
stringFromPersonNameComponentsSelector = mkSelector "stringFromPersonNameComponents:"

-- | @Selector@ for @annotatedStringFromPersonNameComponents:@
annotatedStringFromPersonNameComponentsSelector :: Selector '[Id NSPersonNameComponents] (Id NSAttributedString)
annotatedStringFromPersonNameComponentsSelector = mkSelector "annotatedStringFromPersonNameComponents:"

-- | @Selector@ for @personNameComponentsFromString:@
personNameComponentsFromStringSelector :: Selector '[Id NSString] (Id NSPersonNameComponents)
personNameComponentsFromStringSelector = mkSelector "personNameComponentsFromString:"

-- | @Selector@ for @getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescriptionSelector :: Selector '[Ptr RawId, Id NSString, Id NSString] Bool
getObjectValue_forString_errorDescriptionSelector = mkSelector "getObjectValue:forString:errorDescription:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] NSPersonNameComponentsFormatterStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[NSPersonNameComponentsFormatterStyle] ()
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @phonetic@
phoneticSelector :: Selector '[] Bool
phoneticSelector = mkSelector "phonetic"

-- | @Selector@ for @setPhonetic:@
setPhoneticSelector :: Selector '[Bool] ()
setPhoneticSelector = mkSelector "setPhonetic:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

