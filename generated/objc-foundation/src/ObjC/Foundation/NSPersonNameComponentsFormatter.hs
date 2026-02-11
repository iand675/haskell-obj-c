{-# LANGUAGE PatternSynonyms #-}
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
  , localizedStringFromPersonNameComponents_style_optionsSelector
  , stringFromPersonNameComponentsSelector
  , annotatedStringFromPersonNameComponentsSelector
  , personNameComponentsFromStringSelector
  , getObjectValue_forString_errorDescriptionSelector
  , styleSelector
  , setStyleSelector
  , phoneticSelector
  , setPhoneticSelector
  , localeSelector
  , setLocaleSelector

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ localizedStringFromPersonNameComponents:style:options:@
localizedStringFromPersonNameComponents_style_options :: IsNSPersonNameComponents components => components -> NSPersonNameComponentsFormatterStyle -> NSPersonNameComponentsFormatterOptions -> IO (Id NSString)
localizedStringFromPersonNameComponents_style_options components nameFormatStyle nameOptions =
  do
    cls' <- getRequiredClass "NSPersonNameComponentsFormatter"
    withObjCPtr components $ \raw_components ->
      sendClassMsg cls' (mkSelector "localizedStringFromPersonNameComponents:style:options:") (retPtr retVoid) [argPtr (castPtr raw_components :: Ptr ()), argCLong (coerce nameFormatStyle), argCULong (coerce nameOptions)] >>= retainedObject . castPtr

-- | @- stringFromPersonNameComponents:@
stringFromPersonNameComponents :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSPersonNameComponents components) => nsPersonNameComponentsFormatter -> components -> IO (Id NSString)
stringFromPersonNameComponents nsPersonNameComponentsFormatter  components =
withObjCPtr components $ \raw_components ->
    sendMsg nsPersonNameComponentsFormatter (mkSelector "stringFromPersonNameComponents:") (retPtr retVoid) [argPtr (castPtr raw_components :: Ptr ())] >>= retainedObject . castPtr

-- | @- annotatedStringFromPersonNameComponents:@
annotatedStringFromPersonNameComponents :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSPersonNameComponents components) => nsPersonNameComponentsFormatter -> components -> IO (Id NSAttributedString)
annotatedStringFromPersonNameComponents nsPersonNameComponentsFormatter  components =
withObjCPtr components $ \raw_components ->
    sendMsg nsPersonNameComponentsFormatter (mkSelector "annotatedStringFromPersonNameComponents:") (retPtr retVoid) [argPtr (castPtr raw_components :: Ptr ())] >>= retainedObject . castPtr

-- | @- personNameComponentsFromString:@
personNameComponentsFromString :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSString string) => nsPersonNameComponentsFormatter -> string -> IO (Id NSPersonNameComponents)
personNameComponentsFromString nsPersonNameComponentsFormatter  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsPersonNameComponentsFormatter (mkSelector "personNameComponentsFromString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescription :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSString string, IsNSString error_) => nsPersonNameComponentsFormatter -> Ptr RawId -> string -> error_ -> IO Bool
getObjectValue_forString_errorDescription nsPersonNameComponentsFormatter  obj_ string error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersonNameComponentsFormatter (mkSelector "getObjectValue:forString:errorDescription:") retCULong [argPtr obj_, argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- style@
style :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> IO NSPersonNameComponentsFormatterStyle
style nsPersonNameComponentsFormatter  =
  fmap (coerce :: CLong -> NSPersonNameComponentsFormatterStyle) $ sendMsg nsPersonNameComponentsFormatter (mkSelector "style") retCLong []

-- | @- setStyle:@
setStyle :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> NSPersonNameComponentsFormatterStyle -> IO ()
setStyle nsPersonNameComponentsFormatter  value =
  sendMsg nsPersonNameComponentsFormatter (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- | @- phonetic@
phonetic :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> IO Bool
phonetic nsPersonNameComponentsFormatter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersonNameComponentsFormatter (mkSelector "phonetic") retCULong []

-- | @- setPhonetic:@
setPhonetic :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> Bool -> IO ()
setPhonetic nsPersonNameComponentsFormatter  value =
  sendMsg nsPersonNameComponentsFormatter (mkSelector "setPhonetic:") retVoid [argCULong (if value then 1 else 0)]

-- | @- locale@
locale :: IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter => nsPersonNameComponentsFormatter -> IO (Id NSLocale)
locale nsPersonNameComponentsFormatter  =
  sendMsg nsPersonNameComponentsFormatter (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsNSPersonNameComponentsFormatter nsPersonNameComponentsFormatter, IsNSLocale value) => nsPersonNameComponentsFormatter -> value -> IO ()
setLocale nsPersonNameComponentsFormatter  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersonNameComponentsFormatter (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedStringFromPersonNameComponents:style:options:@
localizedStringFromPersonNameComponents_style_optionsSelector :: Selector
localizedStringFromPersonNameComponents_style_optionsSelector = mkSelector "localizedStringFromPersonNameComponents:style:options:"

-- | @Selector@ for @stringFromPersonNameComponents:@
stringFromPersonNameComponentsSelector :: Selector
stringFromPersonNameComponentsSelector = mkSelector "stringFromPersonNameComponents:"

-- | @Selector@ for @annotatedStringFromPersonNameComponents:@
annotatedStringFromPersonNameComponentsSelector :: Selector
annotatedStringFromPersonNameComponentsSelector = mkSelector "annotatedStringFromPersonNameComponents:"

-- | @Selector@ for @personNameComponentsFromString:@
personNameComponentsFromStringSelector :: Selector
personNameComponentsFromStringSelector = mkSelector "personNameComponentsFromString:"

-- | @Selector@ for @getObjectValue:forString:errorDescription:@
getObjectValue_forString_errorDescriptionSelector :: Selector
getObjectValue_forString_errorDescriptionSelector = mkSelector "getObjectValue:forString:errorDescription:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @phonetic@
phoneticSelector :: Selector
phoneticSelector = mkSelector "phonetic"

-- | @Selector@ for @setPhonetic:@
setPhoneticSelector :: Selector
setPhoneticSelector = mkSelector "setPhonetic:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

