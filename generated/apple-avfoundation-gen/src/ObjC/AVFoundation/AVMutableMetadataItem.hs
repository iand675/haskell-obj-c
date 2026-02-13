{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMutableMetadataItem@.
module ObjC.AVFoundation.AVMutableMetadataItem
  ( AVMutableMetadataItem
  , IsAVMutableMetadataItem(..)
  , metadataItem
  , identifier
  , setIdentifier
  , extendedLanguageTag
  , setExtendedLanguageTag
  , locale
  , setLocale
  , dataType
  , setDataType
  , value
  , setValue
  , extraAttributes
  , setExtraAttributes
  , keySpace
  , setKeySpace
  , key
  , setKey
  , startDate
  , setStartDate
  , dataTypeSelector
  , extendedLanguageTagSelector
  , extraAttributesSelector
  , identifierSelector
  , keySelector
  , keySpaceSelector
  , localeSelector
  , metadataItemSelector
  , setDataTypeSelector
  , setExtendedLanguageTagSelector
  , setExtraAttributesSelector
  , setIdentifierSelector
  , setKeySelector
  , setKeySpaceSelector
  , setLocaleSelector
  , setStartDateSelector
  , setValueSelector
  , startDateSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | metadataItem
--
-- Returns an instance of AVMutableMetadataItem.
--
-- ObjC selector: @+ metadataItem@
metadataItem :: IO (Id AVMutableMetadataItem)
metadataItem  =
  do
    cls' <- getRequiredClass "AVMutableMetadataItem"
    sendClassMessage cls' metadataItemSelector

-- | @- identifier@
identifier :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSString)
identifier avMutableMetadataItem =
  sendMessage avMutableMetadataItem identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSString value) => avMutableMetadataItem -> value -> IO ()
setIdentifier avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setIdentifierSelector (toNSString value)

-- | @- extendedLanguageTag@
extendedLanguageTag :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSString)
extendedLanguageTag avMutableMetadataItem =
  sendMessage avMutableMetadataItem extendedLanguageTagSelector

-- | @- setExtendedLanguageTag:@
setExtendedLanguageTag :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSString value) => avMutableMetadataItem -> value -> IO ()
setExtendedLanguageTag avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setExtendedLanguageTagSelector (toNSString value)

-- | @- locale@
locale :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSLocale)
locale avMutableMetadataItem =
  sendMessage avMutableMetadataItem localeSelector

-- | @- setLocale:@
setLocale :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSLocale value) => avMutableMetadataItem -> value -> IO ()
setLocale avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setLocaleSelector (toNSLocale value)

-- | @- dataType@
dataType :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSString)
dataType avMutableMetadataItem =
  sendMessage avMutableMetadataItem dataTypeSelector

-- | @- setDataType:@
setDataType :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSString value) => avMutableMetadataItem -> value -> IO ()
setDataType avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setDataTypeSelector (toNSString value)

-- | @- value@
value :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO RawId
value avMutableMetadataItem =
  sendMessage avMutableMetadataItem valueSelector

-- | @- setValue:@
setValue :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> RawId -> IO ()
setValue avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setValueSelector value

-- | @- extraAttributes@
extraAttributes :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSDictionary)
extraAttributes avMutableMetadataItem =
  sendMessage avMutableMetadataItem extraAttributesSelector

-- | @- setExtraAttributes:@
setExtraAttributes :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSDictionary value) => avMutableMetadataItem -> value -> IO ()
setExtraAttributes avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setExtraAttributesSelector (toNSDictionary value)

-- | @- keySpace@
keySpace :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSString)
keySpace avMutableMetadataItem =
  sendMessage avMutableMetadataItem keySpaceSelector

-- | @- setKeySpace:@
setKeySpace :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSString value) => avMutableMetadataItem -> value -> IO ()
setKeySpace avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setKeySpaceSelector (toNSString value)

-- | @- key@
key :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO RawId
key avMutableMetadataItem =
  sendMessage avMutableMetadataItem keySelector

-- | @- setKey:@
setKey :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> RawId -> IO ()
setKey avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setKeySelector value

-- | @- startDate@
startDate :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSDate)
startDate avMutableMetadataItem =
  sendMessage avMutableMetadataItem startDateSelector

-- | @- setStartDate:@
setStartDate :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSDate value) => avMutableMetadataItem -> value -> IO ()
setStartDate avMutableMetadataItem value =
  sendMessage avMutableMetadataItem setStartDateSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataItem@
metadataItemSelector :: Selector '[] (Id AVMutableMetadataItem)
metadataItemSelector = mkSelector "metadataItem"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector '[] (Id NSString)
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @setExtendedLanguageTag:@
setExtendedLanguageTagSelector :: Selector '[Id NSString] ()
setExtendedLanguageTagSelector = mkSelector "setExtendedLanguageTag:"

-- | @Selector@ for @locale@
localeSelector :: Selector '[] (Id NSLocale)
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector '[Id NSLocale] ()
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] (Id NSString)
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector '[Id NSString] ()
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[RawId] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @extraAttributes@
extraAttributesSelector :: Selector '[] (Id NSDictionary)
extraAttributesSelector = mkSelector "extraAttributes"

-- | @Selector@ for @setExtraAttributes:@
setExtraAttributesSelector :: Selector '[Id NSDictionary] ()
setExtraAttributesSelector = mkSelector "setExtraAttributes:"

-- | @Selector@ for @keySpace@
keySpaceSelector :: Selector '[] (Id NSString)
keySpaceSelector = mkSelector "keySpace"

-- | @Selector@ for @setKeySpace:@
setKeySpaceSelector :: Selector '[Id NSString] ()
setKeySpaceSelector = mkSelector "setKeySpace:"

-- | @Selector@ for @key@
keySelector :: Selector '[] RawId
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector '[RawId] ()
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector '[Id NSDate] ()
setStartDateSelector = mkSelector "setStartDate:"

