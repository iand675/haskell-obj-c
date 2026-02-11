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
  , extraAttributes
  , setExtraAttributes
  , keySpace
  , setKeySpace
  , startDate
  , setStartDate
  , metadataItemSelector
  , identifierSelector
  , setIdentifierSelector
  , extendedLanguageTagSelector
  , setExtendedLanguageTagSelector
  , localeSelector
  , setLocaleSelector
  , dataTypeSelector
  , setDataTypeSelector
  , extraAttributesSelector
  , setExtraAttributesSelector
  , keySpaceSelector
  , setKeySpaceSelector
  , startDateSelector
  , setStartDateSelector


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
    sendClassMsg cls' (mkSelector "metadataItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSString)
identifier avMutableMetadataItem  =
  sendMsg avMutableMetadataItem (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSString value) => avMutableMetadataItem -> value -> IO ()
setIdentifier avMutableMetadataItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMetadataItem (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extendedLanguageTag@
extendedLanguageTag :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSString)
extendedLanguageTag avMutableMetadataItem  =
  sendMsg avMutableMetadataItem (mkSelector "extendedLanguageTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtendedLanguageTag:@
setExtendedLanguageTag :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSString value) => avMutableMetadataItem -> value -> IO ()
setExtendedLanguageTag avMutableMetadataItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMetadataItem (mkSelector "setExtendedLanguageTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- locale@
locale :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSLocale)
locale avMutableMetadataItem  =
  sendMsg avMutableMetadataItem (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocale:@
setLocale :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSLocale value) => avMutableMetadataItem -> value -> IO ()
setLocale avMutableMetadataItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMetadataItem (mkSelector "setLocale:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dataType@
dataType :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSString)
dataType avMutableMetadataItem  =
  sendMsg avMutableMetadataItem (mkSelector "dataType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDataType:@
setDataType :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSString value) => avMutableMetadataItem -> value -> IO ()
setDataType avMutableMetadataItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMetadataItem (mkSelector "setDataType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extraAttributes@
extraAttributes :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSDictionary)
extraAttributes avMutableMetadataItem  =
  sendMsg avMutableMetadataItem (mkSelector "extraAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtraAttributes:@
setExtraAttributes :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSDictionary value) => avMutableMetadataItem -> value -> IO ()
setExtraAttributes avMutableMetadataItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMetadataItem (mkSelector "setExtraAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keySpace@
keySpace :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSString)
keySpace avMutableMetadataItem  =
  sendMsg avMutableMetadataItem (mkSelector "keySpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeySpace:@
setKeySpace :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSString value) => avMutableMetadataItem -> value -> IO ()
setKeySpace avMutableMetadataItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMetadataItem (mkSelector "setKeySpace:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startDate@
startDate :: IsAVMutableMetadataItem avMutableMetadataItem => avMutableMetadataItem -> IO (Id NSDate)
startDate avMutableMetadataItem  =
  sendMsg avMutableMetadataItem (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartDate:@
setStartDate :: (IsAVMutableMetadataItem avMutableMetadataItem, IsNSDate value) => avMutableMetadataItem -> value -> IO ()
setStartDate avMutableMetadataItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableMetadataItem (mkSelector "setStartDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metadataItem@
metadataItemSelector :: Selector
metadataItemSelector = mkSelector "metadataItem"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @extendedLanguageTag@
extendedLanguageTagSelector :: Selector
extendedLanguageTagSelector = mkSelector "extendedLanguageTag"

-- | @Selector@ for @setExtendedLanguageTag:@
setExtendedLanguageTagSelector :: Selector
setExtendedLanguageTagSelector = mkSelector "setExtendedLanguageTag:"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @setLocale:@
setLocaleSelector :: Selector
setLocaleSelector = mkSelector "setLocale:"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @setDataType:@
setDataTypeSelector :: Selector
setDataTypeSelector = mkSelector "setDataType:"

-- | @Selector@ for @extraAttributes@
extraAttributesSelector :: Selector
extraAttributesSelector = mkSelector "extraAttributes"

-- | @Selector@ for @setExtraAttributes:@
setExtraAttributesSelector :: Selector
setExtraAttributesSelector = mkSelector "setExtraAttributes:"

-- | @Selector@ for @keySpace@
keySpaceSelector :: Selector
keySpaceSelector = mkSelector "keySpace"

-- | @Selector@ for @setKeySpace:@
setKeySpaceSelector :: Selector
setKeySpaceSelector = mkSelector "setKeySpace:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @setStartDate:@
setStartDateSelector :: Selector
setStartDateSelector = mkSelector "setStartDate:"

