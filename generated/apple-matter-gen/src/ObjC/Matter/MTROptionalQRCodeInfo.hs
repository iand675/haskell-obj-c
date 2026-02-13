{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An optional information item present in the setup payload.
--
-- Note that while the Matter specification allows elements containing arbitrary TLV data types, this implementation currently only supports String and Int32 values.
--
-- Objects of this type are immutable; calling any deprecated property setters has no effect.
--
-- Generated bindings for @MTROptionalQRCodeInfo@.
module ObjC.Matter.MTROptionalQRCodeInfo
  ( MTROptionalQRCodeInfo
  , IsMTROptionalQRCodeInfo(..)
  , initWithTag_stringValue
  , initWithTag_int32Value
  , init_
  , setType
  , setTag
  , setIntegerValue
  , setStringValue
  , type_
  , tag
  , integerValue
  , stringValue
  , infoType
  , setInfoType
  , infoTypeSelector
  , initSelector
  , initWithTag_int32ValueSelector
  , initWithTag_stringValueSelector
  , integerValueSelector
  , setInfoTypeSelector
  , setIntegerValueSelector
  , setStringValueSelector
  , setTagSelector
  , setTypeSelector
  , stringValueSelector
  , tagSelector
  , typeSelector

  -- * Enum types
  , MTROptionalQRCodeInfoType(MTROptionalQRCodeInfoType)
  , pattern MTROptionalQRCodeInfoTypeUnknown
  , pattern MTROptionalQRCodeInfoTypeString
  , pattern MTROptionalQRCodeInfoTypeInt32

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Matter.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes the object with a tag and string value. The tag must be in the range 0x80 - 0xFF.
--
-- ObjC selector: @- initWithTag:stringValue:@
initWithTag_stringValue :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber tag, IsNSString value) => mtrOptionalQRCodeInfo -> tag -> value -> IO (Id MTROptionalQRCodeInfo)
initWithTag_stringValue mtrOptionalQRCodeInfo tag value =
  sendOwnedMessage mtrOptionalQRCodeInfo initWithTag_stringValueSelector (toNSNumber tag) (toNSString value)

-- | Initializes the object with a tag and int32 value. The tag must be in the range 0x80 - 0xFF.
--
-- ObjC selector: @- initWithTag:int32Value:@
initWithTag_int32Value :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber tag) => mtrOptionalQRCodeInfo -> tag -> CInt -> IO (Id MTROptionalQRCodeInfo)
initWithTag_int32Value mtrOptionalQRCodeInfo tag value =
  sendOwnedMessage mtrOptionalQRCodeInfo initWithTag_int32ValueSelector (toNSNumber tag) value

-- | @- init@
init_ :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id MTROptionalQRCodeInfo)
init_ mtrOptionalQRCodeInfo =
  sendOwnedMessage mtrOptionalQRCodeInfo initSelector

-- | @- setType:@
setType :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> MTROptionalQRCodeInfoType -> IO ()
setType mtrOptionalQRCodeInfo type_ =
  sendMessage mtrOptionalQRCodeInfo setTypeSelector type_

-- | @- setTag:@
setTag :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber tag) => mtrOptionalQRCodeInfo -> tag -> IO ()
setTag mtrOptionalQRCodeInfo tag =
  sendMessage mtrOptionalQRCodeInfo setTagSelector (toNSNumber tag)

-- | @- setIntegerValue:@
setIntegerValue :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber integerValue) => mtrOptionalQRCodeInfo -> integerValue -> IO ()
setIntegerValue mtrOptionalQRCodeInfo integerValue =
  sendMessage mtrOptionalQRCodeInfo setIntegerValueSelector (toNSNumber integerValue)

-- | @- setStringValue:@
setStringValue :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSString stringValue) => mtrOptionalQRCodeInfo -> stringValue -> IO ()
setStringValue mtrOptionalQRCodeInfo stringValue =
  sendMessage mtrOptionalQRCodeInfo setStringValueSelector (toNSString stringValue)

-- | @- type@
type_ :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO MTROptionalQRCodeInfoType
type_ mtrOptionalQRCodeInfo =
  sendMessage mtrOptionalQRCodeInfo typeSelector

-- | The vendor-specific TLV tag number for this information item.
--
-- Vendor-specific elements have tags in the range 0x80 - 0xFF.
--
-- ObjC selector: @- tag@
tag :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id NSNumber)
tag mtrOptionalQRCodeInfo =
  sendMessage mtrOptionalQRCodeInfo tagSelector

-- | The value held in this extension element, if @type@ is an integer type, or nil otherwise.
--
-- ObjC selector: @- integerValue@
integerValue :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id NSNumber)
integerValue mtrOptionalQRCodeInfo =
  sendMessage mtrOptionalQRCodeInfo integerValueSelector

-- | The value held in this extension element, if @type@ is @MTROptionalQRCodeInfoTypeString@, or nil otherwise.
--
-- ObjC selector: @- stringValue@
stringValue :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id NSString)
stringValue mtrOptionalQRCodeInfo =
  sendMessage mtrOptionalQRCodeInfo stringValueSelector

-- | @- infoType@
infoType :: IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo => mtrOptionalQRCodeInfo -> IO (Id NSNumber)
infoType mtrOptionalQRCodeInfo =
  sendMessage mtrOptionalQRCodeInfo infoTypeSelector

-- | @- setInfoType:@
setInfoType :: (IsMTROptionalQRCodeInfo mtrOptionalQRCodeInfo, IsNSNumber value) => mtrOptionalQRCodeInfo -> value -> IO ()
setInfoType mtrOptionalQRCodeInfo value =
  sendMessage mtrOptionalQRCodeInfo setInfoTypeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTag:stringValue:@
initWithTag_stringValueSelector :: Selector '[Id NSNumber, Id NSString] (Id MTROptionalQRCodeInfo)
initWithTag_stringValueSelector = mkSelector "initWithTag:stringValue:"

-- | @Selector@ for @initWithTag:int32Value:@
initWithTag_int32ValueSelector :: Selector '[Id NSNumber, CInt] (Id MTROptionalQRCodeInfo)
initWithTag_int32ValueSelector = mkSelector "initWithTag:int32Value:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTROptionalQRCodeInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[MTROptionalQRCodeInfoType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector '[Id NSNumber] ()
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @setIntegerValue:@
setIntegerValueSelector :: Selector '[Id NSNumber] ()
setIntegerValueSelector = mkSelector "setIntegerValue:"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector '[Id NSString] ()
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MTROptionalQRCodeInfoType
typeSelector = mkSelector "type"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] (Id NSNumber)
tagSelector = mkSelector "tag"

-- | @Selector@ for @integerValue@
integerValueSelector :: Selector '[] (Id NSNumber)
integerValueSelector = mkSelector "integerValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @infoType@
infoTypeSelector :: Selector '[] (Id NSNumber)
infoTypeSelector = mkSelector "infoType"

-- | @Selector@ for @setInfoType:@
setInfoTypeSelector :: Selector '[Id NSNumber] ()
setInfoTypeSelector = mkSelector "setInfoType:"

