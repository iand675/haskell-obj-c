{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CLSBinaryItem represents user generated information that is true or false, pass or fail, yes or no.
--
-- Generated bindings for @CLSBinaryItem@.
module ObjC.ClassKit.CLSBinaryItem
  ( CLSBinaryItem
  , IsCLSBinaryItem(..)
  , initWithIdentifier_title_type
  , value
  , setValue
  , valueType
  , initWithIdentifier_title_typeSelector
  , valueSelector
  , setValueSelector
  , valueTypeSelector

  -- * Enum types
  , CLSBinaryValueType(CLSBinaryValueType)
  , pattern CLSBinaryValueTypeTrueFalse
  , pattern CLSBinaryValueTypePassFail
  , pattern CLSBinaryValueTypeYesNo
  , pattern CLSBinaryValueTypeCorrectIncorrect

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

import ObjC.ClassKit.Internal.Classes
import ObjC.ClassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create an item that represents a binary value
--
-- @title@ — Title of the CLSBinaryItem.
--
-- @identifier@ — An identifier that is unique within its owning activity.
--
-- @valueType@ — The type of binary value. Ex. pass or fail.
--
-- ObjC selector: @- initWithIdentifier:title:type:@
initWithIdentifier_title_type :: (IsCLSBinaryItem clsBinaryItem, IsNSString identifier, IsNSString title) => clsBinaryItem -> identifier -> title -> CLSBinaryValueType -> IO (Id CLSBinaryItem)
initWithIdentifier_title_type clsBinaryItem  identifier title valueType =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr title $ \raw_title ->
      sendMsg clsBinaryItem (mkSelector "initWithIdentifier:title:type:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCLong (coerce valueType)] >>= ownedObject . castPtr

-- | True or false value.
--
-- ObjC selector: @- value@
value :: IsCLSBinaryItem clsBinaryItem => clsBinaryItem -> IO Bool
value clsBinaryItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clsBinaryItem (mkSelector "value") retCULong []

-- | True or false value.
--
-- ObjC selector: @- setValue:@
setValue :: IsCLSBinaryItem clsBinaryItem => clsBinaryItem -> Bool -> IO ()
setValue clsBinaryItem  value =
  sendMsg clsBinaryItem (mkSelector "setValue:") retVoid [argCULong (if value then 1 else 0)]

-- | Value type of this CLSBinaryItem.
--
-- The type that best describes this CLSBinaryItem value.
--
-- ObjC selector: @- valueType@
valueType :: IsCLSBinaryItem clsBinaryItem => clsBinaryItem -> IO CLSBinaryValueType
valueType clsBinaryItem  =
  fmap (coerce :: CLong -> CLSBinaryValueType) $ sendMsg clsBinaryItem (mkSelector "valueType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:type:@
initWithIdentifier_title_typeSelector :: Selector
initWithIdentifier_title_typeSelector = mkSelector "initWithIdentifier:title:type:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @valueType@
valueTypeSelector :: Selector
valueTypeSelector = mkSelector "valueType"

