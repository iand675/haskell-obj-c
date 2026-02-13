{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , setValueSelector
  , valueSelector
  , valueTypeSelector

  -- * Enum types
  , CLSBinaryValueType(CLSBinaryValueType)
  , pattern CLSBinaryValueTypeTrueFalse
  , pattern CLSBinaryValueTypePassFail
  , pattern CLSBinaryValueTypeYesNo
  , pattern CLSBinaryValueTypeCorrectIncorrect

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithIdentifier_title_type clsBinaryItem identifier title valueType =
  sendOwnedMessage clsBinaryItem initWithIdentifier_title_typeSelector (toNSString identifier) (toNSString title) valueType

-- | True or false value.
--
-- ObjC selector: @- value@
value :: IsCLSBinaryItem clsBinaryItem => clsBinaryItem -> IO Bool
value clsBinaryItem =
  sendMessage clsBinaryItem valueSelector

-- | True or false value.
--
-- ObjC selector: @- setValue:@
setValue :: IsCLSBinaryItem clsBinaryItem => clsBinaryItem -> Bool -> IO ()
setValue clsBinaryItem value =
  sendMessage clsBinaryItem setValueSelector value

-- | Value type of this CLSBinaryItem.
--
-- The type that best describes this CLSBinaryItem value.
--
-- ObjC selector: @- valueType@
valueType :: IsCLSBinaryItem clsBinaryItem => clsBinaryItem -> IO CLSBinaryValueType
valueType clsBinaryItem =
  sendMessage clsBinaryItem valueTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:type:@
initWithIdentifier_title_typeSelector :: Selector '[Id NSString, Id NSString, CLSBinaryValueType] (Id CLSBinaryItem)
initWithIdentifier_title_typeSelector = mkSelector "initWithIdentifier:title:type:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] Bool
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Bool] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @valueType@
valueTypeSelector :: Selector '[] CLSBinaryValueType
valueTypeSelector = mkSelector "valueType"

