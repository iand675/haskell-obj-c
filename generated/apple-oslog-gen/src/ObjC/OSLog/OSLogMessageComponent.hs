{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OSLogMessageComponent
--
-- The message arguments for a particular entry. There is one component for each placeholder in the formatString plus one component for any text after the last placeholder.
--
-- Generated bindings for @OSLogMessageComponent@.
module ObjC.OSLog.OSLogMessageComponent
  ( OSLogMessageComponent
  , IsOSLogMessageComponent(..)
  , formatSubstring
  , placeholder
  , argumentCategory
  , argumentDataValue
  , argumentDoubleValue
  , argumentInt64Value
  , argumentNumberValue
  , argumentStringValue
  , argumentUInt64Value
  , argumentCategorySelector
  , argumentDataValueSelector
  , argumentDoubleValueSelector
  , argumentInt64ValueSelector
  , argumentNumberValueSelector
  , argumentStringValueSelector
  , argumentUInt64ValueSelector
  , formatSubstringSelector
  , placeholderSelector

  -- * Enum types
  , OSLogMessageComponentArgumentCategory(OSLogMessageComponentArgumentCategory)
  , pattern OSLogMessageComponentArgumentCategoryUndefined
  , pattern OSLogMessageComponentArgumentCategoryData
  , pattern OSLogMessageComponentArgumentCategoryDouble
  , pattern OSLogMessageComponentArgumentCategoryInt64
  , pattern OSLogMessageComponentArgumentCategoryString
  , pattern OSLogMessageComponentArgumentCategoryUInt64

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSLog.Internal.Classes
import ObjC.OSLog.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | formatSubstring
--
-- The text immediately preceding a placeholder. This can be an empty string if there is nothing between two placeholders, or between the placeholder and the bounds of the string.
--
-- ObjC selector: @- formatSubstring@
formatSubstring :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSString)
formatSubstring osLogMessageComponent =
  sendMessage osLogMessageComponent formatSubstringSelector

-- | placeholder
--
-- The placeholder text. Is empty for is the last component.
--
-- ObjC selector: @- placeholder@
placeholder :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSString)
placeholder osLogMessageComponent =
  sendMessage osLogMessageComponent placeholderSelector

-- | argumentCategory
--
-- The type of argument corresponding to the placeholder; see OSLogMessageComponentArgumentCategory.
--
-- ObjC selector: @- argumentCategory@
argumentCategory :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO OSLogMessageComponentArgumentCategory
argumentCategory osLogMessageComponent =
  sendMessage osLogMessageComponent argumentCategorySelector

-- | argumentDataValue
--
-- The argument as a sequence of bytes. Can be nil if the argument cannot be decoded (for example, it could be redacted), or if this is the last component.
--
-- ObjC selector: @- argumentDataValue@
argumentDataValue :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSData)
argumentDataValue osLogMessageComponent =
  sendMessage osLogMessageComponent argumentDataValueSelector

-- | argumentDoubleValue
--
-- The argument as a double-precision floating point number; the value is undefined if the argument cannot be decoded or if this is the last component.
--
-- ObjC selector: @- argumentDoubleValue@
argumentDoubleValue :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO CDouble
argumentDoubleValue osLogMessageComponent =
  sendMessage osLogMessageComponent argumentDoubleValueSelector

-- | argumentInt64Value
--
-- The argument as a 64-bit signed integer; the value is undefined if it cannot be decoded or if this is the last component.
--
-- ObjC selector: @- argumentInt64Value@
argumentInt64Value :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO CLong
argumentInt64Value osLogMessageComponent =
  sendMessage osLogMessageComponent argumentInt64ValueSelector

-- | argumentNumberValue
--
-- The argument as a number. Can be nil if the argument cannot be decoded (for example, it could be redacted), or if this is the last component.
--
-- ObjC selector: @- argumentNumberValue@
argumentNumberValue :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSNumber)
argumentNumberValue osLogMessageComponent =
  sendMessage osLogMessageComponent argumentNumberValueSelector

-- | argumentStringValue
--
-- The argument as a string. Can be nil if the argument cannot be decoded (for example, it could be redacted), or if this is the last component.
--
-- ObjC selector: @- argumentStringValue@
argumentStringValue :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSString)
argumentStringValue osLogMessageComponent =
  sendMessage osLogMessageComponent argumentStringValueSelector

-- | argumentUInt64Value
--
-- The argument as a 64-bit unsigned integer; the value is undefined if the argument cannot be decoded or if this is the last component.
--
-- ObjC selector: @- argumentUInt64Value@
argumentUInt64Value :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO CULong
argumentUInt64Value osLogMessageComponent =
  sendMessage osLogMessageComponent argumentUInt64ValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @formatSubstring@
formatSubstringSelector :: Selector '[] (Id NSString)
formatSubstringSelector = mkSelector "formatSubstring"

-- | @Selector@ for @placeholder@
placeholderSelector :: Selector '[] (Id NSString)
placeholderSelector = mkSelector "placeholder"

-- | @Selector@ for @argumentCategory@
argumentCategorySelector :: Selector '[] OSLogMessageComponentArgumentCategory
argumentCategorySelector = mkSelector "argumentCategory"

-- | @Selector@ for @argumentDataValue@
argumentDataValueSelector :: Selector '[] (Id NSData)
argumentDataValueSelector = mkSelector "argumentDataValue"

-- | @Selector@ for @argumentDoubleValue@
argumentDoubleValueSelector :: Selector '[] CDouble
argumentDoubleValueSelector = mkSelector "argumentDoubleValue"

-- | @Selector@ for @argumentInt64Value@
argumentInt64ValueSelector :: Selector '[] CLong
argumentInt64ValueSelector = mkSelector "argumentInt64Value"

-- | @Selector@ for @argumentNumberValue@
argumentNumberValueSelector :: Selector '[] (Id NSNumber)
argumentNumberValueSelector = mkSelector "argumentNumberValue"

-- | @Selector@ for @argumentStringValue@
argumentStringValueSelector :: Selector '[] (Id NSString)
argumentStringValueSelector = mkSelector "argumentStringValue"

-- | @Selector@ for @argumentUInt64Value@
argumentUInt64ValueSelector :: Selector '[] CULong
argumentUInt64ValueSelector = mkSelector "argumentUInt64Value"

