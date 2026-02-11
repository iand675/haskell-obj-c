{-# LANGUAGE PatternSynonyms #-}
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
  , formatSubstringSelector
  , placeholderSelector
  , argumentCategorySelector
  , argumentDataValueSelector
  , argumentDoubleValueSelector
  , argumentInt64ValueSelector
  , argumentNumberValueSelector
  , argumentStringValueSelector
  , argumentUInt64ValueSelector

  -- * Enum types
  , OSLogMessageComponentArgumentCategory(OSLogMessageComponentArgumentCategory)
  , pattern OSLogMessageComponentArgumentCategoryUndefined
  , pattern OSLogMessageComponentArgumentCategoryData
  , pattern OSLogMessageComponentArgumentCategoryDouble
  , pattern OSLogMessageComponentArgumentCategoryInt64
  , pattern OSLogMessageComponentArgumentCategoryString
  , pattern OSLogMessageComponentArgumentCategoryUInt64

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

import ObjC.OSLog.Internal.Classes
import ObjC.OSLog.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | formatSubstring
--
-- The text immediately preceding a placeholder. This can be an empty string if there is nothing between two placeholders, or between the placeholder and the bounds of the string.
--
-- ObjC selector: @- formatSubstring@
formatSubstring :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSString)
formatSubstring osLogMessageComponent  =
  sendMsg osLogMessageComponent (mkSelector "formatSubstring") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | placeholder
--
-- The placeholder text. Is empty for is the last component.
--
-- ObjC selector: @- placeholder@
placeholder :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSString)
placeholder osLogMessageComponent  =
  sendMsg osLogMessageComponent (mkSelector "placeholder") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | argumentCategory
--
-- The type of argument corresponding to the placeholder; see OSLogMessageComponentArgumentCategory.
--
-- ObjC selector: @- argumentCategory@
argumentCategory :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO OSLogMessageComponentArgumentCategory
argumentCategory osLogMessageComponent  =
  fmap (coerce :: CLong -> OSLogMessageComponentArgumentCategory) $ sendMsg osLogMessageComponent (mkSelector "argumentCategory") retCLong []

-- | argumentDataValue
--
-- The argument as a sequence of bytes. Can be nil if the argument cannot be decoded (for example, it could be redacted), or if this is the last component.
--
-- ObjC selector: @- argumentDataValue@
argumentDataValue :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSData)
argumentDataValue osLogMessageComponent  =
  sendMsg osLogMessageComponent (mkSelector "argumentDataValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | argumentDoubleValue
--
-- The argument as a double-precision floating point number; the value is undefined if the argument cannot be decoded or if this is the last component.
--
-- ObjC selector: @- argumentDoubleValue@
argumentDoubleValue :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO CDouble
argumentDoubleValue osLogMessageComponent  =
  sendMsg osLogMessageComponent (mkSelector "argumentDoubleValue") retCDouble []

-- | argumentInt64Value
--
-- The argument as a 64-bit signed integer; the value is undefined if it cannot be decoded or if this is the last component.
--
-- ObjC selector: @- argumentInt64Value@
argumentInt64Value :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO CLong
argumentInt64Value osLogMessageComponent  =
  sendMsg osLogMessageComponent (mkSelector "argumentInt64Value") retCLong []

-- | argumentNumberValue
--
-- The argument as a number. Can be nil if the argument cannot be decoded (for example, it could be redacted), or if this is the last component.
--
-- ObjC selector: @- argumentNumberValue@
argumentNumberValue :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSNumber)
argumentNumberValue osLogMessageComponent  =
  sendMsg osLogMessageComponent (mkSelector "argumentNumberValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | argumentStringValue
--
-- The argument as a string. Can be nil if the argument cannot be decoded (for example, it could be redacted), or if this is the last component.
--
-- ObjC selector: @- argumentStringValue@
argumentStringValue :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO (Id NSString)
argumentStringValue osLogMessageComponent  =
  sendMsg osLogMessageComponent (mkSelector "argumentStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | argumentUInt64Value
--
-- The argument as a 64-bit unsigned integer; the value is undefined if the argument cannot be decoded or if this is the last component.
--
-- ObjC selector: @- argumentUInt64Value@
argumentUInt64Value :: IsOSLogMessageComponent osLogMessageComponent => osLogMessageComponent -> IO CULong
argumentUInt64Value osLogMessageComponent  =
  sendMsg osLogMessageComponent (mkSelector "argumentUInt64Value") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @formatSubstring@
formatSubstringSelector :: Selector
formatSubstringSelector = mkSelector "formatSubstring"

-- | @Selector@ for @placeholder@
placeholderSelector :: Selector
placeholderSelector = mkSelector "placeholder"

-- | @Selector@ for @argumentCategory@
argumentCategorySelector :: Selector
argumentCategorySelector = mkSelector "argumentCategory"

-- | @Selector@ for @argumentDataValue@
argumentDataValueSelector :: Selector
argumentDataValueSelector = mkSelector "argumentDataValue"

-- | @Selector@ for @argumentDoubleValue@
argumentDoubleValueSelector :: Selector
argumentDoubleValueSelector = mkSelector "argumentDoubleValue"

-- | @Selector@ for @argumentInt64Value@
argumentInt64ValueSelector :: Selector
argumentInt64ValueSelector = mkSelector "argumentInt64Value"

-- | @Selector@ for @argumentNumberValue@
argumentNumberValueSelector :: Selector
argumentNumberValueSelector = mkSelector "argumentNumberValue"

-- | @Selector@ for @argumentStringValue@
argumentStringValueSelector :: Selector
argumentStringValueSelector = mkSelector "argumentStringValue"

-- | @Selector@ for @argumentUInt64Value@
argumentUInt64ValueSelector :: Selector
argumentUInt64ValueSelector = mkSelector "argumentUInt64Value"

