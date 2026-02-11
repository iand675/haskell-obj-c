{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Constraint of a state feature value.
--
-- Generated bindings for @MLStateConstraint@.
module ObjC.CoreML.MLStateConstraint
  ( MLStateConstraint
  , IsMLStateConstraint(..)
  , dataType
  , dataTypeSelector

  -- * Enum types
  , MLMultiArrayDataType(MLMultiArrayDataType)
  , pattern MLMultiArrayDataTypeDouble
  , pattern MLMultiArrayDataTypeFloat64
  , pattern MLMultiArrayDataTypeFloat32
  , pattern MLMultiArrayDataTypeFloat16
  , pattern MLMultiArrayDataTypeFloat
  , pattern MLMultiArrayDataTypeInt32
  , pattern MLMultiArrayDataTypeInt8

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

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The data type of scalars in the state buffer.
--
-- ObjC selector: @- dataType@
dataType :: IsMLStateConstraint mlStateConstraint => mlStateConstraint -> IO MLMultiArrayDataType
dataType mlStateConstraint  =
  fmap (coerce :: CLong -> MLMultiArrayDataType) $ sendMsg mlStateConstraint (mkSelector "dataType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

