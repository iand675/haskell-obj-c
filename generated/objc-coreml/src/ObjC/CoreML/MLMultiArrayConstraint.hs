{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Constraint describing expected MLMultiArray properties
--
-- Generated bindings for @MLMultiArrayConstraint@.
module ObjC.CoreML.MLMultiArrayConstraint
  ( MLMultiArrayConstraint
  , IsMLMultiArrayConstraint(..)
  , shape
  , dataType
  , shapeConstraint
  , shapeSelector
  , dataTypeSelector
  , shapeConstraintSelector

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

-- | @- shape@
shape :: IsMLMultiArrayConstraint mlMultiArrayConstraint => mlMultiArrayConstraint -> IO (Id NSArray)
shape mlMultiArrayConstraint  =
  sendMsg mlMultiArrayConstraint (mkSelector "shape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dataType@
dataType :: IsMLMultiArrayConstraint mlMultiArrayConstraint => mlMultiArrayConstraint -> IO MLMultiArrayDataType
dataType mlMultiArrayConstraint  =
  fmap (coerce :: CLong -> MLMultiArrayDataType) $ sendMsg mlMultiArrayConstraint (mkSelector "dataType") retCLong []

-- | @- shapeConstraint@
shapeConstraint :: IsMLMultiArrayConstraint mlMultiArrayConstraint => mlMultiArrayConstraint -> IO (Id MLMultiArrayShapeConstraint)
shapeConstraint mlMultiArrayConstraint  =
  sendMsg mlMultiArrayConstraint (mkSelector "shapeConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shape@
shapeSelector :: Selector
shapeSelector = mkSelector "shape"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @shapeConstraint@
shapeConstraintSelector :: Selector
shapeConstraintSelector = mkSelector "shapeConstraint"

