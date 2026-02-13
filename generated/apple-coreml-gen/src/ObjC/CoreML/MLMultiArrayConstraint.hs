{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataTypeSelector
  , shapeConstraintSelector
  , shapeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- shape@
shape :: IsMLMultiArrayConstraint mlMultiArrayConstraint => mlMultiArrayConstraint -> IO (Id NSArray)
shape mlMultiArrayConstraint =
  sendMessage mlMultiArrayConstraint shapeSelector

-- | @- dataType@
dataType :: IsMLMultiArrayConstraint mlMultiArrayConstraint => mlMultiArrayConstraint -> IO MLMultiArrayDataType
dataType mlMultiArrayConstraint =
  sendMessage mlMultiArrayConstraint dataTypeSelector

-- | @- shapeConstraint@
shapeConstraint :: IsMLMultiArrayConstraint mlMultiArrayConstraint => mlMultiArrayConstraint -> IO (Id MLMultiArrayShapeConstraint)
shapeConstraint mlMultiArrayConstraint =
  sendMessage mlMultiArrayConstraint shapeConstraintSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shape@
shapeSelector :: Selector '[] (Id NSArray)
shapeSelector = mkSelector "shape"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MLMultiArrayDataType
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @shapeConstraint@
shapeConstraintSelector :: Selector '[] (Id MLMultiArrayShapeConstraint)
shapeConstraintSelector = mkSelector "shapeConstraint"

