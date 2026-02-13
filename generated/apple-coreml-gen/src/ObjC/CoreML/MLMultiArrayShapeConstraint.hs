{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLMultiArrayShapeConstraint@.
module ObjC.CoreML.MLMultiArrayShapeConstraint
  ( MLMultiArrayShapeConstraint
  , IsMLMultiArrayShapeConstraint(..)
  , type_
  , sizeRangeForDimension
  , enumeratedShapes
  , enumeratedShapesSelector
  , sizeRangeForDimensionSelector
  , typeSelector

  -- * Enum types
  , MLMultiArrayShapeConstraintType(MLMultiArrayShapeConstraintType)
  , pattern MLMultiArrayShapeConstraintTypeUnspecified
  , pattern MLMultiArrayShapeConstraintTypeEnumerated
  , pattern MLMultiArrayShapeConstraintTypeRange

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

-- | @- type@
type_ :: IsMLMultiArrayShapeConstraint mlMultiArrayShapeConstraint => mlMultiArrayShapeConstraint -> IO MLMultiArrayShapeConstraintType
type_ mlMultiArrayShapeConstraint =
  sendMessage mlMultiArrayShapeConstraint typeSelector

-- | @- sizeRangeForDimension@
sizeRangeForDimension :: IsMLMultiArrayShapeConstraint mlMultiArrayShapeConstraint => mlMultiArrayShapeConstraint -> IO (Id NSArray)
sizeRangeForDimension mlMultiArrayShapeConstraint =
  sendMessage mlMultiArrayShapeConstraint sizeRangeForDimensionSelector

-- | @- enumeratedShapes@
enumeratedShapes :: IsMLMultiArrayShapeConstraint mlMultiArrayShapeConstraint => mlMultiArrayShapeConstraint -> IO (Id NSArray)
enumeratedShapes mlMultiArrayShapeConstraint =
  sendMessage mlMultiArrayShapeConstraint enumeratedShapesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] MLMultiArrayShapeConstraintType
typeSelector = mkSelector "type"

-- | @Selector@ for @sizeRangeForDimension@
sizeRangeForDimensionSelector :: Selector '[] (Id NSArray)
sizeRangeForDimensionSelector = mkSelector "sizeRangeForDimension"

-- | @Selector@ for @enumeratedShapes@
enumeratedShapesSelector :: Selector '[] (Id NSArray)
enumeratedShapesSelector = mkSelector "enumeratedShapes"

