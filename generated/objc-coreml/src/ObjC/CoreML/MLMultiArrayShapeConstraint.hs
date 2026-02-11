{-# LANGUAGE PatternSynonyms #-}
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
  , typeSelector
  , sizeRangeForDimensionSelector
  , enumeratedShapesSelector

  -- * Enum types
  , MLMultiArrayShapeConstraintType(MLMultiArrayShapeConstraintType)
  , pattern MLMultiArrayShapeConstraintTypeUnspecified
  , pattern MLMultiArrayShapeConstraintTypeEnumerated
  , pattern MLMultiArrayShapeConstraintTypeRange

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

-- | @- type@
type_ :: IsMLMultiArrayShapeConstraint mlMultiArrayShapeConstraint => mlMultiArrayShapeConstraint -> IO MLMultiArrayShapeConstraintType
type_ mlMultiArrayShapeConstraint  =
  fmap (coerce :: CLong -> MLMultiArrayShapeConstraintType) $ sendMsg mlMultiArrayShapeConstraint (mkSelector "type") retCLong []

-- | @- sizeRangeForDimension@
sizeRangeForDimension :: IsMLMultiArrayShapeConstraint mlMultiArrayShapeConstraint => mlMultiArrayShapeConstraint -> IO (Id NSArray)
sizeRangeForDimension mlMultiArrayShapeConstraint  =
  sendMsg mlMultiArrayShapeConstraint (mkSelector "sizeRangeForDimension") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enumeratedShapes@
enumeratedShapes :: IsMLMultiArrayShapeConstraint mlMultiArrayShapeConstraint => mlMultiArrayShapeConstraint -> IO (Id NSArray)
enumeratedShapes mlMultiArrayShapeConstraint  =
  sendMsg mlMultiArrayShapeConstraint (mkSelector "enumeratedShapes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @sizeRangeForDimension@
sizeRangeForDimensionSelector :: Selector
sizeRangeForDimensionSelector = mkSelector "sizeRangeForDimension"

-- | @Selector@ for @enumeratedShapes@
enumeratedShapesSelector :: Selector
enumeratedShapesSelector = mkSelector "enumeratedShapes"

