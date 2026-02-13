{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLImageSizeConstraint@.
module ObjC.CoreML.MLImageSizeConstraint
  ( MLImageSizeConstraint
  , IsMLImageSizeConstraint(..)
  , type_
  , pixelsWideRange
  , pixelsHighRange
  , enumeratedImageSizes
  , enumeratedImageSizesSelector
  , pixelsHighRangeSelector
  , pixelsWideRangeSelector
  , typeSelector

  -- * Enum types
  , MLImageSizeConstraintType(MLImageSizeConstraintType)
  , pattern MLImageSizeConstraintTypeUnspecified
  , pattern MLImageSizeConstraintTypeEnumerated
  , pattern MLImageSizeConstraintTypeRange

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsMLImageSizeConstraint mlImageSizeConstraint => mlImageSizeConstraint -> IO MLImageSizeConstraintType
type_ mlImageSizeConstraint =
  sendMessage mlImageSizeConstraint typeSelector

-- | @- pixelsWideRange@
pixelsWideRange :: IsMLImageSizeConstraint mlImageSizeConstraint => mlImageSizeConstraint -> IO NSRange
pixelsWideRange mlImageSizeConstraint =
  sendMessage mlImageSizeConstraint pixelsWideRangeSelector

-- | @- pixelsHighRange@
pixelsHighRange :: IsMLImageSizeConstraint mlImageSizeConstraint => mlImageSizeConstraint -> IO NSRange
pixelsHighRange mlImageSizeConstraint =
  sendMessage mlImageSizeConstraint pixelsHighRangeSelector

-- | @- enumeratedImageSizes@
enumeratedImageSizes :: IsMLImageSizeConstraint mlImageSizeConstraint => mlImageSizeConstraint -> IO (Id NSArray)
enumeratedImageSizes mlImageSizeConstraint =
  sendMessage mlImageSizeConstraint enumeratedImageSizesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] MLImageSizeConstraintType
typeSelector = mkSelector "type"

-- | @Selector@ for @pixelsWideRange@
pixelsWideRangeSelector :: Selector '[] NSRange
pixelsWideRangeSelector = mkSelector "pixelsWideRange"

-- | @Selector@ for @pixelsHighRange@
pixelsHighRangeSelector :: Selector '[] NSRange
pixelsHighRangeSelector = mkSelector "pixelsHighRange"

-- | @Selector@ for @enumeratedImageSizes@
enumeratedImageSizesSelector :: Selector '[] (Id NSArray)
enumeratedImageSizesSelector = mkSelector "enumeratedImageSizes"

