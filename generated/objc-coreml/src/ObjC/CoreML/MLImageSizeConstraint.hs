{-# LANGUAGE PatternSynonyms #-}
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
  , typeSelector
  , pixelsWideRangeSelector
  , pixelsHighRangeSelector
  , enumeratedImageSizesSelector

  -- * Enum types
  , MLImageSizeConstraintType(MLImageSizeConstraintType)
  , pattern MLImageSizeConstraintTypeUnspecified
  , pattern MLImageSizeConstraintTypeEnumerated
  , pattern MLImageSizeConstraintTypeRange

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsMLImageSizeConstraint mlImageSizeConstraint => mlImageSizeConstraint -> IO MLImageSizeConstraintType
type_ mlImageSizeConstraint  =
  fmap (coerce :: CLong -> MLImageSizeConstraintType) $ sendMsg mlImageSizeConstraint (mkSelector "type") retCLong []

-- | @- pixelsWideRange@
pixelsWideRange :: IsMLImageSizeConstraint mlImageSizeConstraint => mlImageSizeConstraint -> IO NSRange
pixelsWideRange mlImageSizeConstraint  =
  sendMsgStret mlImageSizeConstraint (mkSelector "pixelsWideRange") retNSRange []

-- | @- pixelsHighRange@
pixelsHighRange :: IsMLImageSizeConstraint mlImageSizeConstraint => mlImageSizeConstraint -> IO NSRange
pixelsHighRange mlImageSizeConstraint  =
  sendMsgStret mlImageSizeConstraint (mkSelector "pixelsHighRange") retNSRange []

-- | @- enumeratedImageSizes@
enumeratedImageSizes :: IsMLImageSizeConstraint mlImageSizeConstraint => mlImageSizeConstraint -> IO (Id NSArray)
enumeratedImageSizes mlImageSizeConstraint  =
  sendMsg mlImageSizeConstraint (mkSelector "enumeratedImageSizes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @pixelsWideRange@
pixelsWideRangeSelector :: Selector
pixelsWideRangeSelector = mkSelector "pixelsWideRange"

-- | @Selector@ for @pixelsHighRange@
pixelsHighRangeSelector :: Selector
pixelsHighRangeSelector = mkSelector "pixelsHighRange"

-- | @Selector@ for @enumeratedImageSizes@
enumeratedImageSizesSelector :: Selector
enumeratedImageSizesSelector = mkSelector "enumeratedImageSizes"

