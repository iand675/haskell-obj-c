{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Constraint describing expected MLSequence properties
--
-- Generated bindings for @MLSequenceConstraint@.
module ObjC.CoreML.MLSequenceConstraint
  ( MLSequenceConstraint
  , IsMLSequenceConstraint(..)
  , valueDescription
  , countRange
  , valueDescriptionSelector
  , countRangeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- valueDescription@
valueDescription :: IsMLSequenceConstraint mlSequenceConstraint => mlSequenceConstraint -> IO (Id MLFeatureDescription)
valueDescription mlSequenceConstraint  =
  sendMsg mlSequenceConstraint (mkSelector "valueDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- countRange@
countRange :: IsMLSequenceConstraint mlSequenceConstraint => mlSequenceConstraint -> IO NSRange
countRange mlSequenceConstraint  =
  sendMsgStret mlSequenceConstraint (mkSelector "countRange") retNSRange []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueDescription@
valueDescriptionSelector :: Selector
valueDescriptionSelector = mkSelector "valueDescription"

-- | @Selector@ for @countRange@
countRangeSelector :: Selector
countRangeSelector = mkSelector "countRange"

