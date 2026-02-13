{-# LANGUAGE DataKinds #-}
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
  , countRangeSelector
  , valueDescriptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- valueDescription@
valueDescription :: IsMLSequenceConstraint mlSequenceConstraint => mlSequenceConstraint -> IO (Id MLFeatureDescription)
valueDescription mlSequenceConstraint =
  sendMessage mlSequenceConstraint valueDescriptionSelector

-- | @- countRange@
countRange :: IsMLSequenceConstraint mlSequenceConstraint => mlSequenceConstraint -> IO NSRange
countRange mlSequenceConstraint =
  sendMessage mlSequenceConstraint countRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueDescription@
valueDescriptionSelector :: Selector '[] (Id MLFeatureDescription)
valueDescriptionSelector = mkSelector "valueDescription"

-- | @Selector@ for @countRange@
countRangeSelector :: Selector '[] NSRange
countRangeSelector = mkSelector "countRange"

