{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CNBoundsPrediction@.
module ObjC.Cinematic.CNBoundsPrediction
  ( CNBoundsPrediction
  , IsCNBoundsPrediction(..)
  , confidence
  , setConfidence
  , confidenceSelector
  , setConfidenceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | the probability that a well-defined object is within the bounds — a number between 0.0 and 1.0.
--
-- ObjC selector: @- confidence@
confidence :: IsCNBoundsPrediction cnBoundsPrediction => cnBoundsPrediction -> IO CFloat
confidence cnBoundsPrediction =
  sendMessage cnBoundsPrediction confidenceSelector

-- | the probability that a well-defined object is within the bounds — a number between 0.0 and 1.0.
--
-- ObjC selector: @- setConfidence:@
setConfidence :: IsCNBoundsPrediction cnBoundsPrediction => cnBoundsPrediction -> CFloat -> IO ()
setConfidence cnBoundsPrediction value =
  sendMessage cnBoundsPrediction setConfidenceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @confidence@
confidenceSelector :: Selector '[] CFloat
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @setConfidence:@
setConfidenceSelector :: Selector '[CFloat] ()
setConfidenceSelector = mkSelector "setConfidence:"

