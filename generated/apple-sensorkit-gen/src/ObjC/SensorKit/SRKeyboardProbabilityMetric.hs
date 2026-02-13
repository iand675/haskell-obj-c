{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRKeyboardProbabilityMetric@.
module ObjC.SensorKit.SRKeyboardProbabilityMetric
  ( SRKeyboardProbabilityMetric
  , IsSRKeyboardProbabilityMetric(..)
  , distributionSampleValues
  , distributionSampleValuesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Sample values from probability distribution
--
-- ObjC selector: @- distributionSampleValues@
distributionSampleValues :: IsSRKeyboardProbabilityMetric srKeyboardProbabilityMetric => srKeyboardProbabilityMetric -> IO (Id NSArray)
distributionSampleValues srKeyboardProbabilityMetric =
  sendMessage srKeyboardProbabilityMetric distributionSampleValuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @distributionSampleValues@
distributionSampleValuesSelector :: Selector '[] (Id NSArray)
distributionSampleValuesSelector = mkSelector "distributionSampleValues"

