{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMHighFrequencyHeartRateData@.
module ObjC.CoreMotion.CMHighFrequencyHeartRateData
  ( CMHighFrequencyHeartRateData
  , IsCMHighFrequencyHeartRateData(..)
  , heartRate
  , confidence
  , date
  , confidenceSelector
  , dateSelector
  , heartRateSelector

  -- * Enum types
  , CMHighFrequencyHeartRateDataConfidence(CMHighFrequencyHeartRateDataConfidence)
  , pattern CMHighFrequencyHeartRateDataConfidenceLow
  , pattern CMHighFrequencyHeartRateDataConfidenceMedium
  , pattern CMHighFrequencyHeartRateDataConfidenceHigh
  , pattern CMHighFrequencyHeartRateDataConfidenceHighest

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- heartRate@
heartRate :: IsCMHighFrequencyHeartRateData cmHighFrequencyHeartRateData => cmHighFrequencyHeartRateData -> IO CDouble
heartRate cmHighFrequencyHeartRateData =
  sendMessage cmHighFrequencyHeartRateData heartRateSelector

-- | @- confidence@
confidence :: IsCMHighFrequencyHeartRateData cmHighFrequencyHeartRateData => cmHighFrequencyHeartRateData -> IO CMHighFrequencyHeartRateDataConfidence
confidence cmHighFrequencyHeartRateData =
  sendMessage cmHighFrequencyHeartRateData confidenceSelector

-- | @- date@
date :: IsCMHighFrequencyHeartRateData cmHighFrequencyHeartRateData => cmHighFrequencyHeartRateData -> IO (Id NSDate)
date cmHighFrequencyHeartRateData =
  sendMessage cmHighFrequencyHeartRateData dateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @heartRate@
heartRateSelector :: Selector '[] CDouble
heartRateSelector = mkSelector "heartRate"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector '[] CMHighFrequencyHeartRateDataConfidence
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

