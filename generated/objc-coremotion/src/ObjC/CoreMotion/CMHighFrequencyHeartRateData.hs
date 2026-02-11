{-# LANGUAGE PatternSynonyms #-}
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
  , heartRateSelector
  , confidenceSelector
  , dateSelector

  -- * Enum types
  , CMHighFrequencyHeartRateDataConfidence(CMHighFrequencyHeartRateDataConfidence)
  , pattern CMHighFrequencyHeartRateDataConfidenceLow
  , pattern CMHighFrequencyHeartRateDataConfidenceMedium
  , pattern CMHighFrequencyHeartRateDataConfidenceHigh
  , pattern CMHighFrequencyHeartRateDataConfidenceHighest

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

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- heartRate@
heartRate :: IsCMHighFrequencyHeartRateData cmHighFrequencyHeartRateData => cmHighFrequencyHeartRateData -> IO CDouble
heartRate cmHighFrequencyHeartRateData  =
  sendMsg cmHighFrequencyHeartRateData (mkSelector "heartRate") retCDouble []

-- | @- confidence@
confidence :: IsCMHighFrequencyHeartRateData cmHighFrequencyHeartRateData => cmHighFrequencyHeartRateData -> IO CMHighFrequencyHeartRateDataConfidence
confidence cmHighFrequencyHeartRateData  =
  fmap (coerce :: CLong -> CMHighFrequencyHeartRateDataConfidence) $ sendMsg cmHighFrequencyHeartRateData (mkSelector "confidence") retCLong []

-- | @- date@
date :: IsCMHighFrequencyHeartRateData cmHighFrequencyHeartRateData => cmHighFrequencyHeartRateData -> IO (Id NSDate)
date cmHighFrequencyHeartRateData  =
  sendMsg cmHighFrequencyHeartRateData (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @heartRate@
heartRateSelector :: Selector
heartRateSelector = mkSelector "heartRate"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

