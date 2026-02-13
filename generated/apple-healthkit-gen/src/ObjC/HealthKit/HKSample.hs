{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSample
--
-- An abstract class representing measurements taken over a period of time.
--
-- Generated bindings for @HKSample@.
module ObjC.HealthKit.HKSample
  ( HKSample
  , IsHKSample(..)
  , sampleType
  , startDate
  , endDate
  , hasUndeterminedDuration
  , endDateSelector
  , hasUndeterminedDurationSelector
  , sampleTypeSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sampleType@
sampleType :: IsHKSample hkSample => hkSample -> IO (Id HKSampleType)
sampleType hkSample =
  sendMessage hkSample sampleTypeSelector

-- | @- startDate@
startDate :: IsHKSample hkSample => hkSample -> IO (Id NSDate)
startDate hkSample =
  sendMessage hkSample startDateSelector

-- | @- endDate@
endDate :: IsHKSample hkSample => hkSample -> IO (Id NSDate)
endDate hkSample =
  sendMessage hkSample endDateSelector

-- | hasUndeterminedDuration
--
-- Indicates whether a sample has an undetermined duration.
--
-- Computed based on the endDate of a sample.
--
-- ObjC selector: @- hasUndeterminedDuration@
hasUndeterminedDuration :: IsHKSample hkSample => hkSample -> IO Bool
hasUndeterminedDuration hkSample =
  sendMessage hkSample hasUndeterminedDurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sampleType@
sampleTypeSelector :: Selector '[] (Id HKSampleType)
sampleTypeSelector = mkSelector "sampleType"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @hasUndeterminedDuration@
hasUndeterminedDurationSelector :: Selector '[] Bool
hasUndeterminedDurationSelector = mkSelector "hasUndeterminedDuration"

