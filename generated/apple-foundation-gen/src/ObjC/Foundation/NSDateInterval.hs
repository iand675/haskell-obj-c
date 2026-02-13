{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDateInterval@.
module ObjC.Foundation.NSDateInterval
  ( NSDateInterval
  , IsNSDateInterval(..)
  , init_
  , initWithCoder
  , initWithStartDate_duration
  , initWithStartDate_endDate
  , compare_
  , isEqualToDateInterval
  , intersectsDateInterval
  , intersectionWithDateInterval
  , containsDate
  , startDate
  , endDate
  , duration
  , compareSelector
  , containsDateSelector
  , durationSelector
  , endDateSelector
  , initSelector
  , initWithCoderSelector
  , initWithStartDate_durationSelector
  , initWithStartDate_endDateSelector
  , intersectionWithDateIntervalSelector
  , intersectsDateIntervalSelector
  , isEqualToDateIntervalSelector
  , startDateSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSDateInterval nsDateInterval => nsDateInterval -> IO (Id NSDateInterval)
init_ nsDateInterval =
  sendOwnedMessage nsDateInterval initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSDateInterval nsDateInterval, IsNSCoder coder) => nsDateInterval -> coder -> IO (Id NSDateInterval)
initWithCoder nsDateInterval coder =
  sendOwnedMessage nsDateInterval initWithCoderSelector (toNSCoder coder)

-- | @- initWithStartDate:duration:@
initWithStartDate_duration :: (IsNSDateInterval nsDateInterval, IsNSDate startDate) => nsDateInterval -> startDate -> CDouble -> IO (Id NSDateInterval)
initWithStartDate_duration nsDateInterval startDate duration =
  sendOwnedMessage nsDateInterval initWithStartDate_durationSelector (toNSDate startDate) duration

-- | @- initWithStartDate:endDate:@
initWithStartDate_endDate :: (IsNSDateInterval nsDateInterval, IsNSDate startDate, IsNSDate endDate) => nsDateInterval -> startDate -> endDate -> IO (Id NSDateInterval)
initWithStartDate_endDate nsDateInterval startDate endDate =
  sendOwnedMessage nsDateInterval initWithStartDate_endDateSelector (toNSDate startDate) (toNSDate endDate)

-- | @- compare:@
compare_ :: (IsNSDateInterval nsDateInterval, IsNSDateInterval dateInterval) => nsDateInterval -> dateInterval -> IO NSComparisonResult
compare_ nsDateInterval dateInterval =
  sendMessage nsDateInterval compareSelector (toNSDateInterval dateInterval)

-- | @- isEqualToDateInterval:@
isEqualToDateInterval :: (IsNSDateInterval nsDateInterval, IsNSDateInterval dateInterval) => nsDateInterval -> dateInterval -> IO Bool
isEqualToDateInterval nsDateInterval dateInterval =
  sendMessage nsDateInterval isEqualToDateIntervalSelector (toNSDateInterval dateInterval)

-- | @- intersectsDateInterval:@
intersectsDateInterval :: (IsNSDateInterval nsDateInterval, IsNSDateInterval dateInterval) => nsDateInterval -> dateInterval -> IO Bool
intersectsDateInterval nsDateInterval dateInterval =
  sendMessage nsDateInterval intersectsDateIntervalSelector (toNSDateInterval dateInterval)

-- | @- intersectionWithDateInterval:@
intersectionWithDateInterval :: (IsNSDateInterval nsDateInterval, IsNSDateInterval dateInterval) => nsDateInterval -> dateInterval -> IO (Id NSDateInterval)
intersectionWithDateInterval nsDateInterval dateInterval =
  sendMessage nsDateInterval intersectionWithDateIntervalSelector (toNSDateInterval dateInterval)

-- | @- containsDate:@
containsDate :: (IsNSDateInterval nsDateInterval, IsNSDate date) => nsDateInterval -> date -> IO Bool
containsDate nsDateInterval date =
  sendMessage nsDateInterval containsDateSelector (toNSDate date)

-- | @- startDate@
startDate :: IsNSDateInterval nsDateInterval => nsDateInterval -> IO (Id NSDate)
startDate nsDateInterval =
  sendMessage nsDateInterval startDateSelector

-- | @- endDate@
endDate :: IsNSDateInterval nsDateInterval => nsDateInterval -> IO (Id NSDate)
endDate nsDateInterval =
  sendMessage nsDateInterval endDateSelector

-- | @- duration@
duration :: IsNSDateInterval nsDateInterval => nsDateInterval -> IO CDouble
duration nsDateInterval =
  sendMessage nsDateInterval durationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDateInterval)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSDateInterval)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithStartDate:duration:@
initWithStartDate_durationSelector :: Selector '[Id NSDate, CDouble] (Id NSDateInterval)
initWithStartDate_durationSelector = mkSelector "initWithStartDate:duration:"

-- | @Selector@ for @initWithStartDate:endDate:@
initWithStartDate_endDateSelector :: Selector '[Id NSDate, Id NSDate] (Id NSDateInterval)
initWithStartDate_endDateSelector = mkSelector "initWithStartDate:endDate:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id NSDateInterval] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @isEqualToDateInterval:@
isEqualToDateIntervalSelector :: Selector '[Id NSDateInterval] Bool
isEqualToDateIntervalSelector = mkSelector "isEqualToDateInterval:"

-- | @Selector@ for @intersectsDateInterval:@
intersectsDateIntervalSelector :: Selector '[Id NSDateInterval] Bool
intersectsDateIntervalSelector = mkSelector "intersectsDateInterval:"

-- | @Selector@ for @intersectionWithDateInterval:@
intersectionWithDateIntervalSelector :: Selector '[Id NSDateInterval] (Id NSDateInterval)
intersectionWithDateIntervalSelector = mkSelector "intersectionWithDateInterval:"

-- | @Selector@ for @containsDate:@
containsDateSelector :: Selector '[Id NSDate] Bool
containsDateSelector = mkSelector "containsDate:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

