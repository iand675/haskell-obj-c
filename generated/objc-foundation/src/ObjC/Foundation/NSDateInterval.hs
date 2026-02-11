{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCoderSelector
  , initWithStartDate_durationSelector
  , initWithStartDate_endDateSelector
  , compareSelector
  , isEqualToDateIntervalSelector
  , intersectsDateIntervalSelector
  , intersectionWithDateIntervalSelector
  , containsDateSelector
  , startDateSelector
  , endDateSelector
  , durationSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSDateInterval nsDateInterval => nsDateInterval -> IO (Id NSDateInterval)
init_ nsDateInterval  =
  sendMsg nsDateInterval (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSDateInterval nsDateInterval, IsNSCoder coder) => nsDateInterval -> coder -> IO (Id NSDateInterval)
initWithCoder nsDateInterval  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsDateInterval (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithStartDate:duration:@
initWithStartDate_duration :: (IsNSDateInterval nsDateInterval, IsNSDate startDate) => nsDateInterval -> startDate -> CDouble -> IO (Id NSDateInterval)
initWithStartDate_duration nsDateInterval  startDate duration =
withObjCPtr startDate $ \raw_startDate ->
    sendMsg nsDateInterval (mkSelector "initWithStartDate:duration:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argCDouble (fromIntegral duration)] >>= ownedObject . castPtr

-- | @- initWithStartDate:endDate:@
initWithStartDate_endDate :: (IsNSDateInterval nsDateInterval, IsNSDate startDate, IsNSDate endDate) => nsDateInterval -> startDate -> endDate -> IO (Id NSDateInterval)
initWithStartDate_endDate nsDateInterval  startDate endDate =
withObjCPtr startDate $ \raw_startDate ->
  withObjCPtr endDate $ \raw_endDate ->
      sendMsg nsDateInterval (mkSelector "initWithStartDate:endDate:") (retPtr retVoid) [argPtr (castPtr raw_startDate :: Ptr ()), argPtr (castPtr raw_endDate :: Ptr ())] >>= ownedObject . castPtr

-- | @- compare:@
compare_ :: (IsNSDateInterval nsDateInterval, IsNSDateInterval dateInterval) => nsDateInterval -> dateInterval -> IO NSComparisonResult
compare_ nsDateInterval  dateInterval =
withObjCPtr dateInterval $ \raw_dateInterval ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsDateInterval (mkSelector "compare:") retCLong [argPtr (castPtr raw_dateInterval :: Ptr ())]

-- | @- isEqualToDateInterval:@
isEqualToDateInterval :: (IsNSDateInterval nsDateInterval, IsNSDateInterval dateInterval) => nsDateInterval -> dateInterval -> IO Bool
isEqualToDateInterval nsDateInterval  dateInterval =
withObjCPtr dateInterval $ \raw_dateInterval ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateInterval (mkSelector "isEqualToDateInterval:") retCULong [argPtr (castPtr raw_dateInterval :: Ptr ())]

-- | @- intersectsDateInterval:@
intersectsDateInterval :: (IsNSDateInterval nsDateInterval, IsNSDateInterval dateInterval) => nsDateInterval -> dateInterval -> IO Bool
intersectsDateInterval nsDateInterval  dateInterval =
withObjCPtr dateInterval $ \raw_dateInterval ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateInterval (mkSelector "intersectsDateInterval:") retCULong [argPtr (castPtr raw_dateInterval :: Ptr ())]

-- | @- intersectionWithDateInterval:@
intersectionWithDateInterval :: (IsNSDateInterval nsDateInterval, IsNSDateInterval dateInterval) => nsDateInterval -> dateInterval -> IO (Id NSDateInterval)
intersectionWithDateInterval nsDateInterval  dateInterval =
withObjCPtr dateInterval $ \raw_dateInterval ->
    sendMsg nsDateInterval (mkSelector "intersectionWithDateInterval:") (retPtr retVoid) [argPtr (castPtr raw_dateInterval :: Ptr ())] >>= retainedObject . castPtr

-- | @- containsDate:@
containsDate :: (IsNSDateInterval nsDateInterval, IsNSDate date) => nsDateInterval -> date -> IO Bool
containsDate nsDateInterval  date =
withObjCPtr date $ \raw_date ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDateInterval (mkSelector "containsDate:") retCULong [argPtr (castPtr raw_date :: Ptr ())]

-- | @- startDate@
startDate :: IsNSDateInterval nsDateInterval => nsDateInterval -> IO (Id NSDate)
startDate nsDateInterval  =
  sendMsg nsDateInterval (mkSelector "startDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDate@
endDate :: IsNSDateInterval nsDateInterval => nsDateInterval -> IO (Id NSDate)
endDate nsDateInterval  =
  sendMsg nsDateInterval (mkSelector "endDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- duration@
duration :: IsNSDateInterval nsDateInterval => nsDateInterval -> IO CDouble
duration nsDateInterval  =
  sendMsg nsDateInterval (mkSelector "duration") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithStartDate:duration:@
initWithStartDate_durationSelector :: Selector
initWithStartDate_durationSelector = mkSelector "initWithStartDate:duration:"

-- | @Selector@ for @initWithStartDate:endDate:@
initWithStartDate_endDateSelector :: Selector
initWithStartDate_endDateSelector = mkSelector "initWithStartDate:endDate:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @isEqualToDateInterval:@
isEqualToDateIntervalSelector :: Selector
isEqualToDateIntervalSelector = mkSelector "isEqualToDateInterval:"

-- | @Selector@ for @intersectsDateInterval:@
intersectsDateIntervalSelector :: Selector
intersectsDateIntervalSelector = mkSelector "intersectsDateInterval:"

-- | @Selector@ for @intersectionWithDateInterval:@
intersectionWithDateIntervalSelector :: Selector
intersectionWithDateIntervalSelector = mkSelector "intersectionWithDateInterval:"

-- | @Selector@ for @containsDate:@
containsDateSelector :: Selector
containsDateSelector = mkSelector "containsDate:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

