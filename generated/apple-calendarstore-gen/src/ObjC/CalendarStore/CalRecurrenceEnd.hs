{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalRecurrenceEnd@.
module ObjC.CalendarStore.CalRecurrenceEnd
  ( CalRecurrenceEnd
  , IsCalRecurrenceEnd(..)
  , recurrenceEndWithEndDate
  , recurrenceEndWithOccurrenceCount
  , usesEndDate
  , endDate
  , occurrenceCount
  , endDateSelector
  , occurrenceCountSelector
  , recurrenceEndWithEndDateSelector
  , recurrenceEndWithOccurrenceCountSelector
  , usesEndDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ recurrenceEndWithEndDate:@
recurrenceEndWithEndDate :: IsNSDate endDate => endDate -> IO RawId
recurrenceEndWithEndDate endDate =
  do
    cls' <- getRequiredClass "CalRecurrenceEnd"
    sendClassMessage cls' recurrenceEndWithEndDateSelector (toNSDate endDate)

-- | @+ recurrenceEndWithOccurrenceCount:@
recurrenceEndWithOccurrenceCount :: CULong -> IO RawId
recurrenceEndWithOccurrenceCount occurrenceCount =
  do
    cls' <- getRequiredClass "CalRecurrenceEnd"
    sendClassMessage cls' recurrenceEndWithOccurrenceCountSelector occurrenceCount

-- | @- usesEndDate@
usesEndDate :: IsCalRecurrenceEnd calRecurrenceEnd => calRecurrenceEnd -> IO Bool
usesEndDate calRecurrenceEnd =
  sendMessage calRecurrenceEnd usesEndDateSelector

-- | @- endDate@
endDate :: IsCalRecurrenceEnd calRecurrenceEnd => calRecurrenceEnd -> IO (Id NSDate)
endDate calRecurrenceEnd =
  sendMessage calRecurrenceEnd endDateSelector

-- | @- occurrenceCount@
occurrenceCount :: IsCalRecurrenceEnd calRecurrenceEnd => calRecurrenceEnd -> IO CULong
occurrenceCount calRecurrenceEnd =
  sendMessage calRecurrenceEnd occurrenceCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recurrenceEndWithEndDate:@
recurrenceEndWithEndDateSelector :: Selector '[Id NSDate] RawId
recurrenceEndWithEndDateSelector = mkSelector "recurrenceEndWithEndDate:"

-- | @Selector@ for @recurrenceEndWithOccurrenceCount:@
recurrenceEndWithOccurrenceCountSelector :: Selector '[CULong] RawId
recurrenceEndWithOccurrenceCountSelector = mkSelector "recurrenceEndWithOccurrenceCount:"

-- | @Selector@ for @usesEndDate@
usesEndDateSelector :: Selector '[] Bool
usesEndDateSelector = mkSelector "usesEndDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @occurrenceCount@
occurrenceCountSelector :: Selector '[] CULong
occurrenceCountSelector = mkSelector "occurrenceCount"

