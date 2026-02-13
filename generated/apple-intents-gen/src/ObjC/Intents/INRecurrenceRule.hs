{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRecurrenceRule@.
module ObjC.Intents.INRecurrenceRule
  ( INRecurrenceRule
  , IsINRecurrenceRule(..)
  , init_
  , initWithInterval_frequency
  , initWithInterval_frequency_weeklyRecurrenceDays
  , interval
  , frequency
  , weeklyRecurrenceDays
  , frequencySelector
  , initSelector
  , initWithInterval_frequencySelector
  , initWithInterval_frequency_weeklyRecurrenceDaysSelector
  , intervalSelector
  , weeklyRecurrenceDaysSelector

  -- * Enum types
  , INDayOfWeekOptions(INDayOfWeekOptions)
  , pattern INDayOfWeekOptionMonday
  , pattern INDayOfWeekOptionTuesday
  , pattern INDayOfWeekOptionWednesday
  , pattern INDayOfWeekOptionThursday
  , pattern INDayOfWeekOptionFriday
  , pattern INDayOfWeekOptionSaturday
  , pattern INDayOfWeekOptionSunday
  , INRecurrenceFrequency(INRecurrenceFrequency)
  , pattern INRecurrenceFrequencyUnknown
  , pattern INRecurrenceFrequencyMinute
  , pattern INRecurrenceFrequencyHourly
  , pattern INRecurrenceFrequencyDaily
  , pattern INRecurrenceFrequencyWeekly
  , pattern INRecurrenceFrequencyMonthly
  , pattern INRecurrenceFrequencyYearly

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> IO (Id INRecurrenceRule)
init_ inRecurrenceRule =
  sendOwnedMessage inRecurrenceRule initSelector

-- | @- initWithInterval:frequency:@
initWithInterval_frequency :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> CULong -> INRecurrenceFrequency -> IO (Id INRecurrenceRule)
initWithInterval_frequency inRecurrenceRule interval frequency =
  sendOwnedMessage inRecurrenceRule initWithInterval_frequencySelector interval frequency

-- | @- initWithInterval:frequency:weeklyRecurrenceDays:@
initWithInterval_frequency_weeklyRecurrenceDays :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> CULong -> INRecurrenceFrequency -> INDayOfWeekOptions -> IO (Id INRecurrenceRule)
initWithInterval_frequency_weeklyRecurrenceDays inRecurrenceRule interval frequency weeklyRecurrenceDays =
  sendOwnedMessage inRecurrenceRule initWithInterval_frequency_weeklyRecurrenceDaysSelector interval frequency weeklyRecurrenceDays

-- | @- interval@
interval :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> IO CULong
interval inRecurrenceRule =
  sendMessage inRecurrenceRule intervalSelector

-- | @- frequency@
frequency :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> IO INRecurrenceFrequency
frequency inRecurrenceRule =
  sendMessage inRecurrenceRule frequencySelector

-- | @- weeklyRecurrenceDays@
weeklyRecurrenceDays :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> IO INDayOfWeekOptions
weeklyRecurrenceDays inRecurrenceRule =
  sendMessage inRecurrenceRule weeklyRecurrenceDaysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INRecurrenceRule)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterval:frequency:@
initWithInterval_frequencySelector :: Selector '[CULong, INRecurrenceFrequency] (Id INRecurrenceRule)
initWithInterval_frequencySelector = mkSelector "initWithInterval:frequency:"

-- | @Selector@ for @initWithInterval:frequency:weeklyRecurrenceDays:@
initWithInterval_frequency_weeklyRecurrenceDaysSelector :: Selector '[CULong, INRecurrenceFrequency, INDayOfWeekOptions] (Id INRecurrenceRule)
initWithInterval_frequency_weeklyRecurrenceDaysSelector = mkSelector "initWithInterval:frequency:weeklyRecurrenceDays:"

-- | @Selector@ for @interval@
intervalSelector :: Selector '[] CULong
intervalSelector = mkSelector "interval"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] INRecurrenceFrequency
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @weeklyRecurrenceDays@
weeklyRecurrenceDaysSelector :: Selector '[] INDayOfWeekOptions
weeklyRecurrenceDaysSelector = mkSelector "weeklyRecurrenceDays"

