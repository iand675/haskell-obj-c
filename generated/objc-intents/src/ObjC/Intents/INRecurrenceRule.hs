{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithInterval_frequencySelector
  , initWithInterval_frequency_weeklyRecurrenceDaysSelector
  , intervalSelector
  , frequencySelector
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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> IO (Id INRecurrenceRule)
init_ inRecurrenceRule  =
  sendMsg inRecurrenceRule (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithInterval:frequency:@
initWithInterval_frequency :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> CULong -> INRecurrenceFrequency -> IO (Id INRecurrenceRule)
initWithInterval_frequency inRecurrenceRule  interval frequency =
  sendMsg inRecurrenceRule (mkSelector "initWithInterval:frequency:") (retPtr retVoid) [argCULong (fromIntegral interval), argCLong (coerce frequency)] >>= ownedObject . castPtr

-- | @- initWithInterval:frequency:weeklyRecurrenceDays:@
initWithInterval_frequency_weeklyRecurrenceDays :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> CULong -> INRecurrenceFrequency -> INDayOfWeekOptions -> IO (Id INRecurrenceRule)
initWithInterval_frequency_weeklyRecurrenceDays inRecurrenceRule  interval frequency weeklyRecurrenceDays =
  sendMsg inRecurrenceRule (mkSelector "initWithInterval:frequency:weeklyRecurrenceDays:") (retPtr retVoid) [argCULong (fromIntegral interval), argCLong (coerce frequency), argCULong (coerce weeklyRecurrenceDays)] >>= ownedObject . castPtr

-- | @- interval@
interval :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> IO CULong
interval inRecurrenceRule  =
  sendMsg inRecurrenceRule (mkSelector "interval") retCULong []

-- | @- frequency@
frequency :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> IO INRecurrenceFrequency
frequency inRecurrenceRule  =
  fmap (coerce :: CLong -> INRecurrenceFrequency) $ sendMsg inRecurrenceRule (mkSelector "frequency") retCLong []

-- | @- weeklyRecurrenceDays@
weeklyRecurrenceDays :: IsINRecurrenceRule inRecurrenceRule => inRecurrenceRule -> IO INDayOfWeekOptions
weeklyRecurrenceDays inRecurrenceRule  =
  fmap (coerce :: CULong -> INDayOfWeekOptions) $ sendMsg inRecurrenceRule (mkSelector "weeklyRecurrenceDays") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterval:frequency:@
initWithInterval_frequencySelector :: Selector
initWithInterval_frequencySelector = mkSelector "initWithInterval:frequency:"

-- | @Selector@ for @initWithInterval:frequency:weeklyRecurrenceDays:@
initWithInterval_frequency_weeklyRecurrenceDaysSelector :: Selector
initWithInterval_frequency_weeklyRecurrenceDaysSelector = mkSelector "initWithInterval:frequency:weeklyRecurrenceDays:"

-- | @Selector@ for @interval@
intervalSelector :: Selector
intervalSelector = mkSelector "interval"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @weeklyRecurrenceDays@
weeklyRecurrenceDaysSelector :: Selector
weeklyRecurrenceDaysSelector = mkSelector "weeklyRecurrenceDays"

