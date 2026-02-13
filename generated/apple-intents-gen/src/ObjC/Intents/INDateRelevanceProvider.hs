{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A relevance provider to indicate relevance at a date or date interval.
--
-- Generated bindings for @INDateRelevanceProvider@.
module ObjC.Intents.INDateRelevanceProvider
  ( INDateRelevanceProvider
  , IsINDateRelevanceProvider(..)
  , initWithStartDate_endDate
  , startDate
  , endDate
  , endDateSelector
  , initWithStartDate_endDateSelector
  , startDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a date relevance provider with the specified relevant date interval.
--
-- ObjC selector: @- initWithStartDate:endDate:@
initWithStartDate_endDate :: (IsINDateRelevanceProvider inDateRelevanceProvider, IsNSDate startDate, IsNSDate endDate) => inDateRelevanceProvider -> startDate -> endDate -> IO (Id INDateRelevanceProvider)
initWithStartDate_endDate inDateRelevanceProvider startDate endDate =
  sendOwnedMessage inDateRelevanceProvider initWithStartDate_endDateSelector (toNSDate startDate) (toNSDate endDate)

-- | The start date of the relevant time interval.
--
-- ObjC selector: @- startDate@
startDate :: IsINDateRelevanceProvider inDateRelevanceProvider => inDateRelevanceProvider -> IO (Id NSDate)
startDate inDateRelevanceProvider =
  sendMessage inDateRelevanceProvider startDateSelector

-- | The end date of the relevant time interval.
--
-- Note: If @endDate@ is @nil,@ the relevant time interval will be assumed to represent a single point in time instead of a time interval.
--
-- ObjC selector: @- endDate@
endDate :: IsINDateRelevanceProvider inDateRelevanceProvider => inDateRelevanceProvider -> IO (Id NSDate)
endDate inDateRelevanceProvider =
  sendMessage inDateRelevanceProvider endDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStartDate:endDate:@
initWithStartDate_endDateSelector :: Selector '[Id NSDate, Id NSDate] (Id INDateRelevanceProvider)
initWithStartDate_endDateSelector = mkSelector "initWithStartDate:endDate:"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

