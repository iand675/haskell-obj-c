{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CalNthWeekDay@.
module ObjC.CalendarStore.CalNthWeekDay
  ( CalNthWeekDay
  , IsCalNthWeekDay(..)
  , dayOfTheWeek
  , weekNumber
  , dayOfTheWeekSelector
  , weekNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dayOfTheWeek@
dayOfTheWeek :: IsCalNthWeekDay calNthWeekDay => calNthWeekDay -> IO CULong
dayOfTheWeek calNthWeekDay =
  sendMessage calNthWeekDay dayOfTheWeekSelector

-- | @- weekNumber@
weekNumber :: IsCalNthWeekDay calNthWeekDay => calNthWeekDay -> IO CLong
weekNumber calNthWeekDay =
  sendMessage calNthWeekDay weekNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayOfTheWeek@
dayOfTheWeekSelector :: Selector '[] CULong
dayOfTheWeekSelector = mkSelector "dayOfTheWeek"

-- | @Selector@ for @weekNumber@
weekNumberSelector :: Selector '[] CLong
weekNumberSelector = mkSelector "weekNumber"

