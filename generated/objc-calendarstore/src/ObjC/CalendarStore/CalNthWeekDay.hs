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

import ObjC.CalendarStore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dayOfTheWeek@
dayOfTheWeek :: IsCalNthWeekDay calNthWeekDay => calNthWeekDay -> IO CULong
dayOfTheWeek calNthWeekDay  =
  sendMsg calNthWeekDay (mkSelector "dayOfTheWeek") retCULong []

-- | @- weekNumber@
weekNumber :: IsCalNthWeekDay calNthWeekDay => calNthWeekDay -> IO CLong
weekNumber calNthWeekDay  =
  sendMsg calNthWeekDay (mkSelector "weekNumber") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayOfTheWeek@
dayOfTheWeekSelector :: Selector
dayOfTheWeekSelector = mkSelector "dayOfTheWeek"

-- | @Selector@ for @weekNumber@
weekNumberSelector :: Selector
weekNumberSelector = mkSelector "weekNumber"

