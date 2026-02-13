{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRDeletionRecord@.
module ObjC.SensorKit.SRDeletionRecord
  ( SRDeletionRecord
  , IsSRDeletionRecord(..)
  , startTime
  , endTime
  , reason
  , endTimeSelector
  , reasonSelector
  , startTimeSelector

  -- * Enum types
  , SRDeletionReason(SRDeletionReason)
  , pattern SRDeletionReasonUserInitiated
  , pattern SRDeletionReasonLowDiskSpace
  , pattern SRDeletionReasonAgeLimit
  , pattern SRDeletionReasonNoInterestedClients
  , pattern SRDeletionReasonSystemInitiated

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- startTime@
startTime :: IsSRDeletionRecord srDeletionRecord => srDeletionRecord -> IO CDouble
startTime srDeletionRecord =
  sendMessage srDeletionRecord startTimeSelector

-- | @- endTime@
endTime :: IsSRDeletionRecord srDeletionRecord => srDeletionRecord -> IO CDouble
endTime srDeletionRecord =
  sendMessage srDeletionRecord endTimeSelector

-- | @- reason@
reason :: IsSRDeletionRecord srDeletionRecord => srDeletionRecord -> IO SRDeletionReason
reason srDeletionRecord =
  sendMessage srDeletionRecord reasonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] CDouble
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector '[] CDouble
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] SRDeletionReason
reasonSelector = mkSelector "reason"

