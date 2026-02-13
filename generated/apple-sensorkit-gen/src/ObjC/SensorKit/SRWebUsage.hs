{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRWebUsage@.
module ObjC.SensorKit.SRWebUsage
  ( SRWebUsage
  , IsSRWebUsage(..)
  , totalUsageTime
  , totalUsageTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- totalUsageTime@
totalUsageTime :: IsSRWebUsage srWebUsage => srWebUsage -> IO CDouble
totalUsageTime srWebUsage =
  sendMessage srWebUsage totalUsageTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @totalUsageTime@
totalUsageTimeSelector :: Selector '[] CDouble
totalUsageTimeSelector = mkSelector "totalUsageTime"

