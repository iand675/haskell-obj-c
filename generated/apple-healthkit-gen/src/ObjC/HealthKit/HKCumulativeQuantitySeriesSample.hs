{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKCumulativeQuantitySeriesSample@.
module ObjC.HealthKit.HKCumulativeQuantitySeriesSample
  ( HKCumulativeQuantitySeriesSample
  , IsHKCumulativeQuantitySeriesSample(..)
  , sum_
  , sumSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sum@
sum_ :: IsHKCumulativeQuantitySeriesSample hkCumulativeQuantitySeriesSample => hkCumulativeQuantitySeriesSample -> IO (Id HKQuantity)
sum_ hkCumulativeQuantitySeriesSample =
  sendMessage hkCumulativeQuantitySeriesSample sumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sum@
sumSelector :: Selector '[] (Id HKQuantity)
sumSelector = mkSelector "sum"

