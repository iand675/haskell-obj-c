{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKHeartbeatSeriesSample
--
-- An HKHeartbeatSeriesSample represents a series of heartbeats.
--
-- To retrieve the underlying series data for an HKHeartbeatSeriesSample, use HKHeartbeatSeriesQuery
--
-- Generated bindings for @HKHeartbeatSeriesSample@.
module ObjC.HealthKit.HKHeartbeatSeriesSample
  ( HKHeartbeatSeriesSample
  , IsHKHeartbeatSeriesSample(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

