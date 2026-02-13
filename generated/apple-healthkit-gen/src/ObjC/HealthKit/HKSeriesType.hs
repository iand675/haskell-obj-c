{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSeriesType
--
-- Represents a type of HKSeriesSample
--
-- Generated bindings for @HKSeriesType@.
module ObjC.HealthKit.HKSeriesType
  ( HKSeriesType
  , IsHKSeriesType(..)
  , workoutRouteType
  , heartbeatSeriesType
  , heartbeatSeriesTypeSelector
  , workoutRouteTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ workoutRouteType@
workoutRouteType :: IO (Id HKSeriesType)
workoutRouteType  =
  do
    cls' <- getRequiredClass "HKSeriesType"
    sendClassMessage cls' workoutRouteTypeSelector

-- | @+ heartbeatSeriesType@
heartbeatSeriesType :: IO (Id HKSeriesType)
heartbeatSeriesType  =
  do
    cls' <- getRequiredClass "HKSeriesType"
    sendClassMessage cls' heartbeatSeriesTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @workoutRouteType@
workoutRouteTypeSelector :: Selector '[] (Id HKSeriesType)
workoutRouteTypeSelector = mkSelector "workoutRouteType"

-- | @Selector@ for @heartbeatSeriesType@
heartbeatSeriesTypeSelector :: Selector '[] (Id HKSeriesType)
heartbeatSeriesTypeSelector = mkSelector "heartbeatSeriesType"

