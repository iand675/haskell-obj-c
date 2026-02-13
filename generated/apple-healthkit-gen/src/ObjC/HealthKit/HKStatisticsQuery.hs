{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKStatisticsQuery@.
module ObjC.HealthKit.HKStatisticsQuery
  ( HKStatisticsQuery
  , IsHKStatisticsQuery(..)
  , initWithQuantityType_quantitySamplePredicate_options_completionHandler
  , initWithQuantityType_quantitySamplePredicate_options_completionHandlerSelector

  -- * Enum types
  , HKStatisticsOptions(HKStatisticsOptions)
  , pattern HKStatisticsOptionNone
  , pattern HKStatisticsOptionSeparateBySource
  , pattern HKStatisticsOptionDiscreteAverage
  , pattern HKStatisticsOptionDiscreteMin
  , pattern HKStatisticsOptionDiscreteMax
  , pattern HKStatisticsOptionCumulativeSum
  , pattern HKStatisticsOptionMostRecent
  , pattern HKStatisticsOptionDiscreteMostRecent
  , pattern HKStatisticsOptionDuration

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithQuantityType:quantitySamplePredicate:options:completionHandler:@
initWithQuantityType_quantitySamplePredicate_options_completionHandler :: (IsHKStatisticsQuery hkStatisticsQuery, IsHKQuantityType quantityType, IsNSPredicate quantitySamplePredicate) => hkStatisticsQuery -> quantityType -> quantitySamplePredicate -> HKStatisticsOptions -> Ptr () -> IO (Id HKStatisticsQuery)
initWithQuantityType_quantitySamplePredicate_options_completionHandler hkStatisticsQuery quantityType quantitySamplePredicate options handler =
  sendOwnedMessage hkStatisticsQuery initWithQuantityType_quantitySamplePredicate_options_completionHandlerSelector (toHKQuantityType quantityType) (toNSPredicate quantitySamplePredicate) options handler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithQuantityType:quantitySamplePredicate:options:completionHandler:@
initWithQuantityType_quantitySamplePredicate_options_completionHandlerSelector :: Selector '[Id HKQuantityType, Id NSPredicate, HKStatisticsOptions, Ptr ()] (Id HKStatisticsQuery)
initWithQuantityType_quantitySamplePredicate_options_completionHandlerSelector = mkSelector "initWithQuantityType:quantitySamplePredicate:options:completionHandler:"

