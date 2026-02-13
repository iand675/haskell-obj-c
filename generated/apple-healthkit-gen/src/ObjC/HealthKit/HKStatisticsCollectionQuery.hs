{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKStatisticsCollectionQuery@.
module ObjC.HealthKit.HKStatisticsCollectionQuery
  ( HKStatisticsCollectionQuery
  , IsHKStatisticsCollectionQuery(..)
  , initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponents
  , anchorDate
  , options
  , intervalComponents
  , initialResultsHandler
  , setInitialResultsHandler
  , statisticsUpdateHandler
  , setStatisticsUpdateHandler
  , anchorDateSelector
  , initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponentsSelector
  , initialResultsHandlerSelector
  , intervalComponentsSelector
  , optionsSelector
  , setInitialResultsHandlerSelector
  , setStatisticsUpdateHandlerSelector
  , statisticsUpdateHandlerSelector

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

-- | @- initWithQuantityType:quantitySamplePredicate:options:anchorDate:intervalComponents:@
initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponents :: (IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery, IsHKQuantityType quantityType, IsNSPredicate quantitySamplePredicate, IsNSDate anchorDate, IsNSDateComponents intervalComponents) => hkStatisticsCollectionQuery -> quantityType -> quantitySamplePredicate -> HKStatisticsOptions -> anchorDate -> intervalComponents -> IO (Id HKStatisticsCollectionQuery)
initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponents hkStatisticsCollectionQuery quantityType quantitySamplePredicate options anchorDate intervalComponents =
  sendOwnedMessage hkStatisticsCollectionQuery initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponentsSelector (toHKQuantityType quantityType) (toNSPredicate quantitySamplePredicate) options (toNSDate anchorDate) (toNSDateComponents intervalComponents)

-- | @- anchorDate@
anchorDate :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO (Id NSDate)
anchorDate hkStatisticsCollectionQuery =
  sendMessage hkStatisticsCollectionQuery anchorDateSelector

-- | @- options@
options :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO HKStatisticsOptions
options hkStatisticsCollectionQuery =
  sendMessage hkStatisticsCollectionQuery optionsSelector

-- | @- intervalComponents@
intervalComponents :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO (Id NSDateComponents)
intervalComponents hkStatisticsCollectionQuery =
  sendMessage hkStatisticsCollectionQuery intervalComponentsSelector

-- | @- initialResultsHandler@
initialResultsHandler :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO (Ptr ())
initialResultsHandler hkStatisticsCollectionQuery =
  sendOwnedMessage hkStatisticsCollectionQuery initialResultsHandlerSelector

-- | @- setInitialResultsHandler:@
setInitialResultsHandler :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> Ptr () -> IO ()
setInitialResultsHandler hkStatisticsCollectionQuery value =
  sendMessage hkStatisticsCollectionQuery setInitialResultsHandlerSelector value

-- | @- statisticsUpdateHandler@
statisticsUpdateHandler :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> IO (Ptr ())
statisticsUpdateHandler hkStatisticsCollectionQuery =
  sendMessage hkStatisticsCollectionQuery statisticsUpdateHandlerSelector

-- | @- setStatisticsUpdateHandler:@
setStatisticsUpdateHandler :: IsHKStatisticsCollectionQuery hkStatisticsCollectionQuery => hkStatisticsCollectionQuery -> Ptr () -> IO ()
setStatisticsUpdateHandler hkStatisticsCollectionQuery value =
  sendMessage hkStatisticsCollectionQuery setStatisticsUpdateHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithQuantityType:quantitySamplePredicate:options:anchorDate:intervalComponents:@
initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponentsSelector :: Selector '[Id HKQuantityType, Id NSPredicate, HKStatisticsOptions, Id NSDate, Id NSDateComponents] (Id HKStatisticsCollectionQuery)
initWithQuantityType_quantitySamplePredicate_options_anchorDate_intervalComponentsSelector = mkSelector "initWithQuantityType:quantitySamplePredicate:options:anchorDate:intervalComponents:"

-- | @Selector@ for @anchorDate@
anchorDateSelector :: Selector '[] (Id NSDate)
anchorDateSelector = mkSelector "anchorDate"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] HKStatisticsOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @intervalComponents@
intervalComponentsSelector :: Selector '[] (Id NSDateComponents)
intervalComponentsSelector = mkSelector "intervalComponents"

-- | @Selector@ for @initialResultsHandler@
initialResultsHandlerSelector :: Selector '[] (Ptr ())
initialResultsHandlerSelector = mkSelector "initialResultsHandler"

-- | @Selector@ for @setInitialResultsHandler:@
setInitialResultsHandlerSelector :: Selector '[Ptr ()] ()
setInitialResultsHandlerSelector = mkSelector "setInitialResultsHandler:"

-- | @Selector@ for @statisticsUpdateHandler@
statisticsUpdateHandlerSelector :: Selector '[] (Ptr ())
statisticsUpdateHandlerSelector = mkSelector "statisticsUpdateHandler"

-- | @Selector@ for @setStatisticsUpdateHandler:@
setStatisticsUpdateHandlerSelector :: Selector '[Ptr ()] ()
setStatisticsUpdateHandlerSelector = mkSelector "setStatisticsUpdateHandler:"

