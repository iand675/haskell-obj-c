{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKElectrocardiogram
--
-- An HKElectrocardiogram is a collection of voltage values as waveforms                from one or more leads
--
-- Generated bindings for @HKElectrocardiogram@.
module ObjC.HealthKit.HKElectrocardiogram
  ( HKElectrocardiogram
  , IsHKElectrocardiogram(..)
  , numberOfVoltageMeasurements
  , samplingFrequency
  , classification
  , averageHeartRate
  , symptomsStatus
  , averageHeartRateSelector
  , classificationSelector
  , numberOfVoltageMeasurementsSelector
  , samplingFrequencySelector
  , symptomsStatusSelector

  -- * Enum types
  , HKElectrocardiogramClassification(HKElectrocardiogramClassification)
  , pattern HKElectrocardiogramClassificationNotSet
  , pattern HKElectrocardiogramClassificationSinusRhythm
  , pattern HKElectrocardiogramClassificationAtrialFibrillation
  , pattern HKElectrocardiogramClassificationInconclusiveLowHeartRate
  , pattern HKElectrocardiogramClassificationInconclusiveHighHeartRate
  , pattern HKElectrocardiogramClassificationInconclusivePoorReading
  , pattern HKElectrocardiogramClassificationInconclusiveOther
  , pattern HKElectrocardiogramClassificationUnrecognized
  , HKElectrocardiogramSymptomsStatus(HKElectrocardiogramSymptomsStatus)
  , pattern HKElectrocardiogramSymptomsStatusNotSet
  , pattern HKElectrocardiogramSymptomsStatusNone
  , pattern HKElectrocardiogramSymptomsStatusPresent

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

-- | The number of voltage measurements in the electrocardiogram.
--
-- ObjC selector: @- numberOfVoltageMeasurements@
numberOfVoltageMeasurements :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO CLong
numberOfVoltageMeasurements hkElectrocardiogram =
  sendMessage hkElectrocardiogram numberOfVoltageMeasurementsSelector

-- | The frequency at which the data was sampled. This is reported in [HKUnit hertzUnit].
--
-- ObjC selector: @- samplingFrequency@
samplingFrequency :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO (Id HKQuantity)
samplingFrequency hkElectrocardiogram =
  sendMessage hkElectrocardiogram samplingFrequencySelector

-- | The classification of this electrocardiogram sample.
--
-- ObjC selector: @- classification@
classification :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO HKElectrocardiogramClassification
classification hkElectrocardiogram =
  sendMessage hkElectrocardiogram classificationSelector

-- | The average heart rate of the user while the electrocardiogram was recorded.
--
-- ObjC selector: @- averageHeartRate@
averageHeartRate :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO (Id HKQuantity)
averageHeartRate hkElectrocardiogram =
  sendMessage hkElectrocardiogram averageHeartRateSelector

-- | Whether the user experienced symptoms during this electrocardiogram.
--
-- ObjC selector: @- symptomsStatus@
symptomsStatus :: IsHKElectrocardiogram hkElectrocardiogram => hkElectrocardiogram -> IO HKElectrocardiogramSymptomsStatus
symptomsStatus hkElectrocardiogram =
  sendMessage hkElectrocardiogram symptomsStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @numberOfVoltageMeasurements@
numberOfVoltageMeasurementsSelector :: Selector '[] CLong
numberOfVoltageMeasurementsSelector = mkSelector "numberOfVoltageMeasurements"

-- | @Selector@ for @samplingFrequency@
samplingFrequencySelector :: Selector '[] (Id HKQuantity)
samplingFrequencySelector = mkSelector "samplingFrequency"

-- | @Selector@ for @classification@
classificationSelector :: Selector '[] HKElectrocardiogramClassification
classificationSelector = mkSelector "classification"

-- | @Selector@ for @averageHeartRate@
averageHeartRateSelector :: Selector '[] (Id HKQuantity)
averageHeartRateSelector = mkSelector "averageHeartRate"

-- | @Selector@ for @symptomsStatus@
symptomsStatusSelector :: Selector '[] HKElectrocardiogramSymptomsStatus
symptomsStatusSelector = mkSelector "symptomsStatus"

