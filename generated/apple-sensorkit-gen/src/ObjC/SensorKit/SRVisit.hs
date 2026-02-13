{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRVisit@.
module ObjC.SensorKit.SRVisit
  ( SRVisit
  , IsSRVisit(..)
  , distanceFromHome
  , arrivalDateInterval
  , departureDateInterval
  , locationCategory
  , identifier
  , arrivalDateIntervalSelector
  , departureDateIntervalSelector
  , distanceFromHomeSelector
  , identifierSelector
  , locationCategorySelector

  -- * Enum types
  , SRLocationCategory(SRLocationCategory)
  , pattern SRLocationCategoryUnknown
  , pattern SRLocationCategoryHome
  , pattern SRLocationCategoryWork
  , pattern SRLocationCategorySchool
  , pattern SRLocationCategoryGym

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

-- | The distance between the location of interest to home
--
-- ObjC selector: @- distanceFromHome@
distanceFromHome :: IsSRVisit srVisit => srVisit -> IO CDouble
distanceFromHome srVisit =
  sendMessage srVisit distanceFromHomeSelector

-- | The range of time the arrival to a location of interest occurred
--
-- ObjC selector: @- arrivalDateInterval@
arrivalDateInterval :: IsSRVisit srVisit => srVisit -> IO (Id NSDateInterval)
arrivalDateInterval srVisit =
  sendMessage srVisit arrivalDateIntervalSelector

-- | The range of time the departure from a location of interest occurred
--
-- ObjC selector: @- departureDateInterval@
departureDateInterval :: IsSRVisit srVisit => srVisit -> IO (Id NSDateInterval)
departureDateInterval srVisit =
  sendMessage srVisit departureDateIntervalSelector

-- | @- locationCategory@
locationCategory :: IsSRVisit srVisit => srVisit -> IO SRLocationCategory
locationCategory srVisit =
  sendMessage srVisit locationCategorySelector

-- | An identifier for the location of interest. This can be used to identify the same location regardless of type
--
-- ObjC selector: @- identifier@
identifier :: IsSRVisit srVisit => srVisit -> IO (Id NSUUID)
identifier srVisit =
  sendMessage srVisit identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @distanceFromHome@
distanceFromHomeSelector :: Selector '[] CDouble
distanceFromHomeSelector = mkSelector "distanceFromHome"

-- | @Selector@ for @arrivalDateInterval@
arrivalDateIntervalSelector :: Selector '[] (Id NSDateInterval)
arrivalDateIntervalSelector = mkSelector "arrivalDateInterval"

-- | @Selector@ for @departureDateInterval@
departureDateIntervalSelector :: Selector '[] (Id NSDateInterval)
departureDateIntervalSelector = mkSelector "departureDateInterval"

-- | @Selector@ for @locationCategory@
locationCategorySelector :: Selector '[] SRLocationCategory
locationCategorySelector = mkSelector "locationCategory"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSUUID)
identifierSelector = mkSelector "identifier"

