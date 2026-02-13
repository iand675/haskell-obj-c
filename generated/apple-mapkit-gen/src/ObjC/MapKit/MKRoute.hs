{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKRoute@.
module ObjC.MapKit.MKRoute
  ( MKRoute
  , IsMKRoute(..)
  , name
  , advisoryNotices
  , distance
  , expectedTravelTime
  , transportType
  , polyline
  , steps
  , hasTolls
  , hasHighways
  , advisoryNoticesSelector
  , distanceSelector
  , expectedTravelTimeSelector
  , hasHighwaysSelector
  , hasTollsSelector
  , nameSelector
  , polylineSelector
  , stepsSelector
  , transportTypeSelector

  -- * Enum types
  , MKDirectionsTransportType(MKDirectionsTransportType)
  , pattern MKDirectionsTransportTypeAutomobile
  , pattern MKDirectionsTransportTypeWalking
  , pattern MKDirectionsTransportTypeTransit
  , pattern MKDirectionsTransportTypeCycling
  , pattern MKDirectionsTransportTypeAny

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMKRoute mkRoute => mkRoute -> IO (Id NSString)
name mkRoute =
  sendMessage mkRoute nameSelector

-- | @- advisoryNotices@
advisoryNotices :: IsMKRoute mkRoute => mkRoute -> IO (Id NSArray)
advisoryNotices mkRoute =
  sendMessage mkRoute advisoryNoticesSelector

-- | @- distance@
distance :: IsMKRoute mkRoute => mkRoute -> IO CDouble
distance mkRoute =
  sendMessage mkRoute distanceSelector

-- | @- expectedTravelTime@
expectedTravelTime :: IsMKRoute mkRoute => mkRoute -> IO CDouble
expectedTravelTime mkRoute =
  sendMessage mkRoute expectedTravelTimeSelector

-- | @- transportType@
transportType :: IsMKRoute mkRoute => mkRoute -> IO MKDirectionsTransportType
transportType mkRoute =
  sendMessage mkRoute transportTypeSelector

-- | @- polyline@
polyline :: IsMKRoute mkRoute => mkRoute -> IO (Id MKPolyline)
polyline mkRoute =
  sendMessage mkRoute polylineSelector

-- | @- steps@
steps :: IsMKRoute mkRoute => mkRoute -> IO (Id NSArray)
steps mkRoute =
  sendMessage mkRoute stepsSelector

-- | @- hasTolls@
hasTolls :: IsMKRoute mkRoute => mkRoute -> IO Bool
hasTolls mkRoute =
  sendMessage mkRoute hasTollsSelector

-- | @- hasHighways@
hasHighways :: IsMKRoute mkRoute => mkRoute -> IO Bool
hasHighways mkRoute =
  sendMessage mkRoute hasHighwaysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @advisoryNotices@
advisoryNoticesSelector :: Selector '[] (Id NSArray)
advisoryNoticesSelector = mkSelector "advisoryNotices"

-- | @Selector@ for @distance@
distanceSelector :: Selector '[] CDouble
distanceSelector = mkSelector "distance"

-- | @Selector@ for @expectedTravelTime@
expectedTravelTimeSelector :: Selector '[] CDouble
expectedTravelTimeSelector = mkSelector "expectedTravelTime"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector '[] MKDirectionsTransportType
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @polyline@
polylineSelector :: Selector '[] (Id MKPolyline)
polylineSelector = mkSelector "polyline"

-- | @Selector@ for @steps@
stepsSelector :: Selector '[] (Id NSArray)
stepsSelector = mkSelector "steps"

-- | @Selector@ for @hasTolls@
hasTollsSelector :: Selector '[] Bool
hasTollsSelector = mkSelector "hasTolls"

-- | @Selector@ for @hasHighways@
hasHighwaysSelector :: Selector '[] Bool
hasHighwaysSelector = mkSelector "hasHighways"

