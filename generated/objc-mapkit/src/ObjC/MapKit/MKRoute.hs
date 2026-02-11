{-# LANGUAGE PatternSynonyms #-}
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
  , nameSelector
  , advisoryNoticesSelector
  , distanceSelector
  , expectedTravelTimeSelector
  , transportTypeSelector
  , polylineSelector
  , stepsSelector
  , hasTollsSelector
  , hasHighwaysSelector

  -- * Enum types
  , MKDirectionsTransportType(MKDirectionsTransportType)
  , pattern MKDirectionsTransportTypeAutomobile
  , pattern MKDirectionsTransportTypeWalking
  , pattern MKDirectionsTransportTypeTransit
  , pattern MKDirectionsTransportTypeCycling
  , pattern MKDirectionsTransportTypeAny

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMKRoute mkRoute => mkRoute -> IO (Id NSString)
name mkRoute  =
  sendMsg mkRoute (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- advisoryNotices@
advisoryNotices :: IsMKRoute mkRoute => mkRoute -> IO (Id NSArray)
advisoryNotices mkRoute  =
  sendMsg mkRoute (mkSelector "advisoryNotices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- distance@
distance :: IsMKRoute mkRoute => mkRoute -> IO CDouble
distance mkRoute  =
  sendMsg mkRoute (mkSelector "distance") retCDouble []

-- | @- expectedTravelTime@
expectedTravelTime :: IsMKRoute mkRoute => mkRoute -> IO CDouble
expectedTravelTime mkRoute  =
  sendMsg mkRoute (mkSelector "expectedTravelTime") retCDouble []

-- | @- transportType@
transportType :: IsMKRoute mkRoute => mkRoute -> IO MKDirectionsTransportType
transportType mkRoute  =
  fmap (coerce :: CULong -> MKDirectionsTransportType) $ sendMsg mkRoute (mkSelector "transportType") retCULong []

-- | @- polyline@
polyline :: IsMKRoute mkRoute => mkRoute -> IO (Id MKPolyline)
polyline mkRoute  =
  sendMsg mkRoute (mkSelector "polyline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- steps@
steps :: IsMKRoute mkRoute => mkRoute -> IO (Id NSArray)
steps mkRoute  =
  sendMsg mkRoute (mkSelector "steps") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasTolls@
hasTolls :: IsMKRoute mkRoute => mkRoute -> IO Bool
hasTolls mkRoute  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkRoute (mkSelector "hasTolls") retCULong []

-- | @- hasHighways@
hasHighways :: IsMKRoute mkRoute => mkRoute -> IO Bool
hasHighways mkRoute  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkRoute (mkSelector "hasHighways") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @advisoryNotices@
advisoryNoticesSelector :: Selector
advisoryNoticesSelector = mkSelector "advisoryNotices"

-- | @Selector@ for @distance@
distanceSelector :: Selector
distanceSelector = mkSelector "distance"

-- | @Selector@ for @expectedTravelTime@
expectedTravelTimeSelector :: Selector
expectedTravelTimeSelector = mkSelector "expectedTravelTime"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @polyline@
polylineSelector :: Selector
polylineSelector = mkSelector "polyline"

-- | @Selector@ for @steps@
stepsSelector :: Selector
stepsSelector = mkSelector "steps"

-- | @Selector@ for @hasTolls@
hasTollsSelector :: Selector
hasTollsSelector = mkSelector "hasTolls"

-- | @Selector@ for @hasHighways@
hasHighwaysSelector :: Selector
hasHighwaysSelector = mkSelector "hasHighways"

