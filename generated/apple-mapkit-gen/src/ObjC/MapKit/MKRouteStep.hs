{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKRouteStep@.
module ObjC.MapKit.MKRouteStep
  ( MKRouteStep
  , IsMKRouteStep(..)
  , instructions
  , notice
  , polyline
  , distance
  , transportType
  , distanceSelector
  , instructionsSelector
  , noticeSelector
  , polylineSelector
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

-- | @- instructions@
instructions :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO (Id NSString)
instructions mkRouteStep =
  sendMessage mkRouteStep instructionsSelector

-- | @- notice@
notice :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO (Id NSString)
notice mkRouteStep =
  sendMessage mkRouteStep noticeSelector

-- | @- polyline@
polyline :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO (Id MKPolyline)
polyline mkRouteStep =
  sendMessage mkRouteStep polylineSelector

-- | @- distance@
distance :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO CDouble
distance mkRouteStep =
  sendMessage mkRouteStep distanceSelector

-- | @- transportType@
transportType :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO MKDirectionsTransportType
transportType mkRouteStep =
  sendMessage mkRouteStep transportTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instructions@
instructionsSelector :: Selector '[] (Id NSString)
instructionsSelector = mkSelector "instructions"

-- | @Selector@ for @notice@
noticeSelector :: Selector '[] (Id NSString)
noticeSelector = mkSelector "notice"

-- | @Selector@ for @polyline@
polylineSelector :: Selector '[] (Id MKPolyline)
polylineSelector = mkSelector "polyline"

-- | @Selector@ for @distance@
distanceSelector :: Selector '[] CDouble
distanceSelector = mkSelector "distance"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector '[] MKDirectionsTransportType
transportTypeSelector = mkSelector "transportType"

