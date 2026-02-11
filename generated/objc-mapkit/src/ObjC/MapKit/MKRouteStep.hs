{-# LANGUAGE PatternSynonyms #-}
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
  , instructionsSelector
  , noticeSelector
  , polylineSelector
  , distanceSelector
  , transportTypeSelector

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

-- | @- instructions@
instructions :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO (Id NSString)
instructions mkRouteStep  =
  sendMsg mkRouteStep (mkSelector "instructions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notice@
notice :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO (Id NSString)
notice mkRouteStep  =
  sendMsg mkRouteStep (mkSelector "notice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- polyline@
polyline :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO (Id MKPolyline)
polyline mkRouteStep  =
  sendMsg mkRouteStep (mkSelector "polyline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- distance@
distance :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO CDouble
distance mkRouteStep  =
  sendMsg mkRouteStep (mkSelector "distance") retCDouble []

-- | @- transportType@
transportType :: IsMKRouteStep mkRouteStep => mkRouteStep -> IO MKDirectionsTransportType
transportType mkRouteStep  =
  fmap (coerce :: CULong -> MKDirectionsTransportType) $ sendMsg mkRouteStep (mkSelector "transportType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instructions@
instructionsSelector :: Selector
instructionsSelector = mkSelector "instructions"

-- | @Selector@ for @notice@
noticeSelector :: Selector
noticeSelector = mkSelector "notice"

-- | @Selector@ for @polyline@
polylineSelector :: Selector
polylineSelector = mkSelector "polyline"

-- | @Selector@ for @distance@
distanceSelector :: Selector
distanceSelector = mkSelector "distance"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector
transportTypeSelector = mkSelector "transportType"

