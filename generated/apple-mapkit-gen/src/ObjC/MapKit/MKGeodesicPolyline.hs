{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGeodesicPolyline@.
module ObjC.MapKit.MKGeodesicPolyline
  ( MKGeodesicPolyline
  , IsMKGeodesicPolyline(..)
  , polylineWithPoints_count
  , polylineWithCoordinates_count
  , polylineWithCoordinates_countSelector
  , polylineWithPoints_countSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ polylineWithPoints:count:@
polylineWithPoints_count :: Const RawId -> CULong -> IO (Id MKGeodesicPolyline)
polylineWithPoints_count points count =
  do
    cls' <- getRequiredClass "MKGeodesicPolyline"
    sendClassMessage cls' polylineWithPoints_countSelector points count

-- | @+ polylineWithCoordinates:count:@
polylineWithCoordinates_count :: Const RawId -> CULong -> IO (Id MKGeodesicPolyline)
polylineWithCoordinates_count coords count =
  do
    cls' <- getRequiredClass "MKGeodesicPolyline"
    sendClassMessage cls' polylineWithCoordinates_countSelector coords count

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @polylineWithPoints:count:@
polylineWithPoints_countSelector :: Selector '[Const RawId, CULong] (Id MKGeodesicPolyline)
polylineWithPoints_countSelector = mkSelector "polylineWithPoints:count:"

-- | @Selector@ for @polylineWithCoordinates:count:@
polylineWithCoordinates_countSelector :: Selector '[Const RawId, CULong] (Id MKGeodesicPolyline)
polylineWithCoordinates_countSelector = mkSelector "polylineWithCoordinates:count:"

