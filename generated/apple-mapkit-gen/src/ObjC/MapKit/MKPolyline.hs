{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPolyline@.
module ObjC.MapKit.MKPolyline
  ( MKPolyline
  , IsMKPolyline(..)
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
polylineWithPoints_count :: Const RawId -> CULong -> IO (Id MKPolyline)
polylineWithPoints_count points count =
  do
    cls' <- getRequiredClass "MKPolyline"
    sendClassMessage cls' polylineWithPoints_countSelector points count

-- | @+ polylineWithCoordinates:count:@
polylineWithCoordinates_count :: Const RawId -> CULong -> IO (Id MKPolyline)
polylineWithCoordinates_count coords count =
  do
    cls' <- getRequiredClass "MKPolyline"
    sendClassMessage cls' polylineWithCoordinates_countSelector coords count

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @polylineWithPoints:count:@
polylineWithPoints_countSelector :: Selector '[Const RawId, CULong] (Id MKPolyline)
polylineWithPoints_countSelector = mkSelector "polylineWithPoints:count:"

-- | @Selector@ for @polylineWithCoordinates:count:@
polylineWithCoordinates_countSelector :: Selector '[Const RawId, CULong] (Id MKPolyline)
polylineWithCoordinates_countSelector = mkSelector "polylineWithCoordinates:count:"

