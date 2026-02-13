{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPolygon@.
module ObjC.MapKit.MKPolygon
  ( MKPolygon
  , IsMKPolygon(..)
  , polygonWithPoints_count
  , polygonWithPoints_count_interiorPolygons
  , polygonWithCoordinates_count
  , polygonWithCoordinates_count_interiorPolygons
  , interiorPolygons
  , interiorPolygonsSelector
  , polygonWithCoordinates_countSelector
  , polygonWithCoordinates_count_interiorPolygonsSelector
  , polygonWithPoints_countSelector
  , polygonWithPoints_count_interiorPolygonsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ polygonWithPoints:count:@
polygonWithPoints_count :: Const RawId -> CULong -> IO (Id MKPolygon)
polygonWithPoints_count points count =
  do
    cls' <- getRequiredClass "MKPolygon"
    sendClassMessage cls' polygonWithPoints_countSelector points count

-- | @+ polygonWithPoints:count:interiorPolygons:@
polygonWithPoints_count_interiorPolygons :: IsNSArray interiorPolygons => Const RawId -> CULong -> interiorPolygons -> IO (Id MKPolygon)
polygonWithPoints_count_interiorPolygons points count interiorPolygons =
  do
    cls' <- getRequiredClass "MKPolygon"
    sendClassMessage cls' polygonWithPoints_count_interiorPolygonsSelector points count (toNSArray interiorPolygons)

-- | @+ polygonWithCoordinates:count:@
polygonWithCoordinates_count :: Const RawId -> CULong -> IO (Id MKPolygon)
polygonWithCoordinates_count coords count =
  do
    cls' <- getRequiredClass "MKPolygon"
    sendClassMessage cls' polygonWithCoordinates_countSelector coords count

-- | @+ polygonWithCoordinates:count:interiorPolygons:@
polygonWithCoordinates_count_interiorPolygons :: IsNSArray interiorPolygons => Const RawId -> CULong -> interiorPolygons -> IO (Id MKPolygon)
polygonWithCoordinates_count_interiorPolygons coords count interiorPolygons =
  do
    cls' <- getRequiredClass "MKPolygon"
    sendClassMessage cls' polygonWithCoordinates_count_interiorPolygonsSelector coords count (toNSArray interiorPolygons)

-- | @- interiorPolygons@
interiorPolygons :: IsMKPolygon mkPolygon => mkPolygon -> IO (Id NSArray)
interiorPolygons mkPolygon =
  sendMessage mkPolygon interiorPolygonsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @polygonWithPoints:count:@
polygonWithPoints_countSelector :: Selector '[Const RawId, CULong] (Id MKPolygon)
polygonWithPoints_countSelector = mkSelector "polygonWithPoints:count:"

-- | @Selector@ for @polygonWithPoints:count:interiorPolygons:@
polygonWithPoints_count_interiorPolygonsSelector :: Selector '[Const RawId, CULong, Id NSArray] (Id MKPolygon)
polygonWithPoints_count_interiorPolygonsSelector = mkSelector "polygonWithPoints:count:interiorPolygons:"

-- | @Selector@ for @polygonWithCoordinates:count:@
polygonWithCoordinates_countSelector :: Selector '[Const RawId, CULong] (Id MKPolygon)
polygonWithCoordinates_countSelector = mkSelector "polygonWithCoordinates:count:"

-- | @Selector@ for @polygonWithCoordinates:count:interiorPolygons:@
polygonWithCoordinates_count_interiorPolygonsSelector :: Selector '[Const RawId, CULong, Id NSArray] (Id MKPolygon)
polygonWithCoordinates_count_interiorPolygonsSelector = mkSelector "polygonWithCoordinates:count:interiorPolygons:"

-- | @Selector@ for @interiorPolygons@
interiorPolygonsSelector :: Selector '[] (Id NSArray)
interiorPolygonsSelector = mkSelector "interiorPolygons"

