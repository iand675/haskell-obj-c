{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPolygon@.
module ObjC.MapKit.MKPolygon
  ( MKPolygon
  , IsMKPolygon(..)
  , polygonWithCoordinates_count
  , polygonWithCoordinates_count_interiorPolygons
  , interiorPolygons
  , polygonWithCoordinates_countSelector
  , polygonWithCoordinates_count_interiorPolygonsSelector
  , interiorPolygonsSelector


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
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ polygonWithCoordinates:count:@
polygonWithCoordinates_count :: Const (Ptr CLLocationCoordinate2D) -> CULong -> IO (Id MKPolygon)
polygonWithCoordinates_count coords count =
  do
    cls' <- getRequiredClass "MKPolygon"
    sendClassMsg cls' (mkSelector "polygonWithCoordinates:count:") (retPtr retVoid) [argPtr (unConst coords), argCULong (fromIntegral count)] >>= retainedObject . castPtr

-- | @+ polygonWithCoordinates:count:interiorPolygons:@
polygonWithCoordinates_count_interiorPolygons :: IsNSArray interiorPolygons => Const (Ptr CLLocationCoordinate2D) -> CULong -> interiorPolygons -> IO (Id MKPolygon)
polygonWithCoordinates_count_interiorPolygons coords count interiorPolygons =
  do
    cls' <- getRequiredClass "MKPolygon"
    withObjCPtr interiorPolygons $ \raw_interiorPolygons ->
      sendClassMsg cls' (mkSelector "polygonWithCoordinates:count:interiorPolygons:") (retPtr retVoid) [argPtr (unConst coords), argCULong (fromIntegral count), argPtr (castPtr raw_interiorPolygons :: Ptr ())] >>= retainedObject . castPtr

-- | @- interiorPolygons@
interiorPolygons :: IsMKPolygon mkPolygon => mkPolygon -> IO (Id NSArray)
interiorPolygons mkPolygon  =
  sendMsg mkPolygon (mkSelector "interiorPolygons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @polygonWithCoordinates:count:@
polygonWithCoordinates_countSelector :: Selector
polygonWithCoordinates_countSelector = mkSelector "polygonWithCoordinates:count:"

-- | @Selector@ for @polygonWithCoordinates:count:interiorPolygons:@
polygonWithCoordinates_count_interiorPolygonsSelector :: Selector
polygonWithCoordinates_count_interiorPolygonsSelector = mkSelector "polygonWithCoordinates:count:interiorPolygons:"

-- | @Selector@ for @interiorPolygons@
interiorPolygonsSelector :: Selector
interiorPolygonsSelector = mkSelector "interiorPolygons"

