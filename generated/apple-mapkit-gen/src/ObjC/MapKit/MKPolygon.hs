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
  , polygonWithPoints_countSelector
  , polygonWithPoints_count_interiorPolygonsSelector
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
import ObjC.Foundation.Internal.Classes

-- | @+ polygonWithPoints:count:@
polygonWithPoints_count :: Const RawId -> CULong -> IO (Id MKPolygon)
polygonWithPoints_count points count =
  do
    cls' <- getRequiredClass "MKPolygon"
    sendClassMsg cls' (mkSelector "polygonWithPoints:count:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst points)) :: Ptr ()), argCULong count] >>= retainedObject . castPtr

-- | @+ polygonWithPoints:count:interiorPolygons:@
polygonWithPoints_count_interiorPolygons :: IsNSArray interiorPolygons => Const RawId -> CULong -> interiorPolygons -> IO (Id MKPolygon)
polygonWithPoints_count_interiorPolygons points count interiorPolygons =
  do
    cls' <- getRequiredClass "MKPolygon"
    withObjCPtr interiorPolygons $ \raw_interiorPolygons ->
      sendClassMsg cls' (mkSelector "polygonWithPoints:count:interiorPolygons:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst points)) :: Ptr ()), argCULong count, argPtr (castPtr raw_interiorPolygons :: Ptr ())] >>= retainedObject . castPtr

-- | @+ polygonWithCoordinates:count:@
polygonWithCoordinates_count :: Const RawId -> CULong -> IO (Id MKPolygon)
polygonWithCoordinates_count coords count =
  do
    cls' <- getRequiredClass "MKPolygon"
    sendClassMsg cls' (mkSelector "polygonWithCoordinates:count:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst coords)) :: Ptr ()), argCULong count] >>= retainedObject . castPtr

-- | @+ polygonWithCoordinates:count:interiorPolygons:@
polygonWithCoordinates_count_interiorPolygons :: IsNSArray interiorPolygons => Const RawId -> CULong -> interiorPolygons -> IO (Id MKPolygon)
polygonWithCoordinates_count_interiorPolygons coords count interiorPolygons =
  do
    cls' <- getRequiredClass "MKPolygon"
    withObjCPtr interiorPolygons $ \raw_interiorPolygons ->
      sendClassMsg cls' (mkSelector "polygonWithCoordinates:count:interiorPolygons:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst coords)) :: Ptr ()), argCULong count, argPtr (castPtr raw_interiorPolygons :: Ptr ())] >>= retainedObject . castPtr

-- | @- interiorPolygons@
interiorPolygons :: IsMKPolygon mkPolygon => mkPolygon -> IO (Id NSArray)
interiorPolygons mkPolygon  =
    sendMsg mkPolygon (mkSelector "interiorPolygons") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @polygonWithPoints:count:@
polygonWithPoints_countSelector :: Selector
polygonWithPoints_countSelector = mkSelector "polygonWithPoints:count:"

-- | @Selector@ for @polygonWithPoints:count:interiorPolygons:@
polygonWithPoints_count_interiorPolygonsSelector :: Selector
polygonWithPoints_count_interiorPolygonsSelector = mkSelector "polygonWithPoints:count:interiorPolygons:"

-- | @Selector@ for @polygonWithCoordinates:count:@
polygonWithCoordinates_countSelector :: Selector
polygonWithCoordinates_countSelector = mkSelector "polygonWithCoordinates:count:"

-- | @Selector@ for @polygonWithCoordinates:count:interiorPolygons:@
polygonWithCoordinates_count_interiorPolygonsSelector :: Selector
polygonWithCoordinates_count_interiorPolygonsSelector = mkSelector "polygonWithCoordinates:count:interiorPolygons:"

-- | @Selector@ for @interiorPolygons@
interiorPolygonsSelector :: Selector
interiorPolygonsSelector = mkSelector "interiorPolygons"

