{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGeodesicPolyline@.
module ObjC.MapKit.MKGeodesicPolyline
  ( MKGeodesicPolyline
  , IsMKGeodesicPolyline(..)
  , polylineWithPoints_count
  , polylineWithCoordinates_count
  , polylineWithPoints_countSelector
  , polylineWithCoordinates_countSelector


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

-- | @+ polylineWithPoints:count:@
polylineWithPoints_count :: Const RawId -> CULong -> IO (Id MKGeodesicPolyline)
polylineWithPoints_count points count =
  do
    cls' <- getRequiredClass "MKGeodesicPolyline"
    sendClassMsg cls' (mkSelector "polylineWithPoints:count:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst points)) :: Ptr ()), argCULong count] >>= retainedObject . castPtr

-- | @+ polylineWithCoordinates:count:@
polylineWithCoordinates_count :: Const RawId -> CULong -> IO (Id MKGeodesicPolyline)
polylineWithCoordinates_count coords count =
  do
    cls' <- getRequiredClass "MKGeodesicPolyline"
    sendClassMsg cls' (mkSelector "polylineWithCoordinates:count:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst coords)) :: Ptr ()), argCULong count] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @polylineWithPoints:count:@
polylineWithPoints_countSelector :: Selector
polylineWithPoints_countSelector = mkSelector "polylineWithPoints:count:"

-- | @Selector@ for @polylineWithCoordinates:count:@
polylineWithCoordinates_countSelector :: Selector
polylineWithCoordinates_countSelector = mkSelector "polylineWithCoordinates:count:"

