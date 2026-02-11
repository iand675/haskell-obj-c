{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKGeodesicPolyline@.
module ObjC.MapKit.MKGeodesicPolyline
  ( MKGeodesicPolyline
  , IsMKGeodesicPolyline(..)
  , polylineWithCoordinates_count
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
import ObjC.CoreLocation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ polylineWithCoordinates:count:@
polylineWithCoordinates_count :: Const (Ptr CLLocationCoordinate2D) -> CULong -> IO (Id MKGeodesicPolyline)
polylineWithCoordinates_count coords count =
  do
    cls' <- getRequiredClass "MKGeodesicPolyline"
    sendClassMsg cls' (mkSelector "polylineWithCoordinates:count:") (retPtr retVoid) [argPtr (unConst coords), argCULong (fromIntegral count)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @polylineWithCoordinates:count:@
polylineWithCoordinates_countSelector :: Selector
polylineWithCoordinates_countSelector = mkSelector "polylineWithCoordinates:count:"

