{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPoint@.
module ObjC.MapKit.MKMultiPoint
  ( MKMultiPoint
  , IsMKMultiPoint(..)
  , points
  , getCoordinates_range
  , locationAtPointIndex
  , locationsAtPointIndexes
  , pointCount
  , getCoordinates_rangeSelector
  , locationAtPointIndexSelector
  , locationsAtPointIndexesSelector
  , pointCountSelector
  , pointsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- points@
points :: IsMKMultiPoint mkMultiPoint => mkMultiPoint -> IO RawId
points mkMultiPoint =
  sendMessage mkMultiPoint pointsSelector

-- | @- getCoordinates:range:@
getCoordinates_range :: IsMKMultiPoint mkMultiPoint => mkMultiPoint -> RawId -> NSRange -> IO ()
getCoordinates_range mkMultiPoint coords range =
  sendMessage mkMultiPoint getCoordinates_rangeSelector coords range

-- | @- locationAtPointIndex:@
locationAtPointIndex :: IsMKMultiPoint mkMultiPoint => mkMultiPoint -> CULong -> IO CDouble
locationAtPointIndex mkMultiPoint index =
  sendMessage mkMultiPoint locationAtPointIndexSelector index

-- | @- locationsAtPointIndexes:@
locationsAtPointIndexes :: (IsMKMultiPoint mkMultiPoint, IsNSIndexSet indexes) => mkMultiPoint -> indexes -> IO (Id NSArray)
locationsAtPointIndexes mkMultiPoint indexes =
  sendMessage mkMultiPoint locationsAtPointIndexesSelector (toNSIndexSet indexes)

-- | @- pointCount@
pointCount :: IsMKMultiPoint mkMultiPoint => mkMultiPoint -> IO CULong
pointCount mkMultiPoint =
  sendMessage mkMultiPoint pointCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @points@
pointsSelector :: Selector '[] RawId
pointsSelector = mkSelector "points"

-- | @Selector@ for @getCoordinates:range:@
getCoordinates_rangeSelector :: Selector '[RawId, NSRange] ()
getCoordinates_rangeSelector = mkSelector "getCoordinates:range:"

-- | @Selector@ for @locationAtPointIndex:@
locationAtPointIndexSelector :: Selector '[CULong] CDouble
locationAtPointIndexSelector = mkSelector "locationAtPointIndex:"

-- | @Selector@ for @locationsAtPointIndexes:@
locationsAtPointIndexesSelector :: Selector '[Id NSIndexSet] (Id NSArray)
locationsAtPointIndexesSelector = mkSelector "locationsAtPointIndexes:"

-- | @Selector@ for @pointCount@
pointCountSelector :: Selector '[] CULong
pointCountSelector = mkSelector "pointCount"

