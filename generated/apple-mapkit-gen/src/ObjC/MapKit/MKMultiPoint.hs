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
  , pointsSelector
  , getCoordinates_rangeSelector
  , locationAtPointIndexSelector
  , locationsAtPointIndexesSelector
  , pointCountSelector


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
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- points@
points :: IsMKMultiPoint mkMultiPoint => mkMultiPoint -> IO RawId
points mkMultiPoint  =
    fmap (RawId . castPtr) $ sendMsg mkMultiPoint (mkSelector "points") (retPtr retVoid) []

-- | @- getCoordinates:range:@
getCoordinates_range :: IsMKMultiPoint mkMultiPoint => mkMultiPoint -> RawId -> NSRange -> IO ()
getCoordinates_range mkMultiPoint  coords range =
    sendMsg mkMultiPoint (mkSelector "getCoordinates:range:") retVoid [argPtr (castPtr (unRawId coords) :: Ptr ()), argNSRange range]

-- | @- locationAtPointIndex:@
locationAtPointIndex :: IsMKMultiPoint mkMultiPoint => mkMultiPoint -> CULong -> IO CDouble
locationAtPointIndex mkMultiPoint  index =
    sendMsg mkMultiPoint (mkSelector "locationAtPointIndex:") retCDouble [argCULong index]

-- | @- locationsAtPointIndexes:@
locationsAtPointIndexes :: (IsMKMultiPoint mkMultiPoint, IsNSIndexSet indexes) => mkMultiPoint -> indexes -> IO (Id NSArray)
locationsAtPointIndexes mkMultiPoint  indexes =
  withObjCPtr indexes $ \raw_indexes ->
      sendMsg mkMultiPoint (mkSelector "locationsAtPointIndexes:") (retPtr retVoid) [argPtr (castPtr raw_indexes :: Ptr ())] >>= retainedObject . castPtr

-- | @- pointCount@
pointCount :: IsMKMultiPoint mkMultiPoint => mkMultiPoint -> IO CULong
pointCount mkMultiPoint  =
    sendMsg mkMultiPoint (mkSelector "pointCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @points@
pointsSelector :: Selector
pointsSelector = mkSelector "points"

-- | @Selector@ for @getCoordinates:range:@
getCoordinates_rangeSelector :: Selector
getCoordinates_rangeSelector = mkSelector "getCoordinates:range:"

-- | @Selector@ for @locationAtPointIndex:@
locationAtPointIndexSelector :: Selector
locationAtPointIndexSelector = mkSelector "locationAtPointIndex:"

-- | @Selector@ for @locationsAtPointIndexes:@
locationsAtPointIndexesSelector :: Selector
locationsAtPointIndexesSelector = mkSelector "locationsAtPointIndexes:"

-- | @Selector@ for @pointCount@
pointCountSelector :: Selector
pointCountSelector = mkSelector "pointCount"

