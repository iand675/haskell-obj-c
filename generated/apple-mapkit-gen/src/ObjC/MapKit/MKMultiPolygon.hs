{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPolygon@.
module ObjC.MapKit.MKMultiPolygon
  ( MKMultiPolygon
  , IsMKMultiPolygon(..)
  , initWithPolygons
  , polygons
  , initWithPolygonsSelector
  , polygonsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPolygons:@
initWithPolygons :: (IsMKMultiPolygon mkMultiPolygon, IsNSArray polygons) => mkMultiPolygon -> polygons -> IO (Id MKMultiPolygon)
initWithPolygons mkMultiPolygon polygons =
  sendOwnedMessage mkMultiPolygon initWithPolygonsSelector (toNSArray polygons)

-- | @- polygons@
polygons :: IsMKMultiPolygon mkMultiPolygon => mkMultiPolygon -> IO (Id NSArray)
polygons mkMultiPolygon =
  sendMessage mkMultiPolygon polygonsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPolygons:@
initWithPolygonsSelector :: Selector '[Id NSArray] (Id MKMultiPolygon)
initWithPolygonsSelector = mkSelector "initWithPolygons:"

-- | @Selector@ for @polygons@
polygonsSelector :: Selector '[] (Id NSArray)
polygonsSelector = mkSelector "polygons"

