{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPolyline@.
module ObjC.MapKit.MKMultiPolyline
  ( MKMultiPolyline
  , IsMKMultiPolyline(..)
  , initWithPolylines
  , polylines
  , initWithPolylinesSelector
  , polylinesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPolylines:@
initWithPolylines :: (IsMKMultiPolyline mkMultiPolyline, IsNSArray polylines) => mkMultiPolyline -> polylines -> IO (Id MKMultiPolyline)
initWithPolylines mkMultiPolyline polylines =
  sendOwnedMessage mkMultiPolyline initWithPolylinesSelector (toNSArray polylines)

-- | @- polylines@
polylines :: IsMKMultiPolyline mkMultiPolyline => mkMultiPolyline -> IO (Id NSArray)
polylines mkMultiPolyline =
  sendMessage mkMultiPolyline polylinesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPolylines:@
initWithPolylinesSelector :: Selector '[Id NSArray] (Id MKMultiPolyline)
initWithPolylinesSelector = mkSelector "initWithPolylines:"

-- | @Selector@ for @polylines@
polylinesSelector :: Selector '[] (Id NSArray)
polylinesSelector = mkSelector "polylines"

