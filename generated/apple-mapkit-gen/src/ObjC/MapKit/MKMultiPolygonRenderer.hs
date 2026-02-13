{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPolygonRenderer@.
module ObjC.MapKit.MKMultiPolygonRenderer
  ( MKMultiPolygonRenderer
  , IsMKMultiPolygonRenderer(..)
  , initWithMultiPolygon
  , multiPolygon
  , initWithMultiPolygonSelector
  , multiPolygonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMultiPolygon:@
initWithMultiPolygon :: (IsMKMultiPolygonRenderer mkMultiPolygonRenderer, IsMKMultiPolygon multiPolygon) => mkMultiPolygonRenderer -> multiPolygon -> IO (Id MKMultiPolygonRenderer)
initWithMultiPolygon mkMultiPolygonRenderer multiPolygon =
  sendOwnedMessage mkMultiPolygonRenderer initWithMultiPolygonSelector (toMKMultiPolygon multiPolygon)

-- | @- multiPolygon@
multiPolygon :: IsMKMultiPolygonRenderer mkMultiPolygonRenderer => mkMultiPolygonRenderer -> IO (Id MKMultiPolygon)
multiPolygon mkMultiPolygonRenderer =
  sendMessage mkMultiPolygonRenderer multiPolygonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMultiPolygon:@
initWithMultiPolygonSelector :: Selector '[Id MKMultiPolygon] (Id MKMultiPolygonRenderer)
initWithMultiPolygonSelector = mkSelector "initWithMultiPolygon:"

-- | @Selector@ for @multiPolygon@
multiPolygonSelector :: Selector '[] (Id MKMultiPolygon)
multiPolygonSelector = mkSelector "multiPolygon"

