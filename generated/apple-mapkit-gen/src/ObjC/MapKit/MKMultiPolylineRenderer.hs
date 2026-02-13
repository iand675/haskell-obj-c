{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMultiPolylineRenderer@.
module ObjC.MapKit.MKMultiPolylineRenderer
  ( MKMultiPolylineRenderer
  , IsMKMultiPolylineRenderer(..)
  , initWithMultiPolyline
  , multiPolyline
  , initWithMultiPolylineSelector
  , multiPolylineSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMultiPolyline:@
initWithMultiPolyline :: (IsMKMultiPolylineRenderer mkMultiPolylineRenderer, IsMKMultiPolyline multiPolyline) => mkMultiPolylineRenderer -> multiPolyline -> IO (Id MKMultiPolylineRenderer)
initWithMultiPolyline mkMultiPolylineRenderer multiPolyline =
  sendOwnedMessage mkMultiPolylineRenderer initWithMultiPolylineSelector (toMKMultiPolyline multiPolyline)

-- | @- multiPolyline@
multiPolyline :: IsMKMultiPolylineRenderer mkMultiPolylineRenderer => mkMultiPolylineRenderer -> IO (Id MKMultiPolyline)
multiPolyline mkMultiPolylineRenderer =
  sendMessage mkMultiPolylineRenderer multiPolylineSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMultiPolyline:@
initWithMultiPolylineSelector :: Selector '[Id MKMultiPolyline] (Id MKMultiPolylineRenderer)
initWithMultiPolylineSelector = mkSelector "initWithMultiPolyline:"

-- | @Selector@ for @multiPolyline@
multiPolylineSelector :: Selector '[] (Id MKMultiPolyline)
multiPolylineSelector = mkSelector "multiPolyline"

