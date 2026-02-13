{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPolylineRenderer@.
module ObjC.MapKit.MKPolylineRenderer
  ( MKPolylineRenderer
  , IsMKPolylineRenderer(..)
  , initWithPolyline
  , polyline
  , strokeStart
  , setStrokeStart
  , strokeEnd
  , setStrokeEnd
  , initWithPolylineSelector
  , polylineSelector
  , setStrokeEndSelector
  , setStrokeStartSelector
  , strokeEndSelector
  , strokeStartSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPolyline:@
initWithPolyline :: (IsMKPolylineRenderer mkPolylineRenderer, IsMKPolyline polyline) => mkPolylineRenderer -> polyline -> IO (Id MKPolylineRenderer)
initWithPolyline mkPolylineRenderer polyline =
  sendOwnedMessage mkPolylineRenderer initWithPolylineSelector (toMKPolyline polyline)

-- | @- polyline@
polyline :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> IO (Id MKPolyline)
polyline mkPolylineRenderer =
  sendMessage mkPolylineRenderer polylineSelector

-- | @- strokeStart@
strokeStart :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> IO CDouble
strokeStart mkPolylineRenderer =
  sendMessage mkPolylineRenderer strokeStartSelector

-- | @- setStrokeStart:@
setStrokeStart :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> CDouble -> IO ()
setStrokeStart mkPolylineRenderer value =
  sendMessage mkPolylineRenderer setStrokeStartSelector value

-- | @- strokeEnd@
strokeEnd :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> IO CDouble
strokeEnd mkPolylineRenderer =
  sendMessage mkPolylineRenderer strokeEndSelector

-- | @- setStrokeEnd:@
setStrokeEnd :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> CDouble -> IO ()
setStrokeEnd mkPolylineRenderer value =
  sendMessage mkPolylineRenderer setStrokeEndSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPolyline:@
initWithPolylineSelector :: Selector '[Id MKPolyline] (Id MKPolylineRenderer)
initWithPolylineSelector = mkSelector "initWithPolyline:"

-- | @Selector@ for @polyline@
polylineSelector :: Selector '[] (Id MKPolyline)
polylineSelector = mkSelector "polyline"

-- | @Selector@ for @strokeStart@
strokeStartSelector :: Selector '[] CDouble
strokeStartSelector = mkSelector "strokeStart"

-- | @Selector@ for @setStrokeStart:@
setStrokeStartSelector :: Selector '[CDouble] ()
setStrokeStartSelector = mkSelector "setStrokeStart:"

-- | @Selector@ for @strokeEnd@
strokeEndSelector :: Selector '[] CDouble
strokeEndSelector = mkSelector "strokeEnd"

-- | @Selector@ for @setStrokeEnd:@
setStrokeEndSelector :: Selector '[CDouble] ()
setStrokeEndSelector = mkSelector "setStrokeEnd:"

