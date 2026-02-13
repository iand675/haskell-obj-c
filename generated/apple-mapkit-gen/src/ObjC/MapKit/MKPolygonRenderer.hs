{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPolygonRenderer@.
module ObjC.MapKit.MKPolygonRenderer
  ( MKPolygonRenderer
  , IsMKPolygonRenderer(..)
  , initWithPolygon
  , polygon
  , strokeStart
  , setStrokeStart
  , strokeEnd
  , setStrokeEnd
  , initWithPolygonSelector
  , polygonSelector
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

-- | @- initWithPolygon:@
initWithPolygon :: (IsMKPolygonRenderer mkPolygonRenderer, IsMKPolygon polygon) => mkPolygonRenderer -> polygon -> IO (Id MKPolygonRenderer)
initWithPolygon mkPolygonRenderer polygon =
  sendOwnedMessage mkPolygonRenderer initWithPolygonSelector (toMKPolygon polygon)

-- | @- polygon@
polygon :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> IO (Id MKPolygon)
polygon mkPolygonRenderer =
  sendMessage mkPolygonRenderer polygonSelector

-- | @- strokeStart@
strokeStart :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> IO CDouble
strokeStart mkPolygonRenderer =
  sendMessage mkPolygonRenderer strokeStartSelector

-- | @- setStrokeStart:@
setStrokeStart :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> CDouble -> IO ()
setStrokeStart mkPolygonRenderer value =
  sendMessage mkPolygonRenderer setStrokeStartSelector value

-- | @- strokeEnd@
strokeEnd :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> IO CDouble
strokeEnd mkPolygonRenderer =
  sendMessage mkPolygonRenderer strokeEndSelector

-- | @- setStrokeEnd:@
setStrokeEnd :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> CDouble -> IO ()
setStrokeEnd mkPolygonRenderer value =
  sendMessage mkPolygonRenderer setStrokeEndSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPolygon:@
initWithPolygonSelector :: Selector '[Id MKPolygon] (Id MKPolygonRenderer)
initWithPolygonSelector = mkSelector "initWithPolygon:"

-- | @Selector@ for @polygon@
polygonSelector :: Selector '[] (Id MKPolygon)
polygonSelector = mkSelector "polygon"

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

