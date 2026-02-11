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
  , strokeStartSelector
  , setStrokeStartSelector
  , strokeEndSelector
  , setStrokeEndSelector


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

-- | @- initWithPolyline:@
initWithPolyline :: (IsMKPolylineRenderer mkPolylineRenderer, IsMKPolyline polyline) => mkPolylineRenderer -> polyline -> IO (Id MKPolylineRenderer)
initWithPolyline mkPolylineRenderer  polyline =
withObjCPtr polyline $ \raw_polyline ->
    sendMsg mkPolylineRenderer (mkSelector "initWithPolyline:") (retPtr retVoid) [argPtr (castPtr raw_polyline :: Ptr ())] >>= ownedObject . castPtr

-- | @- polyline@
polyline :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> IO (Id MKPolyline)
polyline mkPolylineRenderer  =
  sendMsg mkPolylineRenderer (mkSelector "polyline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- strokeStart@
strokeStart :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> IO CDouble
strokeStart mkPolylineRenderer  =
  sendMsg mkPolylineRenderer (mkSelector "strokeStart") retCDouble []

-- | @- setStrokeStart:@
setStrokeStart :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> CDouble -> IO ()
setStrokeStart mkPolylineRenderer  value =
  sendMsg mkPolylineRenderer (mkSelector "setStrokeStart:") retVoid [argCDouble (fromIntegral value)]

-- | @- strokeEnd@
strokeEnd :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> IO CDouble
strokeEnd mkPolylineRenderer  =
  sendMsg mkPolylineRenderer (mkSelector "strokeEnd") retCDouble []

-- | @- setStrokeEnd:@
setStrokeEnd :: IsMKPolylineRenderer mkPolylineRenderer => mkPolylineRenderer -> CDouble -> IO ()
setStrokeEnd mkPolylineRenderer  value =
  sendMsg mkPolylineRenderer (mkSelector "setStrokeEnd:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPolyline:@
initWithPolylineSelector :: Selector
initWithPolylineSelector = mkSelector "initWithPolyline:"

-- | @Selector@ for @polyline@
polylineSelector :: Selector
polylineSelector = mkSelector "polyline"

-- | @Selector@ for @strokeStart@
strokeStartSelector :: Selector
strokeStartSelector = mkSelector "strokeStart"

-- | @Selector@ for @setStrokeStart:@
setStrokeStartSelector :: Selector
setStrokeStartSelector = mkSelector "setStrokeStart:"

-- | @Selector@ for @strokeEnd@
strokeEndSelector :: Selector
strokeEndSelector = mkSelector "strokeEnd"

-- | @Selector@ for @setStrokeEnd:@
setStrokeEndSelector :: Selector
setStrokeEndSelector = mkSelector "setStrokeEnd:"

