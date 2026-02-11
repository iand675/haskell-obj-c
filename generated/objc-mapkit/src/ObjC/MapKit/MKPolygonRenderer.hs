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

-- | @- initWithPolygon:@
initWithPolygon :: (IsMKPolygonRenderer mkPolygonRenderer, IsMKPolygon polygon) => mkPolygonRenderer -> polygon -> IO (Id MKPolygonRenderer)
initWithPolygon mkPolygonRenderer  polygon =
withObjCPtr polygon $ \raw_polygon ->
    sendMsg mkPolygonRenderer (mkSelector "initWithPolygon:") (retPtr retVoid) [argPtr (castPtr raw_polygon :: Ptr ())] >>= ownedObject . castPtr

-- | @- polygon@
polygon :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> IO (Id MKPolygon)
polygon mkPolygonRenderer  =
  sendMsg mkPolygonRenderer (mkSelector "polygon") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- strokeStart@
strokeStart :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> IO CDouble
strokeStart mkPolygonRenderer  =
  sendMsg mkPolygonRenderer (mkSelector "strokeStart") retCDouble []

-- | @- setStrokeStart:@
setStrokeStart :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> CDouble -> IO ()
setStrokeStart mkPolygonRenderer  value =
  sendMsg mkPolygonRenderer (mkSelector "setStrokeStart:") retVoid [argCDouble (fromIntegral value)]

-- | @- strokeEnd@
strokeEnd :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> IO CDouble
strokeEnd mkPolygonRenderer  =
  sendMsg mkPolygonRenderer (mkSelector "strokeEnd") retCDouble []

-- | @- setStrokeEnd:@
setStrokeEnd :: IsMKPolygonRenderer mkPolygonRenderer => mkPolygonRenderer -> CDouble -> IO ()
setStrokeEnd mkPolygonRenderer  value =
  sendMsg mkPolygonRenderer (mkSelector "setStrokeEnd:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPolygon:@
initWithPolygonSelector :: Selector
initWithPolygonSelector = mkSelector "initWithPolygon:"

-- | @Selector@ for @polygon@
polygonSelector :: Selector
polygonSelector = mkSelector "polygon"

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

