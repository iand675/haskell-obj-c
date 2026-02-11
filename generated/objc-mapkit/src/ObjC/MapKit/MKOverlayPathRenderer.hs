{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKOverlayPathRenderer@.
module ObjC.MapKit.MKOverlayPathRenderer
  ( MKOverlayPathRenderer
  , IsMKOverlayPathRenderer(..)
  , createPath
  , invalidatePath
  , applyStrokePropertiesToContext_atZoomScale
  , applyFillPropertiesToContext_atZoomScale
  , strokePath_inContext
  , fillPath_inContext
  , fillColor
  , setFillColor
  , strokeColor
  , setStrokeColor
  , lineWidth
  , setLineWidth
  , lineJoin
  , setLineJoin
  , lineCap
  , setLineCap
  , miterLimit
  , setMiterLimit
  , lineDashPhase
  , setLineDashPhase
  , lineDashPattern
  , setLineDashPattern
  , shouldRasterize
  , setShouldRasterize
  , path
  , setPath
  , createPathSelector
  , invalidatePathSelector
  , applyStrokePropertiesToContext_atZoomScaleSelector
  , applyFillPropertiesToContext_atZoomScaleSelector
  , strokePath_inContextSelector
  , fillPath_inContextSelector
  , fillColorSelector
  , setFillColorSelector
  , strokeColorSelector
  , setStrokeColorSelector
  , lineWidthSelector
  , setLineWidthSelector
  , lineJoinSelector
  , setLineJoinSelector
  , lineCapSelector
  , setLineCapSelector
  , miterLimitSelector
  , setMiterLimitSelector
  , lineDashPhaseSelector
  , setLineDashPhaseSelector
  , lineDashPatternSelector
  , setLineDashPatternSelector
  , shouldRasterizeSelector
  , setShouldRasterizeSelector
  , pathSelector
  , setPathSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createPath@
createPath :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO ()
createPath mkOverlayPathRenderer  =
  sendMsg mkOverlayPathRenderer (mkSelector "createPath") retVoid []

-- | @- invalidatePath@
invalidatePath :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO ()
invalidatePath mkOverlayPathRenderer  =
  sendMsg mkOverlayPathRenderer (mkSelector "invalidatePath") retVoid []

-- | @- applyStrokePropertiesToContext:atZoomScale:@
applyStrokePropertiesToContext_atZoomScale :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> Ptr () -> CDouble -> IO ()
applyStrokePropertiesToContext_atZoomScale mkOverlayPathRenderer  context zoomScale =
  sendMsg mkOverlayPathRenderer (mkSelector "applyStrokePropertiesToContext:atZoomScale:") retVoid [argPtr context, argCDouble (fromIntegral zoomScale)]

-- | @- applyFillPropertiesToContext:atZoomScale:@
applyFillPropertiesToContext_atZoomScale :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> Ptr () -> CDouble -> IO ()
applyFillPropertiesToContext_atZoomScale mkOverlayPathRenderer  context zoomScale =
  sendMsg mkOverlayPathRenderer (mkSelector "applyFillPropertiesToContext:atZoomScale:") retVoid [argPtr context, argCDouble (fromIntegral zoomScale)]

-- | @- strokePath:inContext:@
strokePath_inContext :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> RawId -> Ptr () -> IO ()
strokePath_inContext mkOverlayPathRenderer  path context =
  sendMsg mkOverlayPathRenderer (mkSelector "strokePath:inContext:") retVoid [argPtr (castPtr (unRawId path) :: Ptr ()), argPtr context]

-- | @- fillPath:inContext:@
fillPath_inContext :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> RawId -> Ptr () -> IO ()
fillPath_inContext mkOverlayPathRenderer  path context =
  sendMsg mkOverlayPathRenderer (mkSelector "fillPath:inContext:") retVoid [argPtr (castPtr (unRawId path) :: Ptr ()), argPtr context]

-- | @- fillColor@
fillColor :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO (Id NSColor)
fillColor mkOverlayPathRenderer  =
  sendMsg mkOverlayPathRenderer (mkSelector "fillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFillColor:@
setFillColor :: (IsMKOverlayPathRenderer mkOverlayPathRenderer, IsNSColor value) => mkOverlayPathRenderer -> value -> IO ()
setFillColor mkOverlayPathRenderer  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkOverlayPathRenderer (mkSelector "setFillColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- strokeColor@
strokeColor :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO (Id NSColor)
strokeColor mkOverlayPathRenderer  =
  sendMsg mkOverlayPathRenderer (mkSelector "strokeColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStrokeColor:@
setStrokeColor :: (IsMKOverlayPathRenderer mkOverlayPathRenderer, IsNSColor value) => mkOverlayPathRenderer -> value -> IO ()
setStrokeColor mkOverlayPathRenderer  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkOverlayPathRenderer (mkSelector "setStrokeColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lineWidth@
lineWidth :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CDouble
lineWidth mkOverlayPathRenderer  =
  sendMsg mkOverlayPathRenderer (mkSelector "lineWidth") retCDouble []

-- | @- setLineWidth:@
setLineWidth :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CDouble -> IO ()
setLineWidth mkOverlayPathRenderer  value =
  sendMsg mkOverlayPathRenderer (mkSelector "setLineWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineJoin@
lineJoin :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CGLineJoin
lineJoin mkOverlayPathRenderer  =
  fmap (coerce :: CInt -> CGLineJoin) $ sendMsg mkOverlayPathRenderer (mkSelector "lineJoin") retCInt []

-- | @- setLineJoin:@
setLineJoin :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CGLineJoin -> IO ()
setLineJoin mkOverlayPathRenderer  value =
  sendMsg mkOverlayPathRenderer (mkSelector "setLineJoin:") retVoid [argCInt (coerce value)]

-- | @- lineCap@
lineCap :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CGLineCap
lineCap mkOverlayPathRenderer  =
  fmap (coerce :: CInt -> CGLineCap) $ sendMsg mkOverlayPathRenderer (mkSelector "lineCap") retCInt []

-- | @- setLineCap:@
setLineCap :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CGLineCap -> IO ()
setLineCap mkOverlayPathRenderer  value =
  sendMsg mkOverlayPathRenderer (mkSelector "setLineCap:") retVoid [argCInt (coerce value)]

-- | @- miterLimit@
miterLimit :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CDouble
miterLimit mkOverlayPathRenderer  =
  sendMsg mkOverlayPathRenderer (mkSelector "miterLimit") retCDouble []

-- | @- setMiterLimit:@
setMiterLimit :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CDouble -> IO ()
setMiterLimit mkOverlayPathRenderer  value =
  sendMsg mkOverlayPathRenderer (mkSelector "setMiterLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineDashPhase@
lineDashPhase :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CDouble
lineDashPhase mkOverlayPathRenderer  =
  sendMsg mkOverlayPathRenderer (mkSelector "lineDashPhase") retCDouble []

-- | @- setLineDashPhase:@
setLineDashPhase :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CDouble -> IO ()
setLineDashPhase mkOverlayPathRenderer  value =
  sendMsg mkOverlayPathRenderer (mkSelector "setLineDashPhase:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineDashPattern@
lineDashPattern :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO (Id NSArray)
lineDashPattern mkOverlayPathRenderer  =
  sendMsg mkOverlayPathRenderer (mkSelector "lineDashPattern") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineDashPattern:@
setLineDashPattern :: (IsMKOverlayPathRenderer mkOverlayPathRenderer, IsNSArray value) => mkOverlayPathRenderer -> value -> IO ()
setLineDashPattern mkOverlayPathRenderer  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkOverlayPathRenderer (mkSelector "setLineDashPattern:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shouldRasterize@
shouldRasterize :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO Bool
shouldRasterize mkOverlayPathRenderer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkOverlayPathRenderer (mkSelector "shouldRasterize") retCULong []

-- | @- setShouldRasterize:@
setShouldRasterize :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> Bool -> IO ()
setShouldRasterize mkOverlayPathRenderer  value =
  sendMsg mkOverlayPathRenderer (mkSelector "setShouldRasterize:") retVoid [argCULong (if value then 1 else 0)]

-- | @- path@
path :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO RawId
path mkOverlayPathRenderer  =
  fmap (RawId . castPtr) $ sendMsg mkOverlayPathRenderer (mkSelector "path") (retPtr retVoid) []

-- | @- setPath:@
setPath :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> RawId -> IO ()
setPath mkOverlayPathRenderer  value =
  sendMsg mkOverlayPathRenderer (mkSelector "setPath:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createPath@
createPathSelector :: Selector
createPathSelector = mkSelector "createPath"

-- | @Selector@ for @invalidatePath@
invalidatePathSelector :: Selector
invalidatePathSelector = mkSelector "invalidatePath"

-- | @Selector@ for @applyStrokePropertiesToContext:atZoomScale:@
applyStrokePropertiesToContext_atZoomScaleSelector :: Selector
applyStrokePropertiesToContext_atZoomScaleSelector = mkSelector "applyStrokePropertiesToContext:atZoomScale:"

-- | @Selector@ for @applyFillPropertiesToContext:atZoomScale:@
applyFillPropertiesToContext_atZoomScaleSelector :: Selector
applyFillPropertiesToContext_atZoomScaleSelector = mkSelector "applyFillPropertiesToContext:atZoomScale:"

-- | @Selector@ for @strokePath:inContext:@
strokePath_inContextSelector :: Selector
strokePath_inContextSelector = mkSelector "strokePath:inContext:"

-- | @Selector@ for @fillPath:inContext:@
fillPath_inContextSelector :: Selector
fillPath_inContextSelector = mkSelector "fillPath:inContext:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @strokeColor@
strokeColorSelector :: Selector
strokeColorSelector = mkSelector "strokeColor"

-- | @Selector@ for @setStrokeColor:@
setStrokeColorSelector :: Selector
setStrokeColorSelector = mkSelector "setStrokeColor:"

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @lineJoin@
lineJoinSelector :: Selector
lineJoinSelector = mkSelector "lineJoin"

-- | @Selector@ for @setLineJoin:@
setLineJoinSelector :: Selector
setLineJoinSelector = mkSelector "setLineJoin:"

-- | @Selector@ for @lineCap@
lineCapSelector :: Selector
lineCapSelector = mkSelector "lineCap"

-- | @Selector@ for @setLineCap:@
setLineCapSelector :: Selector
setLineCapSelector = mkSelector "setLineCap:"

-- | @Selector@ for @miterLimit@
miterLimitSelector :: Selector
miterLimitSelector = mkSelector "miterLimit"

-- | @Selector@ for @setMiterLimit:@
setMiterLimitSelector :: Selector
setMiterLimitSelector = mkSelector "setMiterLimit:"

-- | @Selector@ for @lineDashPhase@
lineDashPhaseSelector :: Selector
lineDashPhaseSelector = mkSelector "lineDashPhase"

-- | @Selector@ for @setLineDashPhase:@
setLineDashPhaseSelector :: Selector
setLineDashPhaseSelector = mkSelector "setLineDashPhase:"

-- | @Selector@ for @lineDashPattern@
lineDashPatternSelector :: Selector
lineDashPatternSelector = mkSelector "lineDashPattern"

-- | @Selector@ for @setLineDashPattern:@
setLineDashPatternSelector :: Selector
setLineDashPatternSelector = mkSelector "setLineDashPattern:"

-- | @Selector@ for @shouldRasterize@
shouldRasterizeSelector :: Selector
shouldRasterizeSelector = mkSelector "shouldRasterize"

-- | @Selector@ for @setShouldRasterize:@
setShouldRasterizeSelector :: Selector
setShouldRasterizeSelector = mkSelector "setShouldRasterize:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

