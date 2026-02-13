{-# LANGUAGE DataKinds #-}
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
  , applyFillPropertiesToContext_atZoomScaleSelector
  , applyStrokePropertiesToContext_atZoomScaleSelector
  , createPathSelector
  , fillColorSelector
  , fillPath_inContextSelector
  , invalidatePathSelector
  , lineCapSelector
  , lineDashPatternSelector
  , lineDashPhaseSelector
  , lineJoinSelector
  , lineWidthSelector
  , miterLimitSelector
  , pathSelector
  , setFillColorSelector
  , setLineCapSelector
  , setLineDashPatternSelector
  , setLineDashPhaseSelector
  , setLineJoinSelector
  , setLineWidthSelector
  , setMiterLimitSelector
  , setPathSelector
  , setShouldRasterizeSelector
  , setStrokeColorSelector
  , shouldRasterizeSelector
  , strokeColorSelector
  , strokePath_inContextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createPath@
createPath :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO ()
createPath mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer createPathSelector

-- | @- invalidatePath@
invalidatePath :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO ()
invalidatePath mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer invalidatePathSelector

-- | @- applyStrokePropertiesToContext:atZoomScale:@
applyStrokePropertiesToContext_atZoomScale :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> Ptr () -> CDouble -> IO ()
applyStrokePropertiesToContext_atZoomScale mkOverlayPathRenderer context zoomScale =
  sendMessage mkOverlayPathRenderer applyStrokePropertiesToContext_atZoomScaleSelector context zoomScale

-- | @- applyFillPropertiesToContext:atZoomScale:@
applyFillPropertiesToContext_atZoomScale :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> Ptr () -> CDouble -> IO ()
applyFillPropertiesToContext_atZoomScale mkOverlayPathRenderer context zoomScale =
  sendMessage mkOverlayPathRenderer applyFillPropertiesToContext_atZoomScaleSelector context zoomScale

-- | @- strokePath:inContext:@
strokePath_inContext :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> RawId -> Ptr () -> IO ()
strokePath_inContext mkOverlayPathRenderer path context =
  sendMessage mkOverlayPathRenderer strokePath_inContextSelector path context

-- | @- fillPath:inContext:@
fillPath_inContext :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> RawId -> Ptr () -> IO ()
fillPath_inContext mkOverlayPathRenderer path context =
  sendMessage mkOverlayPathRenderer fillPath_inContextSelector path context

-- | @- fillColor@
fillColor :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO (Id NSColor)
fillColor mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer fillColorSelector

-- | @- setFillColor:@
setFillColor :: (IsMKOverlayPathRenderer mkOverlayPathRenderer, IsNSColor value) => mkOverlayPathRenderer -> value -> IO ()
setFillColor mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setFillColorSelector (toNSColor value)

-- | @- strokeColor@
strokeColor :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO (Id NSColor)
strokeColor mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer strokeColorSelector

-- | @- setStrokeColor:@
setStrokeColor :: (IsMKOverlayPathRenderer mkOverlayPathRenderer, IsNSColor value) => mkOverlayPathRenderer -> value -> IO ()
setStrokeColor mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setStrokeColorSelector (toNSColor value)

-- | @- lineWidth@
lineWidth :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CDouble
lineWidth mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer lineWidthSelector

-- | @- setLineWidth:@
setLineWidth :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CDouble -> IO ()
setLineWidth mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setLineWidthSelector value

-- | @- lineJoin@
lineJoin :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CInt
lineJoin mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer lineJoinSelector

-- | @- setLineJoin:@
setLineJoin :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CInt -> IO ()
setLineJoin mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setLineJoinSelector value

-- | @- lineCap@
lineCap :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CInt
lineCap mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer lineCapSelector

-- | @- setLineCap:@
setLineCap :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CInt -> IO ()
setLineCap mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setLineCapSelector value

-- | @- miterLimit@
miterLimit :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CDouble
miterLimit mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer miterLimitSelector

-- | @- setMiterLimit:@
setMiterLimit :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CDouble -> IO ()
setMiterLimit mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setMiterLimitSelector value

-- | @- lineDashPhase@
lineDashPhase :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO CDouble
lineDashPhase mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer lineDashPhaseSelector

-- | @- setLineDashPhase:@
setLineDashPhase :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> CDouble -> IO ()
setLineDashPhase mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setLineDashPhaseSelector value

-- | @- lineDashPattern@
lineDashPattern :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO (Id NSArray)
lineDashPattern mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer lineDashPatternSelector

-- | @- setLineDashPattern:@
setLineDashPattern :: (IsMKOverlayPathRenderer mkOverlayPathRenderer, IsNSArray value) => mkOverlayPathRenderer -> value -> IO ()
setLineDashPattern mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setLineDashPatternSelector (toNSArray value)

-- | @- shouldRasterize@
shouldRasterize :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO Bool
shouldRasterize mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer shouldRasterizeSelector

-- | @- setShouldRasterize:@
setShouldRasterize :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> Bool -> IO ()
setShouldRasterize mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setShouldRasterizeSelector value

-- | @- path@
path :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> IO RawId
path mkOverlayPathRenderer =
  sendMessage mkOverlayPathRenderer pathSelector

-- | @- setPath:@
setPath :: IsMKOverlayPathRenderer mkOverlayPathRenderer => mkOverlayPathRenderer -> RawId -> IO ()
setPath mkOverlayPathRenderer value =
  sendMessage mkOverlayPathRenderer setPathSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createPath@
createPathSelector :: Selector '[] ()
createPathSelector = mkSelector "createPath"

-- | @Selector@ for @invalidatePath@
invalidatePathSelector :: Selector '[] ()
invalidatePathSelector = mkSelector "invalidatePath"

-- | @Selector@ for @applyStrokePropertiesToContext:atZoomScale:@
applyStrokePropertiesToContext_atZoomScaleSelector :: Selector '[Ptr (), CDouble] ()
applyStrokePropertiesToContext_atZoomScaleSelector = mkSelector "applyStrokePropertiesToContext:atZoomScale:"

-- | @Selector@ for @applyFillPropertiesToContext:atZoomScale:@
applyFillPropertiesToContext_atZoomScaleSelector :: Selector '[Ptr (), CDouble] ()
applyFillPropertiesToContext_atZoomScaleSelector = mkSelector "applyFillPropertiesToContext:atZoomScale:"

-- | @Selector@ for @strokePath:inContext:@
strokePath_inContextSelector :: Selector '[RawId, Ptr ()] ()
strokePath_inContextSelector = mkSelector "strokePath:inContext:"

-- | @Selector@ for @fillPath:inContext:@
fillPath_inContextSelector :: Selector '[RawId, Ptr ()] ()
fillPath_inContextSelector = mkSelector "fillPath:inContext:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector '[] (Id NSColor)
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector '[Id NSColor] ()
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @strokeColor@
strokeColorSelector :: Selector '[] (Id NSColor)
strokeColorSelector = mkSelector "strokeColor"

-- | @Selector@ for @setStrokeColor:@
setStrokeColorSelector :: Selector '[Id NSColor] ()
setStrokeColorSelector = mkSelector "setStrokeColor:"

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector '[] CDouble
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector '[CDouble] ()
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @lineJoin@
lineJoinSelector :: Selector '[] CInt
lineJoinSelector = mkSelector "lineJoin"

-- | @Selector@ for @setLineJoin:@
setLineJoinSelector :: Selector '[CInt] ()
setLineJoinSelector = mkSelector "setLineJoin:"

-- | @Selector@ for @lineCap@
lineCapSelector :: Selector '[] CInt
lineCapSelector = mkSelector "lineCap"

-- | @Selector@ for @setLineCap:@
setLineCapSelector :: Selector '[CInt] ()
setLineCapSelector = mkSelector "setLineCap:"

-- | @Selector@ for @miterLimit@
miterLimitSelector :: Selector '[] CDouble
miterLimitSelector = mkSelector "miterLimit"

-- | @Selector@ for @setMiterLimit:@
setMiterLimitSelector :: Selector '[CDouble] ()
setMiterLimitSelector = mkSelector "setMiterLimit:"

-- | @Selector@ for @lineDashPhase@
lineDashPhaseSelector :: Selector '[] CDouble
lineDashPhaseSelector = mkSelector "lineDashPhase"

-- | @Selector@ for @setLineDashPhase:@
setLineDashPhaseSelector :: Selector '[CDouble] ()
setLineDashPhaseSelector = mkSelector "setLineDashPhase:"

-- | @Selector@ for @lineDashPattern@
lineDashPatternSelector :: Selector '[] (Id NSArray)
lineDashPatternSelector = mkSelector "lineDashPattern"

-- | @Selector@ for @setLineDashPattern:@
setLineDashPatternSelector :: Selector '[Id NSArray] ()
setLineDashPatternSelector = mkSelector "setLineDashPattern:"

-- | @Selector@ for @shouldRasterize@
shouldRasterizeSelector :: Selector '[] Bool
shouldRasterizeSelector = mkSelector "shouldRasterize"

-- | @Selector@ for @setShouldRasterize:@
setShouldRasterizeSelector :: Selector '[Bool] ()
setShouldRasterizeSelector = mkSelector "setShouldRasterize:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] RawId
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[RawId] ()
setPathSelector = mkSelector "setPath:"

