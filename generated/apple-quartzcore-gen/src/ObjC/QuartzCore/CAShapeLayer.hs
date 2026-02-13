{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAShapeLayer@.
module ObjC.QuartzCore.CAShapeLayer
  ( CAShapeLayer
  , IsCAShapeLayer(..)
  , path
  , setPath
  , fillColor
  , setFillColor
  , fillRule
  , setFillRule
  , strokeColor
  , setStrokeColor
  , strokeStart
  , setStrokeStart
  , strokeEnd
  , setStrokeEnd
  , lineWidth
  , setLineWidth
  , miterLimit
  , setMiterLimit
  , lineCap
  , setLineCap
  , lineJoin
  , setLineJoin
  , lineDashPhase
  , setLineDashPhase
  , lineDashPattern
  , setLineDashPattern
  , fillColorSelector
  , fillRuleSelector
  , lineCapSelector
  , lineDashPatternSelector
  , lineDashPhaseSelector
  , lineJoinSelector
  , lineWidthSelector
  , miterLimitSelector
  , pathSelector
  , setFillColorSelector
  , setFillRuleSelector
  , setLineCapSelector
  , setLineDashPatternSelector
  , setLineDashPhaseSelector
  , setLineJoinSelector
  , setLineWidthSelector
  , setMiterLimitSelector
  , setPathSelector
  , setStrokeColorSelector
  , setStrokeEndSelector
  , setStrokeStartSelector
  , strokeColorSelector
  , strokeEndSelector
  , strokeStartSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- path@
path :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO RawId
path caShapeLayer =
  sendMessage caShapeLayer pathSelector

-- | @- setPath:@
setPath :: IsCAShapeLayer caShapeLayer => caShapeLayer -> RawId -> IO ()
setPath caShapeLayer value =
  sendMessage caShapeLayer setPathSelector value

-- | @- fillColor@
fillColor :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Ptr ())
fillColor caShapeLayer =
  sendMessage caShapeLayer fillColorSelector

-- | @- setFillColor:@
setFillColor :: IsCAShapeLayer caShapeLayer => caShapeLayer -> Ptr () -> IO ()
setFillColor caShapeLayer value =
  sendMessage caShapeLayer setFillColorSelector value

-- | @- fillRule@
fillRule :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Id NSString)
fillRule caShapeLayer =
  sendMessage caShapeLayer fillRuleSelector

-- | @- setFillRule:@
setFillRule :: (IsCAShapeLayer caShapeLayer, IsNSString value) => caShapeLayer -> value -> IO ()
setFillRule caShapeLayer value =
  sendMessage caShapeLayer setFillRuleSelector (toNSString value)

-- | @- strokeColor@
strokeColor :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Ptr ())
strokeColor caShapeLayer =
  sendMessage caShapeLayer strokeColorSelector

-- | @- setStrokeColor:@
setStrokeColor :: IsCAShapeLayer caShapeLayer => caShapeLayer -> Ptr () -> IO ()
setStrokeColor caShapeLayer value =
  sendMessage caShapeLayer setStrokeColorSelector value

-- | @- strokeStart@
strokeStart :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
strokeStart caShapeLayer =
  sendMessage caShapeLayer strokeStartSelector

-- | @- setStrokeStart:@
setStrokeStart :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setStrokeStart caShapeLayer value =
  sendMessage caShapeLayer setStrokeStartSelector value

-- | @- strokeEnd@
strokeEnd :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
strokeEnd caShapeLayer =
  sendMessage caShapeLayer strokeEndSelector

-- | @- setStrokeEnd:@
setStrokeEnd :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setStrokeEnd caShapeLayer value =
  sendMessage caShapeLayer setStrokeEndSelector value

-- | @- lineWidth@
lineWidth :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
lineWidth caShapeLayer =
  sendMessage caShapeLayer lineWidthSelector

-- | @- setLineWidth:@
setLineWidth :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setLineWidth caShapeLayer value =
  sendMessage caShapeLayer setLineWidthSelector value

-- | @- miterLimit@
miterLimit :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
miterLimit caShapeLayer =
  sendMessage caShapeLayer miterLimitSelector

-- | @- setMiterLimit:@
setMiterLimit :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setMiterLimit caShapeLayer value =
  sendMessage caShapeLayer setMiterLimitSelector value

-- | @- lineCap@
lineCap :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Id NSString)
lineCap caShapeLayer =
  sendMessage caShapeLayer lineCapSelector

-- | @- setLineCap:@
setLineCap :: (IsCAShapeLayer caShapeLayer, IsNSString value) => caShapeLayer -> value -> IO ()
setLineCap caShapeLayer value =
  sendMessage caShapeLayer setLineCapSelector (toNSString value)

-- | @- lineJoin@
lineJoin :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Id NSString)
lineJoin caShapeLayer =
  sendMessage caShapeLayer lineJoinSelector

-- | @- setLineJoin:@
setLineJoin :: (IsCAShapeLayer caShapeLayer, IsNSString value) => caShapeLayer -> value -> IO ()
setLineJoin caShapeLayer value =
  sendMessage caShapeLayer setLineJoinSelector (toNSString value)

-- | @- lineDashPhase@
lineDashPhase :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
lineDashPhase caShapeLayer =
  sendMessage caShapeLayer lineDashPhaseSelector

-- | @- setLineDashPhase:@
setLineDashPhase :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setLineDashPhase caShapeLayer value =
  sendMessage caShapeLayer setLineDashPhaseSelector value

-- | @- lineDashPattern@
lineDashPattern :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Id NSArray)
lineDashPattern caShapeLayer =
  sendMessage caShapeLayer lineDashPatternSelector

-- | @- setLineDashPattern:@
setLineDashPattern :: (IsCAShapeLayer caShapeLayer, IsNSArray value) => caShapeLayer -> value -> IO ()
setLineDashPattern caShapeLayer value =
  sendMessage caShapeLayer setLineDashPatternSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @path@
pathSelector :: Selector '[] RawId
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[RawId] ()
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector '[] (Ptr ())
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector '[Ptr ()] ()
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @fillRule@
fillRuleSelector :: Selector '[] (Id NSString)
fillRuleSelector = mkSelector "fillRule"

-- | @Selector@ for @setFillRule:@
setFillRuleSelector :: Selector '[Id NSString] ()
setFillRuleSelector = mkSelector "setFillRule:"

-- | @Selector@ for @strokeColor@
strokeColorSelector :: Selector '[] (Ptr ())
strokeColorSelector = mkSelector "strokeColor"

-- | @Selector@ for @setStrokeColor:@
setStrokeColorSelector :: Selector '[Ptr ()] ()
setStrokeColorSelector = mkSelector "setStrokeColor:"

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

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector '[] CDouble
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector '[CDouble] ()
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @miterLimit@
miterLimitSelector :: Selector '[] CDouble
miterLimitSelector = mkSelector "miterLimit"

-- | @Selector@ for @setMiterLimit:@
setMiterLimitSelector :: Selector '[CDouble] ()
setMiterLimitSelector = mkSelector "setMiterLimit:"

-- | @Selector@ for @lineCap@
lineCapSelector :: Selector '[] (Id NSString)
lineCapSelector = mkSelector "lineCap"

-- | @Selector@ for @setLineCap:@
setLineCapSelector :: Selector '[Id NSString] ()
setLineCapSelector = mkSelector "setLineCap:"

-- | @Selector@ for @lineJoin@
lineJoinSelector :: Selector '[] (Id NSString)
lineJoinSelector = mkSelector "lineJoin"

-- | @Selector@ for @setLineJoin:@
setLineJoinSelector :: Selector '[Id NSString] ()
setLineJoinSelector = mkSelector "setLineJoin:"

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

