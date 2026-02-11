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
  , pathSelector
  , setPathSelector
  , fillColorSelector
  , setFillColorSelector
  , fillRuleSelector
  , setFillRuleSelector
  , strokeColorSelector
  , setStrokeColorSelector
  , strokeStartSelector
  , setStrokeStartSelector
  , strokeEndSelector
  , setStrokeEndSelector
  , lineWidthSelector
  , setLineWidthSelector
  , miterLimitSelector
  , setMiterLimitSelector
  , lineCapSelector
  , setLineCapSelector
  , lineJoinSelector
  , setLineJoinSelector
  , lineDashPhaseSelector
  , setLineDashPhaseSelector
  , lineDashPatternSelector
  , setLineDashPatternSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- path@
path :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO RawId
path caShapeLayer  =
  fmap (RawId . castPtr) $ sendMsg caShapeLayer (mkSelector "path") (retPtr retVoid) []

-- | @- setPath:@
setPath :: IsCAShapeLayer caShapeLayer => caShapeLayer -> RawId -> IO ()
setPath caShapeLayer  value =
  sendMsg caShapeLayer (mkSelector "setPath:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- fillColor@
fillColor :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Ptr ())
fillColor caShapeLayer  =
  fmap castPtr $ sendMsg caShapeLayer (mkSelector "fillColor") (retPtr retVoid) []

-- | @- setFillColor:@
setFillColor :: IsCAShapeLayer caShapeLayer => caShapeLayer -> Ptr () -> IO ()
setFillColor caShapeLayer  value =
  sendMsg caShapeLayer (mkSelector "setFillColor:") retVoid [argPtr value]

-- | @- fillRule@
fillRule :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Id NSString)
fillRule caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "fillRule") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFillRule:@
setFillRule :: (IsCAShapeLayer caShapeLayer, IsNSString value) => caShapeLayer -> value -> IO ()
setFillRule caShapeLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caShapeLayer (mkSelector "setFillRule:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- strokeColor@
strokeColor :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Ptr ())
strokeColor caShapeLayer  =
  fmap castPtr $ sendMsg caShapeLayer (mkSelector "strokeColor") (retPtr retVoid) []

-- | @- setStrokeColor:@
setStrokeColor :: IsCAShapeLayer caShapeLayer => caShapeLayer -> Ptr () -> IO ()
setStrokeColor caShapeLayer  value =
  sendMsg caShapeLayer (mkSelector "setStrokeColor:") retVoid [argPtr value]

-- | @- strokeStart@
strokeStart :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
strokeStart caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "strokeStart") retCDouble []

-- | @- setStrokeStart:@
setStrokeStart :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setStrokeStart caShapeLayer  value =
  sendMsg caShapeLayer (mkSelector "setStrokeStart:") retVoid [argCDouble (fromIntegral value)]

-- | @- strokeEnd@
strokeEnd :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
strokeEnd caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "strokeEnd") retCDouble []

-- | @- setStrokeEnd:@
setStrokeEnd :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setStrokeEnd caShapeLayer  value =
  sendMsg caShapeLayer (mkSelector "setStrokeEnd:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineWidth@
lineWidth :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
lineWidth caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "lineWidth") retCDouble []

-- | @- setLineWidth:@
setLineWidth :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setLineWidth caShapeLayer  value =
  sendMsg caShapeLayer (mkSelector "setLineWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- miterLimit@
miterLimit :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
miterLimit caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "miterLimit") retCDouble []

-- | @- setMiterLimit:@
setMiterLimit :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setMiterLimit caShapeLayer  value =
  sendMsg caShapeLayer (mkSelector "setMiterLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineCap@
lineCap :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Id NSString)
lineCap caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "lineCap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineCap:@
setLineCap :: (IsCAShapeLayer caShapeLayer, IsNSString value) => caShapeLayer -> value -> IO ()
setLineCap caShapeLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caShapeLayer (mkSelector "setLineCap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lineJoin@
lineJoin :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Id NSString)
lineJoin caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "lineJoin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineJoin:@
setLineJoin :: (IsCAShapeLayer caShapeLayer, IsNSString value) => caShapeLayer -> value -> IO ()
setLineJoin caShapeLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caShapeLayer (mkSelector "setLineJoin:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lineDashPhase@
lineDashPhase :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO CDouble
lineDashPhase caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "lineDashPhase") retCDouble []

-- | @- setLineDashPhase:@
setLineDashPhase :: IsCAShapeLayer caShapeLayer => caShapeLayer -> CDouble -> IO ()
setLineDashPhase caShapeLayer  value =
  sendMsg caShapeLayer (mkSelector "setLineDashPhase:") retVoid [argCDouble (fromIntegral value)]

-- | @- lineDashPattern@
lineDashPattern :: IsCAShapeLayer caShapeLayer => caShapeLayer -> IO (Id NSArray)
lineDashPattern caShapeLayer  =
  sendMsg caShapeLayer (mkSelector "lineDashPattern") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineDashPattern:@
setLineDashPattern :: (IsCAShapeLayer caShapeLayer, IsNSArray value) => caShapeLayer -> value -> IO ()
setLineDashPattern caShapeLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caShapeLayer (mkSelector "setLineDashPattern:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @fillColor@
fillColorSelector :: Selector
fillColorSelector = mkSelector "fillColor"

-- | @Selector@ for @setFillColor:@
setFillColorSelector :: Selector
setFillColorSelector = mkSelector "setFillColor:"

-- | @Selector@ for @fillRule@
fillRuleSelector :: Selector
fillRuleSelector = mkSelector "fillRule"

-- | @Selector@ for @setFillRule:@
setFillRuleSelector :: Selector
setFillRuleSelector = mkSelector "setFillRule:"

-- | @Selector@ for @strokeColor@
strokeColorSelector :: Selector
strokeColorSelector = mkSelector "strokeColor"

-- | @Selector@ for @setStrokeColor:@
setStrokeColorSelector :: Selector
setStrokeColorSelector = mkSelector "setStrokeColor:"

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

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @miterLimit@
miterLimitSelector :: Selector
miterLimitSelector = mkSelector "miterLimit"

-- | @Selector@ for @setMiterLimit:@
setMiterLimitSelector :: Selector
setMiterLimitSelector = mkSelector "setMiterLimit:"

-- | @Selector@ for @lineCap@
lineCapSelector :: Selector
lineCapSelector = mkSelector "lineCap"

-- | @Selector@ for @setLineCap:@
setLineCapSelector :: Selector
setLineCapSelector = mkSelector "setLineCap:"

-- | @Selector@ for @lineJoin@
lineJoinSelector :: Selector
lineJoinSelector = mkSelector "lineJoin"

-- | @Selector@ for @setLineJoin:@
setLineJoinSelector :: Selector
setLineJoinSelector = mkSelector "setLineJoin:"

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

