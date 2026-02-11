{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGraphicsContext@.
module ObjC.AppKit.NSGraphicsContext
  ( NSGraphicsContext
  , IsNSGraphicsContext(..)
  , graphicsContextWithAttributes
  , graphicsContextWithBitmapImageRep
  , graphicsContextWithCGContext_flipped
  , currentContextDrawingToScreen
  , nsGraphicsContextSaveGraphicsState
  , nsGraphicsContextRestoreGraphicsState
  , saveGraphicsState
  , restoreGraphicsState
  , flushGraphics
  , setGraphicsState
  , focusStack
  , setFocusStack
  , graphicsContextWithGraphicsPort_flipped
  , graphicsContextWithWindow
  , currentContext
  , setCurrentContext
  , attributes
  , drawingToScreen
  , cgContext
  , flipped
  , ciContext
  , shouldAntialias
  , setShouldAntialias
  , imageInterpolation
  , setImageInterpolation
  , patternPhase
  , setPatternPhase
  , compositingOperation
  , setCompositingOperation
  , colorRenderingIntent
  , setColorRenderingIntent
  , graphicsContextWithAttributesSelector
  , graphicsContextWithBitmapImageRepSelector
  , graphicsContextWithCGContext_flippedSelector
  , currentContextDrawingToScreenSelector
  , saveGraphicsStateSelector
  , restoreGraphicsStateSelector
  , flushGraphicsSelector
  , setGraphicsStateSelector
  , focusStackSelector
  , setFocusStackSelector
  , graphicsContextWithGraphicsPort_flippedSelector
  , graphicsContextWithWindowSelector
  , currentContextSelector
  , setCurrentContextSelector
  , attributesSelector
  , drawingToScreenSelector
  , cgContextSelector
  , flippedSelector
  , ciContextSelector
  , shouldAntialiasSelector
  , setShouldAntialiasSelector
  , imageInterpolationSelector
  , setImageInterpolationSelector
  , patternPhaseSelector
  , setPatternPhaseSelector
  , compositingOperationSelector
  , setCompositingOperationSelector
  , colorRenderingIntentSelector
  , setColorRenderingIntentSelector

  -- * Enum types
  , NSColorRenderingIntent(NSColorRenderingIntent)
  , pattern NSColorRenderingIntentDefault
  , pattern NSColorRenderingIntentAbsoluteColorimetric
  , pattern NSColorRenderingIntentRelativeColorimetric
  , pattern NSColorRenderingIntentPerceptual
  , pattern NSColorRenderingIntentSaturation
  , NSCompositingOperation(NSCompositingOperation)
  , pattern NSCompositingOperationClear
  , pattern NSCompositingOperationCopy
  , pattern NSCompositingOperationSourceOver
  , pattern NSCompositingOperationSourceIn
  , pattern NSCompositingOperationSourceOut
  , pattern NSCompositingOperationSourceAtop
  , pattern NSCompositingOperationDestinationOver
  , pattern NSCompositingOperationDestinationIn
  , pattern NSCompositingOperationDestinationOut
  , pattern NSCompositingOperationDestinationAtop
  , pattern NSCompositingOperationXOR
  , pattern NSCompositingOperationPlusDarker
  , pattern NSCompositingOperationHighlight
  , pattern NSCompositingOperationPlusLighter
  , pattern NSCompositingOperationMultiply
  , pattern NSCompositingOperationScreen
  , pattern NSCompositingOperationOverlay
  , pattern NSCompositingOperationDarken
  , pattern NSCompositingOperationLighten
  , pattern NSCompositingOperationColorDodge
  , pattern NSCompositingOperationColorBurn
  , pattern NSCompositingOperationSoftLight
  , pattern NSCompositingOperationHardLight
  , pattern NSCompositingOperationDifference
  , pattern NSCompositingOperationExclusion
  , pattern NSCompositingOperationHue
  , pattern NSCompositingOperationSaturation
  , pattern NSCompositingOperationColor
  , pattern NSCompositingOperationLuminosity
  , NSImageInterpolation(NSImageInterpolation)
  , pattern NSImageInterpolationDefault
  , pattern NSImageInterpolationNone
  , pattern NSImageInterpolationLow
  , pattern NSImageInterpolationMedium
  , pattern NSImageInterpolationHigh

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ graphicsContextWithAttributes:@
graphicsContextWithAttributes :: IsNSDictionary attributes => attributes -> IO (Id NSGraphicsContext)
graphicsContextWithAttributes attributes =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    withObjCPtr attributes $ \raw_attributes ->
      sendClassMsg cls' (mkSelector "graphicsContextWithAttributes:") (retPtr retVoid) [argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

-- | @+ graphicsContextWithBitmapImageRep:@
graphicsContextWithBitmapImageRep :: IsNSBitmapImageRep bitmapRep => bitmapRep -> IO (Id NSGraphicsContext)
graphicsContextWithBitmapImageRep bitmapRep =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    withObjCPtr bitmapRep $ \raw_bitmapRep ->
      sendClassMsg cls' (mkSelector "graphicsContextWithBitmapImageRep:") (retPtr retVoid) [argPtr (castPtr raw_bitmapRep :: Ptr ())] >>= retainedObject . castPtr

-- | @+ graphicsContextWithCGContext:flipped:@
graphicsContextWithCGContext_flipped :: Ptr () -> Bool -> IO (Id NSGraphicsContext)
graphicsContextWithCGContext_flipped graphicsPort initialFlippedState =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMsg cls' (mkSelector "graphicsContextWithCGContext:flipped:") (retPtr retVoid) [argPtr graphicsPort, argCULong (if initialFlippedState then 1 else 0)] >>= retainedObject . castPtr

-- | @+ currentContextDrawingToScreen@
currentContextDrawingToScreen :: IO Bool
currentContextDrawingToScreen  =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "currentContextDrawingToScreen") retCULong []

-- | @+ saveGraphicsState@
nsGraphicsContextSaveGraphicsState :: IO ()
nsGraphicsContextSaveGraphicsState  =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMsg cls' (mkSelector "saveGraphicsState") retVoid []

-- | @+ restoreGraphicsState@
nsGraphicsContextRestoreGraphicsState :: IO ()
nsGraphicsContextRestoreGraphicsState  =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMsg cls' (mkSelector "restoreGraphicsState") retVoid []

-- | @- saveGraphicsState@
saveGraphicsState :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO ()
saveGraphicsState nsGraphicsContext  =
  sendMsg nsGraphicsContext (mkSelector "saveGraphicsState") retVoid []

-- | @- restoreGraphicsState@
restoreGraphicsState :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO ()
restoreGraphicsState nsGraphicsContext  =
  sendMsg nsGraphicsContext (mkSelector "restoreGraphicsState") retVoid []

-- | @- flushGraphics@
flushGraphics :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO ()
flushGraphics nsGraphicsContext  =
  sendMsg nsGraphicsContext (mkSelector "flushGraphics") retVoid []

-- | @+ setGraphicsState:@
setGraphicsState :: CLong -> IO ()
setGraphicsState gState =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMsg cls' (mkSelector "setGraphicsState:") retVoid [argCLong (fromIntegral gState)]

-- | @- focusStack@
focusStack :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO RawId
focusStack nsGraphicsContext  =
  fmap (RawId . castPtr) $ sendMsg nsGraphicsContext (mkSelector "focusStack") (retPtr retVoid) []

-- | @- setFocusStack:@
setFocusStack :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> RawId -> IO ()
setFocusStack nsGraphicsContext  stack =
  sendMsg nsGraphicsContext (mkSelector "setFocusStack:") retVoid [argPtr (castPtr (unRawId stack) :: Ptr ())]

-- | @+ graphicsContextWithGraphicsPort:flipped:@
graphicsContextWithGraphicsPort_flipped :: Ptr () -> Bool -> IO (Id NSGraphicsContext)
graphicsContextWithGraphicsPort_flipped graphicsPort initialFlippedState =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMsg cls' (mkSelector "graphicsContextWithGraphicsPort:flipped:") (retPtr retVoid) [argPtr graphicsPort, argCULong (if initialFlippedState then 1 else 0)] >>= retainedObject . castPtr

-- | @+ graphicsContextWithWindow:@
graphicsContextWithWindow :: IsNSWindow window => window -> IO (Id NSGraphicsContext)
graphicsContextWithWindow window =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    withObjCPtr window $ \raw_window ->
      sendClassMsg cls' (mkSelector "graphicsContextWithWindow:") (retPtr retVoid) [argPtr (castPtr raw_window :: Ptr ())] >>= retainedObject . castPtr

-- | @+ currentContext@
currentContext :: IO (Id NSGraphicsContext)
currentContext  =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMsg cls' (mkSelector "currentContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setCurrentContext:@
setCurrentContext :: IsNSGraphicsContext value => value -> IO ()
setCurrentContext value =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "setCurrentContext:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributes@
attributes :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO (Id NSDictionary)
attributes nsGraphicsContext  =
  sendMsg nsGraphicsContext (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- drawingToScreen@
drawingToScreen :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO Bool
drawingToScreen nsGraphicsContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGraphicsContext (mkSelector "drawingToScreen") retCULong []

-- | @- CGContext@
cgContext :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO (Ptr ())
cgContext nsGraphicsContext  =
  fmap castPtr $ sendMsg nsGraphicsContext (mkSelector "CGContext") (retPtr retVoid) []

-- | @- flipped@
flipped :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO Bool
flipped nsGraphicsContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGraphicsContext (mkSelector "flipped") retCULong []

-- | @- CIContext@
ciContext :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO (Id CIContext)
ciContext nsGraphicsContext  =
  sendMsg nsGraphicsContext (mkSelector "CIContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shouldAntialias@
shouldAntialias :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO Bool
shouldAntialias nsGraphicsContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGraphicsContext (mkSelector "shouldAntialias") retCULong []

-- | @- setShouldAntialias:@
setShouldAntialias :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> Bool -> IO ()
setShouldAntialias nsGraphicsContext  value =
  sendMsg nsGraphicsContext (mkSelector "setShouldAntialias:") retVoid [argCULong (if value then 1 else 0)]

-- | @- imageInterpolation@
imageInterpolation :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO NSImageInterpolation
imageInterpolation nsGraphicsContext  =
  fmap (coerce :: CULong -> NSImageInterpolation) $ sendMsg nsGraphicsContext (mkSelector "imageInterpolation") retCULong []

-- | @- setImageInterpolation:@
setImageInterpolation :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> NSImageInterpolation -> IO ()
setImageInterpolation nsGraphicsContext  value =
  sendMsg nsGraphicsContext (mkSelector "setImageInterpolation:") retVoid [argCULong (coerce value)]

-- | @- patternPhase@
patternPhase :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO NSPoint
patternPhase nsGraphicsContext  =
  sendMsgStret nsGraphicsContext (mkSelector "patternPhase") retNSPoint []

-- | @- setPatternPhase:@
setPatternPhase :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> NSPoint -> IO ()
setPatternPhase nsGraphicsContext  value =
  sendMsg nsGraphicsContext (mkSelector "setPatternPhase:") retVoid [argNSPoint value]

-- | @- compositingOperation@
compositingOperation :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO NSCompositingOperation
compositingOperation nsGraphicsContext  =
  fmap (coerce :: CULong -> NSCompositingOperation) $ sendMsg nsGraphicsContext (mkSelector "compositingOperation") retCULong []

-- | @- setCompositingOperation:@
setCompositingOperation :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> NSCompositingOperation -> IO ()
setCompositingOperation nsGraphicsContext  value =
  sendMsg nsGraphicsContext (mkSelector "setCompositingOperation:") retVoid [argCULong (coerce value)]

-- | @- colorRenderingIntent@
colorRenderingIntent :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO NSColorRenderingIntent
colorRenderingIntent nsGraphicsContext  =
  fmap (coerce :: CLong -> NSColorRenderingIntent) $ sendMsg nsGraphicsContext (mkSelector "colorRenderingIntent") retCLong []

-- | @- setColorRenderingIntent:@
setColorRenderingIntent :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> NSColorRenderingIntent -> IO ()
setColorRenderingIntent nsGraphicsContext  value =
  sendMsg nsGraphicsContext (mkSelector "setColorRenderingIntent:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graphicsContextWithAttributes:@
graphicsContextWithAttributesSelector :: Selector
graphicsContextWithAttributesSelector = mkSelector "graphicsContextWithAttributes:"

-- | @Selector@ for @graphicsContextWithBitmapImageRep:@
graphicsContextWithBitmapImageRepSelector :: Selector
graphicsContextWithBitmapImageRepSelector = mkSelector "graphicsContextWithBitmapImageRep:"

-- | @Selector@ for @graphicsContextWithCGContext:flipped:@
graphicsContextWithCGContext_flippedSelector :: Selector
graphicsContextWithCGContext_flippedSelector = mkSelector "graphicsContextWithCGContext:flipped:"

-- | @Selector@ for @currentContextDrawingToScreen@
currentContextDrawingToScreenSelector :: Selector
currentContextDrawingToScreenSelector = mkSelector "currentContextDrawingToScreen"

-- | @Selector@ for @saveGraphicsState@
saveGraphicsStateSelector :: Selector
saveGraphicsStateSelector = mkSelector "saveGraphicsState"

-- | @Selector@ for @restoreGraphicsState@
restoreGraphicsStateSelector :: Selector
restoreGraphicsStateSelector = mkSelector "restoreGraphicsState"

-- | @Selector@ for @flushGraphics@
flushGraphicsSelector :: Selector
flushGraphicsSelector = mkSelector "flushGraphics"

-- | @Selector@ for @setGraphicsState:@
setGraphicsStateSelector :: Selector
setGraphicsStateSelector = mkSelector "setGraphicsState:"

-- | @Selector@ for @focusStack@
focusStackSelector :: Selector
focusStackSelector = mkSelector "focusStack"

-- | @Selector@ for @setFocusStack:@
setFocusStackSelector :: Selector
setFocusStackSelector = mkSelector "setFocusStack:"

-- | @Selector@ for @graphicsContextWithGraphicsPort:flipped:@
graphicsContextWithGraphicsPort_flippedSelector :: Selector
graphicsContextWithGraphicsPort_flippedSelector = mkSelector "graphicsContextWithGraphicsPort:flipped:"

-- | @Selector@ for @graphicsContextWithWindow:@
graphicsContextWithWindowSelector :: Selector
graphicsContextWithWindowSelector = mkSelector "graphicsContextWithWindow:"

-- | @Selector@ for @currentContext@
currentContextSelector :: Selector
currentContextSelector = mkSelector "currentContext"

-- | @Selector@ for @setCurrentContext:@
setCurrentContextSelector :: Selector
setCurrentContextSelector = mkSelector "setCurrentContext:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @drawingToScreen@
drawingToScreenSelector :: Selector
drawingToScreenSelector = mkSelector "drawingToScreen"

-- | @Selector@ for @CGContext@
cgContextSelector :: Selector
cgContextSelector = mkSelector "CGContext"

-- | @Selector@ for @flipped@
flippedSelector :: Selector
flippedSelector = mkSelector "flipped"

-- | @Selector@ for @CIContext@
ciContextSelector :: Selector
ciContextSelector = mkSelector "CIContext"

-- | @Selector@ for @shouldAntialias@
shouldAntialiasSelector :: Selector
shouldAntialiasSelector = mkSelector "shouldAntialias"

-- | @Selector@ for @setShouldAntialias:@
setShouldAntialiasSelector :: Selector
setShouldAntialiasSelector = mkSelector "setShouldAntialias:"

-- | @Selector@ for @imageInterpolation@
imageInterpolationSelector :: Selector
imageInterpolationSelector = mkSelector "imageInterpolation"

-- | @Selector@ for @setImageInterpolation:@
setImageInterpolationSelector :: Selector
setImageInterpolationSelector = mkSelector "setImageInterpolation:"

-- | @Selector@ for @patternPhase@
patternPhaseSelector :: Selector
patternPhaseSelector = mkSelector "patternPhase"

-- | @Selector@ for @setPatternPhase:@
setPatternPhaseSelector :: Selector
setPatternPhaseSelector = mkSelector "setPatternPhase:"

-- | @Selector@ for @compositingOperation@
compositingOperationSelector :: Selector
compositingOperationSelector = mkSelector "compositingOperation"

-- | @Selector@ for @setCompositingOperation:@
setCompositingOperationSelector :: Selector
setCompositingOperationSelector = mkSelector "setCompositingOperation:"

-- | @Selector@ for @colorRenderingIntent@
colorRenderingIntentSelector :: Selector
colorRenderingIntentSelector = mkSelector "colorRenderingIntent"

-- | @Selector@ for @setColorRenderingIntent:@
setColorRenderingIntentSelector :: Selector
setColorRenderingIntentSelector = mkSelector "setColorRenderingIntent:"

