{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , graphicsPort
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
  , attributesSelector
  , cgContextSelector
  , ciContextSelector
  , colorRenderingIntentSelector
  , compositingOperationSelector
  , currentContextDrawingToScreenSelector
  , currentContextSelector
  , drawingToScreenSelector
  , flippedSelector
  , flushGraphicsSelector
  , focusStackSelector
  , graphicsContextWithAttributesSelector
  , graphicsContextWithBitmapImageRepSelector
  , graphicsContextWithCGContext_flippedSelector
  , graphicsContextWithGraphicsPort_flippedSelector
  , graphicsContextWithWindowSelector
  , graphicsPortSelector
  , imageInterpolationSelector
  , nsGraphicsContextRestoreGraphicsStateSelector
  , nsGraphicsContextSaveGraphicsStateSelector
  , patternPhaseSelector
  , restoreGraphicsStateSelector
  , saveGraphicsStateSelector
  , setColorRenderingIntentSelector
  , setCompositingOperationSelector
  , setCurrentContextSelector
  , setFocusStackSelector
  , setGraphicsStateSelector
  , setImageInterpolationSelector
  , setPatternPhaseSelector
  , setShouldAntialiasSelector
  , shouldAntialiasSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' graphicsContextWithAttributesSelector (toNSDictionary attributes)

-- | @+ graphicsContextWithBitmapImageRep:@
graphicsContextWithBitmapImageRep :: IsNSBitmapImageRep bitmapRep => bitmapRep -> IO (Id NSGraphicsContext)
graphicsContextWithBitmapImageRep bitmapRep =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' graphicsContextWithBitmapImageRepSelector (toNSBitmapImageRep bitmapRep)

-- | @+ graphicsContextWithCGContext:flipped:@
graphicsContextWithCGContext_flipped :: Ptr () -> Bool -> IO (Id NSGraphicsContext)
graphicsContextWithCGContext_flipped graphicsPort initialFlippedState =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' graphicsContextWithCGContext_flippedSelector graphicsPort initialFlippedState

-- | @+ currentContextDrawingToScreen@
currentContextDrawingToScreen :: IO Bool
currentContextDrawingToScreen  =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' currentContextDrawingToScreenSelector

-- | @+ saveGraphicsState@
nsGraphicsContextSaveGraphicsState :: IO ()
nsGraphicsContextSaveGraphicsState  =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' nsGraphicsContextSaveGraphicsStateSelector

-- | @+ restoreGraphicsState@
nsGraphicsContextRestoreGraphicsState :: IO ()
nsGraphicsContextRestoreGraphicsState  =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' nsGraphicsContextRestoreGraphicsStateSelector

-- | @- saveGraphicsState@
saveGraphicsState :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO ()
saveGraphicsState nsGraphicsContext =
  sendMessage nsGraphicsContext saveGraphicsStateSelector

-- | @- restoreGraphicsState@
restoreGraphicsState :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO ()
restoreGraphicsState nsGraphicsContext =
  sendMessage nsGraphicsContext restoreGraphicsStateSelector

-- | @- flushGraphics@
flushGraphics :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO ()
flushGraphics nsGraphicsContext =
  sendMessage nsGraphicsContext flushGraphicsSelector

-- | @+ setGraphicsState:@
setGraphicsState :: CLong -> IO ()
setGraphicsState gState =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' setGraphicsStateSelector gState

-- | @- focusStack@
focusStack :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO RawId
focusStack nsGraphicsContext =
  sendMessage nsGraphicsContext focusStackSelector

-- | @- setFocusStack:@
setFocusStack :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> RawId -> IO ()
setFocusStack nsGraphicsContext stack =
  sendMessage nsGraphicsContext setFocusStackSelector stack

-- | @+ graphicsContextWithGraphicsPort:flipped:@
graphicsContextWithGraphicsPort_flipped :: Ptr () -> Bool -> IO (Id NSGraphicsContext)
graphicsContextWithGraphicsPort_flipped graphicsPort initialFlippedState =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' graphicsContextWithGraphicsPort_flippedSelector graphicsPort initialFlippedState

-- | @+ graphicsContextWithWindow:@
graphicsContextWithWindow :: IsNSWindow window => window -> IO (Id NSGraphicsContext)
graphicsContextWithWindow window =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' graphicsContextWithWindowSelector (toNSWindow window)

-- | @+ currentContext@
currentContext :: IO (Id NSGraphicsContext)
currentContext  =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' currentContextSelector

-- | @+ setCurrentContext:@
setCurrentContext :: IsNSGraphicsContext value => value -> IO ()
setCurrentContext value =
  do
    cls' <- getRequiredClass "NSGraphicsContext"
    sendClassMessage cls' setCurrentContextSelector (toNSGraphicsContext value)

-- | @- attributes@
attributes :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO (Id NSDictionary)
attributes nsGraphicsContext =
  sendMessage nsGraphicsContext attributesSelector

-- | @- drawingToScreen@
drawingToScreen :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO Bool
drawingToScreen nsGraphicsContext =
  sendMessage nsGraphicsContext drawingToScreenSelector

-- | @- CGContext@
cgContext :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO (Ptr ())
cgContext nsGraphicsContext =
  sendMessage nsGraphicsContext cgContextSelector

-- | @- flipped@
flipped :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO Bool
flipped nsGraphicsContext =
  sendMessage nsGraphicsContext flippedSelector

-- | @- graphicsPort@
graphicsPort :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO (Ptr ())
graphicsPort nsGraphicsContext =
  sendMessage nsGraphicsContext graphicsPortSelector

-- | @- CIContext@
ciContext :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO (Id CIContext)
ciContext nsGraphicsContext =
  sendMessage nsGraphicsContext ciContextSelector

-- | @- shouldAntialias@
shouldAntialias :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO Bool
shouldAntialias nsGraphicsContext =
  sendMessage nsGraphicsContext shouldAntialiasSelector

-- | @- setShouldAntialias:@
setShouldAntialias :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> Bool -> IO ()
setShouldAntialias nsGraphicsContext value =
  sendMessage nsGraphicsContext setShouldAntialiasSelector value

-- | @- imageInterpolation@
imageInterpolation :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO NSImageInterpolation
imageInterpolation nsGraphicsContext =
  sendMessage nsGraphicsContext imageInterpolationSelector

-- | @- setImageInterpolation:@
setImageInterpolation :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> NSImageInterpolation -> IO ()
setImageInterpolation nsGraphicsContext value =
  sendMessage nsGraphicsContext setImageInterpolationSelector value

-- | @- patternPhase@
patternPhase :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO NSPoint
patternPhase nsGraphicsContext =
  sendMessage nsGraphicsContext patternPhaseSelector

-- | @- setPatternPhase:@
setPatternPhase :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> NSPoint -> IO ()
setPatternPhase nsGraphicsContext value =
  sendMessage nsGraphicsContext setPatternPhaseSelector value

-- | @- compositingOperation@
compositingOperation :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO NSCompositingOperation
compositingOperation nsGraphicsContext =
  sendMessage nsGraphicsContext compositingOperationSelector

-- | @- setCompositingOperation:@
setCompositingOperation :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> NSCompositingOperation -> IO ()
setCompositingOperation nsGraphicsContext value =
  sendMessage nsGraphicsContext setCompositingOperationSelector value

-- | @- colorRenderingIntent@
colorRenderingIntent :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> IO NSColorRenderingIntent
colorRenderingIntent nsGraphicsContext =
  sendMessage nsGraphicsContext colorRenderingIntentSelector

-- | @- setColorRenderingIntent:@
setColorRenderingIntent :: IsNSGraphicsContext nsGraphicsContext => nsGraphicsContext -> NSColorRenderingIntent -> IO ()
setColorRenderingIntent nsGraphicsContext value =
  sendMessage nsGraphicsContext setColorRenderingIntentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graphicsContextWithAttributes:@
graphicsContextWithAttributesSelector :: Selector '[Id NSDictionary] (Id NSGraphicsContext)
graphicsContextWithAttributesSelector = mkSelector "graphicsContextWithAttributes:"

-- | @Selector@ for @graphicsContextWithBitmapImageRep:@
graphicsContextWithBitmapImageRepSelector :: Selector '[Id NSBitmapImageRep] (Id NSGraphicsContext)
graphicsContextWithBitmapImageRepSelector = mkSelector "graphicsContextWithBitmapImageRep:"

-- | @Selector@ for @graphicsContextWithCGContext:flipped:@
graphicsContextWithCGContext_flippedSelector :: Selector '[Ptr (), Bool] (Id NSGraphicsContext)
graphicsContextWithCGContext_flippedSelector = mkSelector "graphicsContextWithCGContext:flipped:"

-- | @Selector@ for @currentContextDrawingToScreen@
currentContextDrawingToScreenSelector :: Selector '[] Bool
currentContextDrawingToScreenSelector = mkSelector "currentContextDrawingToScreen"

-- | @Selector@ for @saveGraphicsState@
nsGraphicsContextSaveGraphicsStateSelector :: Selector '[] ()
nsGraphicsContextSaveGraphicsStateSelector = mkSelector "saveGraphicsState"

-- | @Selector@ for @restoreGraphicsState@
nsGraphicsContextRestoreGraphicsStateSelector :: Selector '[] ()
nsGraphicsContextRestoreGraphicsStateSelector = mkSelector "restoreGraphicsState"

-- | @Selector@ for @saveGraphicsState@
saveGraphicsStateSelector :: Selector '[] ()
saveGraphicsStateSelector = mkSelector "saveGraphicsState"

-- | @Selector@ for @restoreGraphicsState@
restoreGraphicsStateSelector :: Selector '[] ()
restoreGraphicsStateSelector = mkSelector "restoreGraphicsState"

-- | @Selector@ for @flushGraphics@
flushGraphicsSelector :: Selector '[] ()
flushGraphicsSelector = mkSelector "flushGraphics"

-- | @Selector@ for @setGraphicsState:@
setGraphicsStateSelector :: Selector '[CLong] ()
setGraphicsStateSelector = mkSelector "setGraphicsState:"

-- | @Selector@ for @focusStack@
focusStackSelector :: Selector '[] RawId
focusStackSelector = mkSelector "focusStack"

-- | @Selector@ for @setFocusStack:@
setFocusStackSelector :: Selector '[RawId] ()
setFocusStackSelector = mkSelector "setFocusStack:"

-- | @Selector@ for @graphicsContextWithGraphicsPort:flipped:@
graphicsContextWithGraphicsPort_flippedSelector :: Selector '[Ptr (), Bool] (Id NSGraphicsContext)
graphicsContextWithGraphicsPort_flippedSelector = mkSelector "graphicsContextWithGraphicsPort:flipped:"

-- | @Selector@ for @graphicsContextWithWindow:@
graphicsContextWithWindowSelector :: Selector '[Id NSWindow] (Id NSGraphicsContext)
graphicsContextWithWindowSelector = mkSelector "graphicsContextWithWindow:"

-- | @Selector@ for @currentContext@
currentContextSelector :: Selector '[] (Id NSGraphicsContext)
currentContextSelector = mkSelector "currentContext"

-- | @Selector@ for @setCurrentContext:@
setCurrentContextSelector :: Selector '[Id NSGraphicsContext] ()
setCurrentContextSelector = mkSelector "setCurrentContext:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSDictionary)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @drawingToScreen@
drawingToScreenSelector :: Selector '[] Bool
drawingToScreenSelector = mkSelector "drawingToScreen"

-- | @Selector@ for @CGContext@
cgContextSelector :: Selector '[] (Ptr ())
cgContextSelector = mkSelector "CGContext"

-- | @Selector@ for @flipped@
flippedSelector :: Selector '[] Bool
flippedSelector = mkSelector "flipped"

-- | @Selector@ for @graphicsPort@
graphicsPortSelector :: Selector '[] (Ptr ())
graphicsPortSelector = mkSelector "graphicsPort"

-- | @Selector@ for @CIContext@
ciContextSelector :: Selector '[] (Id CIContext)
ciContextSelector = mkSelector "CIContext"

-- | @Selector@ for @shouldAntialias@
shouldAntialiasSelector :: Selector '[] Bool
shouldAntialiasSelector = mkSelector "shouldAntialias"

-- | @Selector@ for @setShouldAntialias:@
setShouldAntialiasSelector :: Selector '[Bool] ()
setShouldAntialiasSelector = mkSelector "setShouldAntialias:"

-- | @Selector@ for @imageInterpolation@
imageInterpolationSelector :: Selector '[] NSImageInterpolation
imageInterpolationSelector = mkSelector "imageInterpolation"

-- | @Selector@ for @setImageInterpolation:@
setImageInterpolationSelector :: Selector '[NSImageInterpolation] ()
setImageInterpolationSelector = mkSelector "setImageInterpolation:"

-- | @Selector@ for @patternPhase@
patternPhaseSelector :: Selector '[] NSPoint
patternPhaseSelector = mkSelector "patternPhase"

-- | @Selector@ for @setPatternPhase:@
setPatternPhaseSelector :: Selector '[NSPoint] ()
setPatternPhaseSelector = mkSelector "setPatternPhase:"

-- | @Selector@ for @compositingOperation@
compositingOperationSelector :: Selector '[] NSCompositingOperation
compositingOperationSelector = mkSelector "compositingOperation"

-- | @Selector@ for @setCompositingOperation:@
setCompositingOperationSelector :: Selector '[NSCompositingOperation] ()
setCompositingOperationSelector = mkSelector "setCompositingOperation:"

-- | @Selector@ for @colorRenderingIntent@
colorRenderingIntentSelector :: Selector '[] NSColorRenderingIntent
colorRenderingIntentSelector = mkSelector "colorRenderingIntent"

-- | @Selector@ for @setColorRenderingIntent:@
setColorRenderingIntentSelector :: Selector '[NSColorRenderingIntent] ()
setColorRenderingIntentSelector = mkSelector "setColorRenderingIntent:"

