{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCView@.
module ObjC.Quartz.QCView
  ( QCView
  , IsQCView(..)
  , loadCompositionFromFile
  , loadComposition
  , loadedComposition
  , unloadComposition
  , setAutostartsRendering
  , autostartsRendering
  , setEraseColor
  , eraseColor
  , setEventForwardingMask
  , eventForwardingMask
  , setMaxRenderingFrameRate
  , maxRenderingFrameRate
  , erase
  , startRendering
  , renderAtTime_arguments
  , pauseRendering
  , isPausedRendering
  , resumeRendering
  , stopRendering
  , isRendering
  , snapshotImage
  , createSnapshotImageOfType
  , openGLContext
  , openGLPixelFormat
  , start
  , stop
  , play
  , autostartsRenderingSelector
  , createSnapshotImageOfTypeSelector
  , eraseColorSelector
  , eraseSelector
  , eventForwardingMaskSelector
  , isPausedRenderingSelector
  , isRenderingSelector
  , loadCompositionFromFileSelector
  , loadCompositionSelector
  , loadedCompositionSelector
  , maxRenderingFrameRateSelector
  , openGLContextSelector
  , openGLPixelFormatSelector
  , pauseRenderingSelector
  , playSelector
  , renderAtTime_argumentsSelector
  , resumeRenderingSelector
  , setAutostartsRenderingSelector
  , setEraseColorSelector
  , setEventForwardingMaskSelector
  , setMaxRenderingFrameRateSelector
  , snapshotImageSelector
  , startRenderingSelector
  , startSelector
  , stopRenderingSelector
  , stopSelector
  , unloadCompositionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- loadCompositionFromFile:@
loadCompositionFromFile :: (IsQCView qcView, IsNSString path) => qcView -> path -> IO Bool
loadCompositionFromFile qcView path =
  sendMessage qcView loadCompositionFromFileSelector (toNSString path)

-- | @- loadComposition:@
loadComposition :: (IsQCView qcView, IsQCComposition composition) => qcView -> composition -> IO Bool
loadComposition qcView composition =
  sendMessage qcView loadCompositionSelector (toQCComposition composition)

-- | @- loadedComposition@
loadedComposition :: IsQCView qcView => qcView -> IO (Id QCComposition)
loadedComposition qcView =
  sendMessage qcView loadedCompositionSelector

-- | @- unloadComposition@
unloadComposition :: IsQCView qcView => qcView -> IO ()
unloadComposition qcView =
  sendMessage qcView unloadCompositionSelector

-- | @- setAutostartsRendering:@
setAutostartsRendering :: IsQCView qcView => qcView -> Bool -> IO ()
setAutostartsRendering qcView flag =
  sendMessage qcView setAutostartsRenderingSelector flag

-- | @- autostartsRendering@
autostartsRendering :: IsQCView qcView => qcView -> IO Bool
autostartsRendering qcView =
  sendMessage qcView autostartsRenderingSelector

-- | @- setEraseColor:@
setEraseColor :: (IsQCView qcView, IsNSColor color) => qcView -> color -> IO ()
setEraseColor qcView color =
  sendMessage qcView setEraseColorSelector (toNSColor color)

-- | @- eraseColor@
eraseColor :: IsQCView qcView => qcView -> IO (Id NSColor)
eraseColor qcView =
  sendMessage qcView eraseColorSelector

-- | @- setEventForwardingMask:@
setEventForwardingMask :: IsQCView qcView => qcView -> CULong -> IO ()
setEventForwardingMask qcView mask =
  sendMessage qcView setEventForwardingMaskSelector mask

-- | @- eventForwardingMask@
eventForwardingMask :: IsQCView qcView => qcView -> IO CULong
eventForwardingMask qcView =
  sendMessage qcView eventForwardingMaskSelector

-- | @- setMaxRenderingFrameRate:@
setMaxRenderingFrameRate :: IsQCView qcView => qcView -> CFloat -> IO ()
setMaxRenderingFrameRate qcView maxFPS =
  sendMessage qcView setMaxRenderingFrameRateSelector maxFPS

-- | @- maxRenderingFrameRate@
maxRenderingFrameRate :: IsQCView qcView => qcView -> IO CFloat
maxRenderingFrameRate qcView =
  sendMessage qcView maxRenderingFrameRateSelector

-- | @- erase@
erase :: IsQCView qcView => qcView -> IO ()
erase qcView =
  sendMessage qcView eraseSelector

-- | @- startRendering@
startRendering :: IsQCView qcView => qcView -> IO Bool
startRendering qcView =
  sendMessage qcView startRenderingSelector

-- | @- renderAtTime:arguments:@
renderAtTime_arguments :: (IsQCView qcView, IsNSDictionary arguments) => qcView -> CDouble -> arguments -> IO Bool
renderAtTime_arguments qcView time arguments =
  sendMessage qcView renderAtTime_argumentsSelector time (toNSDictionary arguments)

-- | @- pauseRendering@
pauseRendering :: IsQCView qcView => qcView -> IO ()
pauseRendering qcView =
  sendMessage qcView pauseRenderingSelector

-- | @- isPausedRendering@
isPausedRendering :: IsQCView qcView => qcView -> IO Bool
isPausedRendering qcView =
  sendMessage qcView isPausedRenderingSelector

-- | @- resumeRendering@
resumeRendering :: IsQCView qcView => qcView -> IO ()
resumeRendering qcView =
  sendMessage qcView resumeRenderingSelector

-- | @- stopRendering@
stopRendering :: IsQCView qcView => qcView -> IO ()
stopRendering qcView =
  sendMessage qcView stopRenderingSelector

-- | @- isRendering@
isRendering :: IsQCView qcView => qcView -> IO Bool
isRendering qcView =
  sendMessage qcView isRenderingSelector

-- | @- snapshotImage@
snapshotImage :: IsQCView qcView => qcView -> IO (Id NSImage)
snapshotImage qcView =
  sendMessage qcView snapshotImageSelector

-- | @- createSnapshotImageOfType:@
createSnapshotImageOfType :: (IsQCView qcView, IsNSString type_) => qcView -> type_ -> IO RawId
createSnapshotImageOfType qcView type_ =
  sendMessage qcView createSnapshotImageOfTypeSelector (toNSString type_)

-- | @- openGLContext@
openGLContext :: IsQCView qcView => qcView -> IO (Id NSOpenGLContext)
openGLContext qcView =
  sendMessage qcView openGLContextSelector

-- | @- openGLPixelFormat@
openGLPixelFormat :: IsQCView qcView => qcView -> IO (Id NSOpenGLPixelFormat)
openGLPixelFormat qcView =
  sendMessage qcView openGLPixelFormatSelector

-- | @- start:@
start :: IsQCView qcView => qcView -> RawId -> IO ()
start qcView sender =
  sendMessage qcView startSelector sender

-- | @- stop:@
stop :: IsQCView qcView => qcView -> RawId -> IO ()
stop qcView sender =
  sendMessage qcView stopSelector sender

-- | @- play:@
play :: IsQCView qcView => qcView -> RawId -> IO ()
play qcView sender =
  sendMessage qcView playSelector sender

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadCompositionFromFile:@
loadCompositionFromFileSelector :: Selector '[Id NSString] Bool
loadCompositionFromFileSelector = mkSelector "loadCompositionFromFile:"

-- | @Selector@ for @loadComposition:@
loadCompositionSelector :: Selector '[Id QCComposition] Bool
loadCompositionSelector = mkSelector "loadComposition:"

-- | @Selector@ for @loadedComposition@
loadedCompositionSelector :: Selector '[] (Id QCComposition)
loadedCompositionSelector = mkSelector "loadedComposition"

-- | @Selector@ for @unloadComposition@
unloadCompositionSelector :: Selector '[] ()
unloadCompositionSelector = mkSelector "unloadComposition"

-- | @Selector@ for @setAutostartsRendering:@
setAutostartsRenderingSelector :: Selector '[Bool] ()
setAutostartsRenderingSelector = mkSelector "setAutostartsRendering:"

-- | @Selector@ for @autostartsRendering@
autostartsRenderingSelector :: Selector '[] Bool
autostartsRenderingSelector = mkSelector "autostartsRendering"

-- | @Selector@ for @setEraseColor:@
setEraseColorSelector :: Selector '[Id NSColor] ()
setEraseColorSelector = mkSelector "setEraseColor:"

-- | @Selector@ for @eraseColor@
eraseColorSelector :: Selector '[] (Id NSColor)
eraseColorSelector = mkSelector "eraseColor"

-- | @Selector@ for @setEventForwardingMask:@
setEventForwardingMaskSelector :: Selector '[CULong] ()
setEventForwardingMaskSelector = mkSelector "setEventForwardingMask:"

-- | @Selector@ for @eventForwardingMask@
eventForwardingMaskSelector :: Selector '[] CULong
eventForwardingMaskSelector = mkSelector "eventForwardingMask"

-- | @Selector@ for @setMaxRenderingFrameRate:@
setMaxRenderingFrameRateSelector :: Selector '[CFloat] ()
setMaxRenderingFrameRateSelector = mkSelector "setMaxRenderingFrameRate:"

-- | @Selector@ for @maxRenderingFrameRate@
maxRenderingFrameRateSelector :: Selector '[] CFloat
maxRenderingFrameRateSelector = mkSelector "maxRenderingFrameRate"

-- | @Selector@ for @erase@
eraseSelector :: Selector '[] ()
eraseSelector = mkSelector "erase"

-- | @Selector@ for @startRendering@
startRenderingSelector :: Selector '[] Bool
startRenderingSelector = mkSelector "startRendering"

-- | @Selector@ for @renderAtTime:arguments:@
renderAtTime_argumentsSelector :: Selector '[CDouble, Id NSDictionary] Bool
renderAtTime_argumentsSelector = mkSelector "renderAtTime:arguments:"

-- | @Selector@ for @pauseRendering@
pauseRenderingSelector :: Selector '[] ()
pauseRenderingSelector = mkSelector "pauseRendering"

-- | @Selector@ for @isPausedRendering@
isPausedRenderingSelector :: Selector '[] Bool
isPausedRenderingSelector = mkSelector "isPausedRendering"

-- | @Selector@ for @resumeRendering@
resumeRenderingSelector :: Selector '[] ()
resumeRenderingSelector = mkSelector "resumeRendering"

-- | @Selector@ for @stopRendering@
stopRenderingSelector :: Selector '[] ()
stopRenderingSelector = mkSelector "stopRendering"

-- | @Selector@ for @isRendering@
isRenderingSelector :: Selector '[] Bool
isRenderingSelector = mkSelector "isRendering"

-- | @Selector@ for @snapshotImage@
snapshotImageSelector :: Selector '[] (Id NSImage)
snapshotImageSelector = mkSelector "snapshotImage"

-- | @Selector@ for @createSnapshotImageOfType:@
createSnapshotImageOfTypeSelector :: Selector '[Id NSString] RawId
createSnapshotImageOfTypeSelector = mkSelector "createSnapshotImageOfType:"

-- | @Selector@ for @openGLContext@
openGLContextSelector :: Selector '[] (Id NSOpenGLContext)
openGLContextSelector = mkSelector "openGLContext"

-- | @Selector@ for @openGLPixelFormat@
openGLPixelFormatSelector :: Selector '[] (Id NSOpenGLPixelFormat)
openGLPixelFormatSelector = mkSelector "openGLPixelFormat"

-- | @Selector@ for @start:@
startSelector :: Selector '[RawId] ()
startSelector = mkSelector "start:"

-- | @Selector@ for @stop:@
stopSelector :: Selector '[RawId] ()
stopSelector = mkSelector "stop:"

-- | @Selector@ for @play:@
playSelector :: Selector '[RawId] ()
playSelector = mkSelector "play:"

