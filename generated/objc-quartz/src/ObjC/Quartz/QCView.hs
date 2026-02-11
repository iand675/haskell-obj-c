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
  , loadCompositionFromFileSelector
  , loadCompositionSelector
  , loadedCompositionSelector
  , unloadCompositionSelector
  , setAutostartsRenderingSelector
  , autostartsRenderingSelector
  , setEraseColorSelector
  , eraseColorSelector
  , setEventForwardingMaskSelector
  , eventForwardingMaskSelector
  , setMaxRenderingFrameRateSelector
  , maxRenderingFrameRateSelector
  , eraseSelector
  , startRenderingSelector
  , renderAtTime_argumentsSelector
  , pauseRenderingSelector
  , isPausedRenderingSelector
  , resumeRenderingSelector
  , stopRenderingSelector
  , isRenderingSelector
  , snapshotImageSelector
  , createSnapshotImageOfTypeSelector
  , openGLContextSelector
  , openGLPixelFormatSelector
  , startSelector
  , stopSelector
  , playSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- loadCompositionFromFile:@
loadCompositionFromFile :: (IsQCView qcView, IsNSString path) => qcView -> path -> IO Bool
loadCompositionFromFile qcView  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcView (mkSelector "loadCompositionFromFile:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | @- loadComposition:@
loadComposition :: (IsQCView qcView, IsQCComposition composition) => qcView -> composition -> IO Bool
loadComposition qcView  composition =
withObjCPtr composition $ \raw_composition ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcView (mkSelector "loadComposition:") retCULong [argPtr (castPtr raw_composition :: Ptr ())]

-- | @- loadedComposition@
loadedComposition :: IsQCView qcView => qcView -> IO (Id QCComposition)
loadedComposition qcView  =
  sendMsg qcView (mkSelector "loadedComposition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- unloadComposition@
unloadComposition :: IsQCView qcView => qcView -> IO ()
unloadComposition qcView  =
  sendMsg qcView (mkSelector "unloadComposition") retVoid []

-- | @- setAutostartsRendering:@
setAutostartsRendering :: IsQCView qcView => qcView -> Bool -> IO ()
setAutostartsRendering qcView  flag =
  sendMsg qcView (mkSelector "setAutostartsRendering:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- autostartsRendering@
autostartsRendering :: IsQCView qcView => qcView -> IO Bool
autostartsRendering qcView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcView (mkSelector "autostartsRendering") retCULong []

-- | @- setEraseColor:@
setEraseColor :: (IsQCView qcView, IsNSColor color) => qcView -> color -> IO ()
setEraseColor qcView  color =
withObjCPtr color $ \raw_color ->
    sendMsg qcView (mkSelector "setEraseColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- eraseColor@
eraseColor :: IsQCView qcView => qcView -> IO (Id NSColor)
eraseColor qcView  =
  sendMsg qcView (mkSelector "eraseColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEventForwardingMask:@
setEventForwardingMask :: IsQCView qcView => qcView -> CULong -> IO ()
setEventForwardingMask qcView  mask =
  sendMsg qcView (mkSelector "setEventForwardingMask:") retVoid [argCULong (fromIntegral mask)]

-- | @- eventForwardingMask@
eventForwardingMask :: IsQCView qcView => qcView -> IO CULong
eventForwardingMask qcView  =
  sendMsg qcView (mkSelector "eventForwardingMask") retCULong []

-- | @- setMaxRenderingFrameRate:@
setMaxRenderingFrameRate :: IsQCView qcView => qcView -> CFloat -> IO ()
setMaxRenderingFrameRate qcView  maxFPS =
  sendMsg qcView (mkSelector "setMaxRenderingFrameRate:") retVoid [argCFloat (fromIntegral maxFPS)]

-- | @- maxRenderingFrameRate@
maxRenderingFrameRate :: IsQCView qcView => qcView -> IO CFloat
maxRenderingFrameRate qcView  =
  sendMsg qcView (mkSelector "maxRenderingFrameRate") retCFloat []

-- | @- erase@
erase :: IsQCView qcView => qcView -> IO ()
erase qcView  =
  sendMsg qcView (mkSelector "erase") retVoid []

-- | @- startRendering@
startRendering :: IsQCView qcView => qcView -> IO Bool
startRendering qcView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcView (mkSelector "startRendering") retCULong []

-- | @- renderAtTime:arguments:@
renderAtTime_arguments :: (IsQCView qcView, IsNSDictionary arguments) => qcView -> CDouble -> arguments -> IO Bool
renderAtTime_arguments qcView  time arguments =
withObjCPtr arguments $ \raw_arguments ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcView (mkSelector "renderAtTime:arguments:") retCULong [argCDouble (fromIntegral time), argPtr (castPtr raw_arguments :: Ptr ())]

-- | @- pauseRendering@
pauseRendering :: IsQCView qcView => qcView -> IO ()
pauseRendering qcView  =
  sendMsg qcView (mkSelector "pauseRendering") retVoid []

-- | @- isPausedRendering@
isPausedRendering :: IsQCView qcView => qcView -> IO Bool
isPausedRendering qcView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcView (mkSelector "isPausedRendering") retCULong []

-- | @- resumeRendering@
resumeRendering :: IsQCView qcView => qcView -> IO ()
resumeRendering qcView  =
  sendMsg qcView (mkSelector "resumeRendering") retVoid []

-- | @- stopRendering@
stopRendering :: IsQCView qcView => qcView -> IO ()
stopRendering qcView  =
  sendMsg qcView (mkSelector "stopRendering") retVoid []

-- | @- isRendering@
isRendering :: IsQCView qcView => qcView -> IO Bool
isRendering qcView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcView (mkSelector "isRendering") retCULong []

-- | @- snapshotImage@
snapshotImage :: IsQCView qcView => qcView -> IO (Id NSImage)
snapshotImage qcView  =
  sendMsg qcView (mkSelector "snapshotImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- createSnapshotImageOfType:@
createSnapshotImageOfType :: (IsQCView qcView, IsNSString type_) => qcView -> type_ -> IO RawId
createSnapshotImageOfType qcView  type_ =
withObjCPtr type_ $ \raw_type_ ->
    fmap (RawId . castPtr) $ sendMsg qcView (mkSelector "createSnapshotImageOfType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- openGLContext@
openGLContext :: IsQCView qcView => qcView -> IO (Id NSOpenGLContext)
openGLContext qcView  =
  sendMsg qcView (mkSelector "openGLContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- openGLPixelFormat@
openGLPixelFormat :: IsQCView qcView => qcView -> IO (Id NSOpenGLPixelFormat)
openGLPixelFormat qcView  =
  sendMsg qcView (mkSelector "openGLPixelFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- start:@
start :: IsQCView qcView => qcView -> RawId -> IO ()
start qcView  sender =
  sendMsg qcView (mkSelector "start:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stop:@
stop :: IsQCView qcView => qcView -> RawId -> IO ()
stop qcView  sender =
  sendMsg qcView (mkSelector "stop:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- play:@
play :: IsQCView qcView => qcView -> RawId -> IO ()
play qcView  sender =
  sendMsg qcView (mkSelector "play:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadCompositionFromFile:@
loadCompositionFromFileSelector :: Selector
loadCompositionFromFileSelector = mkSelector "loadCompositionFromFile:"

-- | @Selector@ for @loadComposition:@
loadCompositionSelector :: Selector
loadCompositionSelector = mkSelector "loadComposition:"

-- | @Selector@ for @loadedComposition@
loadedCompositionSelector :: Selector
loadedCompositionSelector = mkSelector "loadedComposition"

-- | @Selector@ for @unloadComposition@
unloadCompositionSelector :: Selector
unloadCompositionSelector = mkSelector "unloadComposition"

-- | @Selector@ for @setAutostartsRendering:@
setAutostartsRenderingSelector :: Selector
setAutostartsRenderingSelector = mkSelector "setAutostartsRendering:"

-- | @Selector@ for @autostartsRendering@
autostartsRenderingSelector :: Selector
autostartsRenderingSelector = mkSelector "autostartsRendering"

-- | @Selector@ for @setEraseColor:@
setEraseColorSelector :: Selector
setEraseColorSelector = mkSelector "setEraseColor:"

-- | @Selector@ for @eraseColor@
eraseColorSelector :: Selector
eraseColorSelector = mkSelector "eraseColor"

-- | @Selector@ for @setEventForwardingMask:@
setEventForwardingMaskSelector :: Selector
setEventForwardingMaskSelector = mkSelector "setEventForwardingMask:"

-- | @Selector@ for @eventForwardingMask@
eventForwardingMaskSelector :: Selector
eventForwardingMaskSelector = mkSelector "eventForwardingMask"

-- | @Selector@ for @setMaxRenderingFrameRate:@
setMaxRenderingFrameRateSelector :: Selector
setMaxRenderingFrameRateSelector = mkSelector "setMaxRenderingFrameRate:"

-- | @Selector@ for @maxRenderingFrameRate@
maxRenderingFrameRateSelector :: Selector
maxRenderingFrameRateSelector = mkSelector "maxRenderingFrameRate"

-- | @Selector@ for @erase@
eraseSelector :: Selector
eraseSelector = mkSelector "erase"

-- | @Selector@ for @startRendering@
startRenderingSelector :: Selector
startRenderingSelector = mkSelector "startRendering"

-- | @Selector@ for @renderAtTime:arguments:@
renderAtTime_argumentsSelector :: Selector
renderAtTime_argumentsSelector = mkSelector "renderAtTime:arguments:"

-- | @Selector@ for @pauseRendering@
pauseRenderingSelector :: Selector
pauseRenderingSelector = mkSelector "pauseRendering"

-- | @Selector@ for @isPausedRendering@
isPausedRenderingSelector :: Selector
isPausedRenderingSelector = mkSelector "isPausedRendering"

-- | @Selector@ for @resumeRendering@
resumeRenderingSelector :: Selector
resumeRenderingSelector = mkSelector "resumeRendering"

-- | @Selector@ for @stopRendering@
stopRenderingSelector :: Selector
stopRenderingSelector = mkSelector "stopRendering"

-- | @Selector@ for @isRendering@
isRenderingSelector :: Selector
isRenderingSelector = mkSelector "isRendering"

-- | @Selector@ for @snapshotImage@
snapshotImageSelector :: Selector
snapshotImageSelector = mkSelector "snapshotImage"

-- | @Selector@ for @createSnapshotImageOfType:@
createSnapshotImageOfTypeSelector :: Selector
createSnapshotImageOfTypeSelector = mkSelector "createSnapshotImageOfType:"

-- | @Selector@ for @openGLContext@
openGLContextSelector :: Selector
openGLContextSelector = mkSelector "openGLContext"

-- | @Selector@ for @openGLPixelFormat@
openGLPixelFormatSelector :: Selector
openGLPixelFormatSelector = mkSelector "openGLPixelFormat"

-- | @Selector@ for @start:@
startSelector :: Selector
startSelector = mkSelector "start:"

-- | @Selector@ for @stop:@
stopSelector :: Selector
stopSelector = mkSelector "stop:"

-- | @Selector@ for @play:@
playSelector :: Selector
playSelector = mkSelector "play:"

