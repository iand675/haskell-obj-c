{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCRenderer@.
module ObjC.Quartz.QCRenderer
  ( QCRenderer
  , IsQCRenderer(..)
  , initWithComposition_colorSpace
  , initWithCGLContext_pixelFormat_colorSpace_composition
  , initOffScreenWithSize_colorSpace_composition
  , initWithOpenGLContext_pixelFormat_file
  , renderAtTime_arguments
  , renderingTimeForTime_arguments
  , composition
  , snapshotImage
  , createSnapshotImageOfType
  , initWithComposition_colorSpaceSelector
  , initWithCGLContext_pixelFormat_colorSpace_compositionSelector
  , initOffScreenWithSize_colorSpace_compositionSelector
  , initWithOpenGLContext_pixelFormat_fileSelector
  , renderAtTime_argumentsSelector
  , renderingTimeForTime_argumentsSelector
  , compositionSelector
  , snapshotImageSelector
  , createSnapshotImageOfTypeSelector


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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithComposition:colorSpace:@
initWithComposition_colorSpace :: (IsQCRenderer qcRenderer, IsQCComposition composition) => qcRenderer -> composition -> Ptr () -> IO RawId
initWithComposition_colorSpace qcRenderer  composition colorSpace =
withObjCPtr composition $ \raw_composition ->
    fmap (RawId . castPtr) $ sendMsg qcRenderer (mkSelector "initWithComposition:colorSpace:") (retPtr retVoid) [argPtr (castPtr raw_composition :: Ptr ()), argPtr colorSpace]

-- | @- initWithCGLContext:pixelFormat:colorSpace:composition:@
initWithCGLContext_pixelFormat_colorSpace_composition :: (IsQCRenderer qcRenderer, IsQCComposition composition) => qcRenderer -> Ptr () -> Ptr () -> Ptr () -> composition -> IO RawId
initWithCGLContext_pixelFormat_colorSpace_composition qcRenderer  context format colorSpace composition =
withObjCPtr composition $ \raw_composition ->
    fmap (RawId . castPtr) $ sendMsg qcRenderer (mkSelector "initWithCGLContext:pixelFormat:colorSpace:composition:") (retPtr retVoid) [argPtr context, argPtr format, argPtr colorSpace, argPtr (castPtr raw_composition :: Ptr ())]

-- | @- initOffScreenWithSize:colorSpace:composition:@
initOffScreenWithSize_colorSpace_composition :: (IsQCRenderer qcRenderer, IsQCComposition composition) => qcRenderer -> NSSize -> Ptr () -> composition -> IO RawId
initOffScreenWithSize_colorSpace_composition qcRenderer  size colorSpace composition =
withObjCPtr composition $ \raw_composition ->
    fmap (RawId . castPtr) $ sendMsg qcRenderer (mkSelector "initOffScreenWithSize:colorSpace:composition:") (retPtr retVoid) [argNSSize size, argPtr colorSpace, argPtr (castPtr raw_composition :: Ptr ())]

-- | @- initWithOpenGLContext:pixelFormat:file:@
initWithOpenGLContext_pixelFormat_file :: (IsQCRenderer qcRenderer, IsNSOpenGLContext context, IsNSOpenGLPixelFormat format, IsNSString path) => qcRenderer -> context -> format -> path -> IO RawId
initWithOpenGLContext_pixelFormat_file qcRenderer  context format path =
withObjCPtr context $ \raw_context ->
  withObjCPtr format $ \raw_format ->
    withObjCPtr path $ \raw_path ->
        fmap (RawId . castPtr) $ sendMsg qcRenderer (mkSelector "initWithOpenGLContext:pixelFormat:file:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())]

-- | @- renderAtTime:arguments:@
renderAtTime_arguments :: (IsQCRenderer qcRenderer, IsNSDictionary arguments) => qcRenderer -> CDouble -> arguments -> IO Bool
renderAtTime_arguments qcRenderer  time arguments =
withObjCPtr arguments $ \raw_arguments ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg qcRenderer (mkSelector "renderAtTime:arguments:") retCULong [argCDouble (fromIntegral time), argPtr (castPtr raw_arguments :: Ptr ())]

-- | @- renderingTimeForTime:arguments:@
renderingTimeForTime_arguments :: (IsQCRenderer qcRenderer, IsNSDictionary arguments) => qcRenderer -> CDouble -> arguments -> IO CDouble
renderingTimeForTime_arguments qcRenderer  time arguments =
withObjCPtr arguments $ \raw_arguments ->
    sendMsg qcRenderer (mkSelector "renderingTimeForTime:arguments:") retCDouble [argCDouble (fromIntegral time), argPtr (castPtr raw_arguments :: Ptr ())]

-- | @- composition@
composition :: IsQCRenderer qcRenderer => qcRenderer -> IO (Id QCComposition)
composition qcRenderer  =
  sendMsg qcRenderer (mkSelector "composition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- snapshotImage@
snapshotImage :: IsQCRenderer qcRenderer => qcRenderer -> IO (Id NSImage)
snapshotImage qcRenderer  =
  sendMsg qcRenderer (mkSelector "snapshotImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- createSnapshotImageOfType:@
createSnapshotImageOfType :: (IsQCRenderer qcRenderer, IsNSString type_) => qcRenderer -> type_ -> IO RawId
createSnapshotImageOfType qcRenderer  type_ =
withObjCPtr type_ $ \raw_type_ ->
    fmap (RawId . castPtr) $ sendMsg qcRenderer (mkSelector "createSnapshotImageOfType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithComposition:colorSpace:@
initWithComposition_colorSpaceSelector :: Selector
initWithComposition_colorSpaceSelector = mkSelector "initWithComposition:colorSpace:"

-- | @Selector@ for @initWithCGLContext:pixelFormat:colorSpace:composition:@
initWithCGLContext_pixelFormat_colorSpace_compositionSelector :: Selector
initWithCGLContext_pixelFormat_colorSpace_compositionSelector = mkSelector "initWithCGLContext:pixelFormat:colorSpace:composition:"

-- | @Selector@ for @initOffScreenWithSize:colorSpace:composition:@
initOffScreenWithSize_colorSpace_compositionSelector :: Selector
initOffScreenWithSize_colorSpace_compositionSelector = mkSelector "initOffScreenWithSize:colorSpace:composition:"

-- | @Selector@ for @initWithOpenGLContext:pixelFormat:file:@
initWithOpenGLContext_pixelFormat_fileSelector :: Selector
initWithOpenGLContext_pixelFormat_fileSelector = mkSelector "initWithOpenGLContext:pixelFormat:file:"

-- | @Selector@ for @renderAtTime:arguments:@
renderAtTime_argumentsSelector :: Selector
renderAtTime_argumentsSelector = mkSelector "renderAtTime:arguments:"

-- | @Selector@ for @renderingTimeForTime:arguments:@
renderingTimeForTime_argumentsSelector :: Selector
renderingTimeForTime_argumentsSelector = mkSelector "renderingTimeForTime:arguments:"

-- | @Selector@ for @composition@
compositionSelector :: Selector
compositionSelector = mkSelector "composition"

-- | @Selector@ for @snapshotImage@
snapshotImageSelector :: Selector
snapshotImageSelector = mkSelector "snapshotImage"

-- | @Selector@ for @createSnapshotImageOfType:@
createSnapshotImageOfTypeSelector :: Selector
createSnapshotImageOfTypeSelector = mkSelector "createSnapshotImageOfType:"

